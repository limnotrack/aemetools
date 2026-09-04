#' Apply a calibration or validation period to an Aeme object
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Sets the simulation window from one row of a
#' \code{\link{get_calib_periods}} result, so the split feeds straight into
#' \code{\link{calib_aeme}} - including the PEST++ engines, which take their
#' observation window from `AEME::time(aeme)` by way of
#' \code{\link{pest_obs_table}}.
#'
#' Spin-up is carried over from the object unless `spin_up` is given. Both
#' periods start on an observation date, so the validation run needs spin-up
#' just as much as the calibration run does: without it the first residual
#' is measured against a model that has not yet moved off its initial
#' condition.
#'
#' @param aeme Aeme object.
#' @param periods An `aeme_calib_periods` object from
#'   \code{\link{get_calib_periods}}.
#' @param period Character or `NULL`. Which period to apply - `"calib"` or
#'   `"valid"` from a split, or `"all"` from `split = FALSE`. `NULL`
#'   (default) takes the object's first period, so a no-split object needs
#'   no argument at all.
#' @param spin_up Numeric or `NULL`. Days of spin-up to set. `NULL`
#'   (default) keeps whatever the object already carries.
#'
#' @return `aeme`, with its simulation window set to the chosen period.
#' @seealso \code{\link{get_calib_periods}}, \code{\link{validate_aeme}}
#' @export
#'
#' @examples
#' \dontrun{
#' p <- get_calib_periods(aeme, vars_sim = "HYD_temp")
#' aeme_cal <- set_calib_period(aeme, p, "calib")
#' sim_id <- calib_aeme(aeme = aeme_cal, param = param, model = "glm_aed",
#'                      vars_sim = "HYD_temp",
#'                      ctrl = create_pest_control(exe = "pestpp-ies"))
#' }
set_calib_period <- function(aeme, periods, period = NULL, spin_up = NULL) {

  .va_check_periods(periods)

  avail <- periods$periods$period
  # Defaulting to the first period means a `split = FALSE` object, which has
  # only "all", needs no argument - the same call works either way.
  period <- period %||% avail[1]

  row <- periods$periods[periods$periods$period == period, , drop = FALSE]
  if (nrow(row) != 1) {
    cli::cli_abort(c(
      "{.arg period} must be one of {.val {avail}}, not {.val {period}}.",
      if (identical(avail, "all")) {
        c("i" = "This is a {.code split = FALSE} result - it has no
                 separate validation period.")
      }
    ))
  }

  spin_up <- spin_up %||% AEME::time(aeme)$spin_up
  AEME::set_time(aeme, start = row$start, stop = row$stop, spin_up = spin_up)
}

#' Score a calibrated parameter set over the calibration and validation
#' periods
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The split-sample half of the PEST workflow. Runs `model` once per period
#' from \code{\link{get_calib_periods}}, both times with the *same*
#' (calibrated) parameters, and reports the fit of each variable in each
#' period.
#'
#' The comparison is the point: a parameter set that fits the calibration
#' period well and the held-out period much worse has been fitted to noise
#' or to conditions that do not recur, which is exactly what a `pestpp-ies`
#' run driven to a low phi can produce. `degradation` reports that gap.
#'
#' Fit values come from `FUN_list`, so they carry the same orientation as in
#' calibration - objectives are written so that *lower is better* (see
#' \code{\link{nse_loss}} and friends, which return the negated statistic,
#' and \code{\link{mae}}/\code{\link{rmse}}/\code{\link{pbias}}, which are
#' already `0`-is-best). A positive `degradation` therefore always means the
#' validation period fits worse.
#'
#' @inheritParams run_and_fit
#' @param periods An `aeme_calib_periods` object from
#'   \code{\link{get_calib_periods}}.
#' @param param dataframe; the calibrated parameters, as returned by
#'   \code{\link{update_param}} or \code{\link{get_best_params}}.
#' @param spin_up Numeric or `NULL`. Passed to
#'   \code{\link{set_calib_period}}.
#'
#' @return A list of class `aeme_validation`:
#'   \describe{
#'     \item{`fit`}{dataframe with one row per variable per period: `period`,
#'       `var_aeme`, `n_obs` and `fit`.}
#'     \item{`degradation`}{dataframe with one row per variable: `calib`,
#'       `valid` and `degradation` (`valid - calib`; positive means the
#'       held-out period fits worse).}
#'     \item{`comparison`}{the modelled/observed rows behind the fits, with
#'       a `period` column.}
#'   }
#' @seealso \code{\link{get_calib_periods}}, \code{\link{set_calib_period}},
#'   \code{\link{calib_aeme}}
#' @export
#'
#' @examples
#' \dontrun{
#' p <- get_calib_periods(aeme, vars_sim = "HYD_temp")
#' sim_id <- calib_aeme(aeme = set_calib_period(aeme, p, "calib"),
#'                      param = param, model = "glm_aed",
#'                      vars_sim = "HYD_temp",
#'                      ctrl = create_pest_control(exe = "pestpp-ies"))
#' calib <- read_pest_results(ctrl)
#' best <- update_param(calib = calib, param = param)
#'
#' v <- validate_aeme(aeme = aeme, param = best, periods = p,
#'                    model = "glm_aed", vars_sim = "HYD_temp",
#'                    FUN_list = list(HYD_temp = kge_loss))
#' v$degradation
#' }
validate_aeme <- function(aeme, param, periods, model, vars_sim, path,
                          FUN_list, weights, model_controls = NULL,
                          spin_up = NULL, na_value = 999) {

  .va_check_periods(periods)
  if (!"valid" %in% periods$periods$period) {
    cli::cli_abort(c(
      "{.arg periods} has no validation period to score against.",
      "i" = "A {.code split = FALSE} result conditions on the whole record
             by design; check for over-fitting with the posterior ensemble
             ({.fn pest_prior_data_conflict}, {.fn read_pest_phi_group})
             rather than a held-out period.",
      "i" = "For a fit over a single period, call {.fn run_and_fit} directly."
    ))
  }
  if (length(model) != 1) {
    cli::cli_abort("{.arg model} must name a single model, not
                   {.val {length(model)}}.")
  }
  if (missing(path)) path <- AEME::get_aeme_path(aeme = aeme)
  if (missing(FUN_list) || is.null(FUN_list)) {
    cli::cli_abort(c(
      "{.arg FUN_list} is required.",
      "i" = "Pass the same fitness functions the calibration used, so the
             two periods are scored on the same footing."
    ))
  }
  missing_fun <- setdiff(vars_sim, names(FUN_list))
  if (length(missing_fun) > 0) {
    cli::cli_abort("No {.arg FUN_list} entry for {.val {missing_fun}}.")
  }
  if (missing(weights) || is.null(weights)) {
    weights <- set_weights(vars_sim = vars_sim)
  }

  include_wlev <- "LKE_lvlwtr" %in% vars_sim
  periods_run <- periods$periods$period

  cmp <- do.call(rbind, lapply(periods_run, function(p) {
    a <- set_calib_period(aeme, periods, period = p, spin_up = spin_up)
    AEME::cli_inform_safe(c("i" = paste0(
      "Running {.val ", model, "} over the {.val ", p, "} period (",
      format(periods$periods$start[periods$periods$period == p]), " to ",
      format(periods$periods$stop[periods$periods$period == p]), ")")))

    df <- run_and_fit(aeme = a, param = param, model = model, path = path,
                      vars_sim = vars_sim, FUN_list = FUN_list,
                      weights = weights, model_controls = model_controls,
                      include_wlev = include_wlev, na_value = na_value,
                      return_df = TRUE)
    # A failed run returns the na_value list rather than a dataframe, the
    # same contract .pest_run_residual() relies on.
    if (!is.data.frame(df)) {
      cli::cli_abort(c(
        "The {.val {p}} run of {.val {model}} did not produce a comparison.",
        "i" = "Check that the model runs over
               {.val {format(periods$periods$start[periods$periods$period == p])}}
               to {.val {format(periods$periods$stop[periods$periods$period == p])}}."
      ))
    }
    df$period <- p
    df
  }))

  fit <- do.call(rbind, lapply(periods_run, function(p) {
    do.call(rbind, lapply(vars_sim, function(v) {
      sub <- cmp[cmp$period == p & cmp$var_aeme == v, , drop = FALSE]
      sub <- sub[!is.na(sub$obs) & !is.na(sub$model), , drop = FALSE]
      data.frame(
        period = p, var_aeme = v, n_obs = nrow(sub),
        # An empty period cannot be scored; NA rather than a fitness
        # function's answer to zero rows, which differs by function.
        fit = if (nrow(sub) > 0) as.numeric(FUN_list[[v]](sub)) else NA_real_,
        stringsAsFactors = FALSE)
    }))
  }))

  out <- list(fit = fit, degradation = .va_degradation(fit), comparison = cmp)
  class(out) <- c("aeme_validation", "list")

  thin <- fit[fit$n_obs == 0, , drop = FALSE]
  if (nrow(thin) > 0) {
    AEME::cli_safe(
      paste0("No comparable observations for ",
             paste(sprintf("%s (%s)", thin$var_aeme, thin$period),
                   collapse = ", "), "."),
      FUN = cli::cli_alert_warning)
  }
  out
}

#' @export
print.aeme_validation <- function(x, ...) {
  cli::cli_h1("Split-sample validation")
  print(x$degradation, row.names = FALSE)
  cli::cli_text("{.emph degradation} = valid - calib; positive means the
                held-out period fits worse.")
  invisible(x)
}

# Internal helpers -----------------------------------------------------------

#' @noRd
.va_check_periods <- function(periods) {
  if (!inherits(periods, "aeme_calib_periods")) {
    cli::cli_abort(c(
      "{.arg periods} must come from {.fn get_calib_periods}.",
      "x" = "Got {.cls {class(periods)[1]}}."
    ))
  }
  invisible(TRUE)
}

#' Calibration/validation fit per variable, and the gap between them.
#' @noRd
.va_degradation <- function(fit) {
  vars <- unique(fit$var_aeme)
  do.call(rbind, lapply(vars, function(v) {
    g <- function(p) {
      z <- fit$fit[fit$var_aeme == v & fit$period == p]
      if (length(z) == 1) z else NA_real_
    }
    cal <- g("calib")
    val <- g("valid")
    data.frame(var_aeme = v, calib = cal, valid = val,
               degradation = val - cal, stringsAsFactors = FALSE)
  }))
}
