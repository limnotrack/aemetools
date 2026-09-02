#' Suggest calibration and validation periods from observation availability
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Picks a single chronological split of an `Aeme` object's observation
#' record into a calibration period (earlier) and a validation period
#' (later), choosing the split date that comes closest to putting `frac` of
#' the observations in the calibration period while leaving every variable
#' in `vars_sim` represented in both.
#'
#' The candidate split dates are the observation dates themselves, so each
#' period begins and ends on a date that actually carries data - no leading
#' or trailing stretch of simulation with nothing to compare against.
#'
#' Sparse years at either end are dropped from the *period*, not from the
#' data: a long thin tail costs model runtime out of all proportion to what
#' it constrains. On a record sampled eight times a year until the early
#' 1990s and hundreds of times a year since, starting the simulation at the
#' first observation buys five extra model-years for a few dozen readings.
#' `min_density` sets where that trade stops being worth it. Only the ends
#' are trimmed, so a lean year in the middle of a dense record is kept and
#' each period stays contiguous.
#'
#' This reports a split rather than applying it. Pass the result to
#' \code{\link{set_calib_period}} to build the two runs, keeping spin-up on
#' the validation run - its period starts on an observation date, so without
#' spin-up the first comparison would be made against a model that has not
#' yet adjusted from its initial condition.
#'
#' # Conditioning on the whole record
#'
#' `split = FALSE` returns a single period spanning every observation,
#' instead of holding any back. Withholding data withholds information that
#' would otherwise have reduced predictive uncertainty, and on a sparse
#' record - a few years of monthly profiles - a 30% holdout is too small to
#' conclude anything from, while the calibration it degrades is real. The
#' returned object has the same shape, so \code{\link{set_calib_period}} and
#' the rest of the workflow are unchanged; only its single period is named
#' `"all"` and `split_date` is `NULL`.
#'
#' This is not a licence to skip checking for over-fitting - it moves that
#' check from a held-out period onto the posterior ensemble. Pair it with an
#' ensemble method (`create_pest_control(exe = "pestpp-ies")`) and read the
#' spread, together with \code{\link{pest_prior_data_conflict}} and
#' \code{\link{read_pest_phi_group}}. Conditioning on everything and then
#' reporting a single best parameter set removes the only guard and puts
#' nothing in its place.
#'
#' @param aeme Aeme object.
#' @param vars_sim Character. Variables (`var_aeme` values) that must be
#'   represented in both periods, e.g. `c("HYD_temp", "CHM_oxy")`. Default
#'   `NULL` uses every variable present in the observations.
#' @param frac Numeric between 0 and 1. Target proportion of the record to
#'   place in the calibration period. Default `0.7`. Ignored when
#'   `split = FALSE`.
#' @param min_obs Integer. Minimum observations of *each* variable in
#'   *each* period for a split to be considered. Default `1`. Ignored when
#'   `split = FALSE`.
#' @param weight_by Character. What `frac` is a proportion of: `"obs"`
#'   (individual observations, so a deep profile counts more than a surface
#'   sample) or `"dates"` (distinct sampling days, so every visit counts
#'   once). Default `"obs"`. Ignored when `split = FALSE`.
#' @param split Logical. `TRUE` (default) splits the record into
#'   calibration and validation periods. `FALSE` returns the whole record as
#'   one period - see *Conditioning on the whole record*.
#' @param min_density Numeric. Drop leading and trailing calendar years
#'   holding fewer than this fraction of the median year's measure, so the
#'   simulation does not run over a sparse tail. Default `0.25`; `0` keeps
#'   the record whole. Ignored on records spanning fewer than `min_years`
#'   complete years, where a median across years means little.
#' @param density_by Character. What `min_density` measures a year by:
#'   `"profiles"` (default) counts casts carrying at least `min_depths`
#'   distinct depths, `"obs"` counts individual readings, `"dates"` counts
#'   sampling days. Profiles are the default because they are what a
#'   depth-resolved calibration is actually constrained by - a year of
#'   surface-and-bottom pairs scores near zero on `"profiles"` while looking
#'   respectable on `"dates"`.
#' @param min_depths Integer. Distinct depths a cast needs before it counts
#'   as a profile under `density_by = "profiles"`. Default `3`. A variable
#'   with no depths at all - water level - is a scalar series, so each of
#'   its dates always counts.
#' @param min_years Integer. Fewest calendar years a record must span before
#'   `min_density` is applied. Default `4`.
#'
#' @importFrom stats setNames
#'
#' @return An object of class `aeme_calib_periods`: a list with
#'   \describe{
#'     \item{`periods`}{dataframe, one row per period - `calib` and `valid`,
#'       or a single `all` when `split = FALSE` - with `start`, `stop`,
#'       `n_days`, `n_obs`, `n_dates` and `n_months` (distinct calendar
#'       months, i.e. seasonal coverage).}
#'     \item{`coverage`}{dataframe, one row per variable per period, with
#'       `n_obs`, `n_dates`, `first` and `last`.}
#'     \item{`split_date`}{Date on which the validation period starts, or
#'       `NULL` when `split = FALSE`.}
#'   }
#' @seealso \code{\link{set_calib_period}}, \code{\link{validate_aeme}},
#'   \code{\link{calib_aeme}}
#' @export
#'
#' @examples
#' \dontrun{
#' aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
#' p <- get_calib_periods(aeme, vars_sim = "HYD_temp")
#' p
#'
#' calib <- set_calib_period(aeme, p, "calib")
#'
#' # Condition on everything instead, and lean on the posterior ensemble.
#' p_all <- get_calib_periods(aeme, vars_sim = "HYD_temp", split = FALSE)
#' aeme_all <- set_calib_period(aeme, p_all)
#' }
get_calib_periods <- function(aeme, vars_sim = NULL, frac = 0.7,
                              min_obs = 1L, weight_by = c("obs", "dates"),
                              split = TRUE, min_density = 0.25,
                              density_by = c("profiles", "obs", "dates"),
                              min_depths = 3L, min_years = 4L) {

  # Recorded before anything reassigns these: arg_match() below writes to
  # `weight_by`, after which missing() would report it as supplied.
  supplied <- c(if (!missing(frac)) "frac", if (!missing(min_obs)) "min_obs",
                if (!missing(weight_by)) "weight_by")

  weight_by <- rlang::arg_match(weight_by)
  density_by <- rlang::arg_match(density_by)
  if (!is.logical(split) || length(split) != 1 || is.na(split)) {
    cli::cli_abort("{.arg split} must be {.val TRUE} or {.val FALSE}.")
  }
  if (!is.numeric(min_density) || length(min_density) != 1 ||
      is.na(min_density) || min_density < 0) {
    cli::cli_abort("{.arg min_density} must be a single non-negative number.")
  }

  df <- .cp_observations(aeme, vars_sim)
  df <- .cp_trim_sparse(df, min_density = min_density,
                        min_years = as.integer(min_years),
                        density_by = density_by,
                        min_depths = as.integer(min_depths))
  vars <- sort(unique(df$var_aeme))

  if (!split) {
    # The split-search arguments have nothing to act on here. Say so rather
    # than accepting a setting that silently does nothing.
    if (length(supplied) > 0) {
      AEME::cli_safe(
        paste0("{.arg ", paste(supplied, collapse = "}, {.arg "),
               "} {cli::qty(", length(supplied),
               ")}{?is/are} ignored when {.code split = FALSE}."),
        FUN = cli::cli_alert_info)
    }
    return(.cp_single(df, vars))
  }

  if (!is.numeric(frac) || length(frac) != 1 || is.na(frac) ||
      frac <= 0 || frac >= 1) {
    cli::cli_abort("{.arg frac} must be a single number strictly between
                   {.val 0} and {.val 1}.")
  }
  min_obs <- as.integer(min_obs)
  if (is.na(min_obs) || min_obs < 1L) {
    cli::cli_abort("{.arg min_obs} must be a positive integer.")
  }

  udates <- sort(unique(df$Date))
  if (length(udates) < 2) {
    cli::cli_abort(c(
      "Need at least {.val 2} distinct observation dates to split a record.",
      "i" = "Found {.val {length(udates)}}.",
      "i" = "Use {.code split = FALSE} to take the whole record as one period."
    ))
  }

  # Cumulative observations per variable up to and including each date, so
  # every candidate split is a column lookup rather than a re-filter.
  tab <- table(factor(df$var_aeme, levels = vars),
               factor(as.character(df$Date), levels = as.character(udates)))
  cum <- t(apply(tab, 1, cumsum))
  # apply() drops to a vector for a single variable.
  if (is.null(dim(cum))) cum <- matrix(cum, nrow = 1, dimnames = list(vars, NULL))
  tot <- cum[, ncol(cum)]

  # Candidate j: calib = udates[1:j], valid = udates[(j+1):n].
  j <- seq_len(length(udates) - 1L)
  n_cal <- cum[, j, drop = FALSE]
  n_val <- tot - n_cal
  ok <- apply(n_cal >= min_obs & n_val >= min_obs, 2, all)

  if (!any(ok)) {
    thin <- vars[tot < 2 * min_obs]
    cli::cli_abort(c(
      "No split leaves every variable with at least {.val {min_obs}}
       observations in both periods.",
      if (length(thin) > 0) {
        c("x" = "Fewer than {.val {2 * min_obs}} observations in total:
                 {.val {thin}}.")
      },
      "i" = "Lower {.arg min_obs}, or drop the sparse variables from
             {.arg vars_sim}.",
      if (min_density > 0) {
        c("i" = "{.code min_density = {min_density}} trimmed the ends of the
                 record; {.code min_density = 0} keeps them.")
      },
      "i" = "{.code split = FALSE} conditions on the whole record instead."
    ))
  }

  share <- if (weight_by == "obs") {
    colSums(n_cal) / sum(tot)
  } else {
    j / length(udates)
  }
  # Closest to the target share; ties go to the earlier split.
  score <- abs(share - frac)
  score[!ok] <- Inf
  k <- which.min(score)

  cal <- c(udates[1], udates[k])
  val <- c(udates[k + 1L], udates[length(udates)])

  out <- list(
    periods = rbind(.cp_period_row("calib", cal, df),
                    .cp_period_row("valid", val, df)),
    coverage = rbind(.cp_coverage("calib", cal, df, vars),
                     .cp_coverage("valid", val, df, vars)),
    split_date = val[1]
  )
  class(out) <- c("aeme_calib_periods", "list")

  AEME::cli_inform_safe(c("i" = paste0(
    "Split at {.val ", format(val[1]), "}: ",
    round(100 * share[k]), "% of the ",
    if (weight_by == "obs") "observations" else "sampling days",
    " in calibration (target ", round(100 * frac), "%).")))

  out
}

#' @export
print.aeme_calib_periods <- function(x, ...) {
  if (is.null(x$split_date)) {
    cli::cli_h1("Whole observation record (no split)")
  } else {
    cli::cli_h1("Suggested calibration / validation periods")
    cli::cli_text("Split date: {.val {format(x$split_date)}}")
  }
  cat("\n")
  print(x$periods, row.names = FALSE)
  cat("\n")
  print(x$coverage, row.names = FALSE)
  invisible(x)
}

# Internal helpers -----------------------------------------------------------

#' Observations per calendar year, by the chosen measure.
#'
#' `"profiles"` counts casts, not readings: a `(variable, date)` group
#' qualifies once it carries `min_depths` distinct depths. That is what
#' separates a lake sampled properly from one sampled thinly, and raw counts
#' do not - on Lake Rototoa the pre-1993 years hold six to eight visits a
#' year, the same as the 2010s, but every one is a surface-and-bottom pair
#' rather than a twenty-depth cast. Counting rows understates them by a
#' factor of ten; counting dates cannot tell them apart at all; counting
#' profiles scores them zero, which is what they are worth to a
#' depth-resolved calibration.
#'
#' A variable with no depths - water level - is a scalar series rather than
#' a profile, so each of its dates always qualifies.
#' @noRd
.cp_density <- function(df, yr, density_by, min_depths) {

  if (density_by == "obs") return(table(yr))
  if (density_by == "dates") return(table(yr[!duplicated(df$Date)]))

  grp <- paste(df$var_aeme, format(df$Date, "%Y-%m-%d"))
  keep <- !duplicated(grp)
  nd <- vapply(split(df$depth, grp), function(z) {
    if (all(is.na(z))) return(Inf)          # scalar series: always counts
    length(unique(z[!is.na(z)]))
  }, numeric(1))

  ok <- nd[grp[keep]] >= min_depths
  y <- yr[keep][ok]
  if (length(y) == 0) return(table(integer()))
  table(factor(y, levels = sort(unique(yr))))
}

#' Drop sparse leading and trailing calendar years.
#'
#' Ends only: `which()` on the dense years takes the first and last, so a
#' lean year between two dense ones stays in and the period is contiguous.
#' The observations are not filtered on their own account - they fall
#' outside the resulting period, which is the same thing said as a date
#' range.
#' @noRd
.cp_trim_sparse <- function(df, min_density, min_years = 4L,
                            density_by = "profiles", min_depths = 3L) {

  if (min_density <= 0) return(df)

  yr <- as.integer(format(df$Date, "%Y"))
  n <- .cp_density(df, yr, density_by = density_by, min_depths = min_depths)
  # A median across two or three years says nothing about which of them is
  # sparse, so leave short records alone.
  if (length(n) < min_years) return(df)

  thr <- min_density * stats::median(as.numeric(n))
  dense <- which(as.numeric(n) >= thr)
  if (length(dense) == 0) return(df)

  yrs <- as.integer(names(n))
  lo <- yrs[dense[1]]
  hi <- yrs[dense[length(dense)]]
  if (lo == min(yrs) && hi == max(yrs)) return(df)

  out <- df[yr >= lo & yr <= hi, , drop = FALSE]
  AEME::cli_safe(
    paste0("Trimmed the period to {.val ", lo, "}-{.val ", hi,
           "}: the ", sum(yr < lo | yr > hi), " observation",
           if (sum(yr < lo | yr > hi) == 1) "" else "s",
           " in ", length(yrs) - length(lo:hi),
           " sparse year", if (length(yrs) - length(lo:hi) == 1) "" else "s",
           " at the ends would cost more model runtime than they constrain."),
    FUN = cli::cli_alert_info)
  out
}

#' The whole record as a single period, in the same shape a split returns
#' so that every downstream helper works unchanged.
#' @noRd
.cp_single <- function(df, vars) {

  rng <- c(min(df$Date), max(df$Date))
  out <- list(periods = .cp_period_row("all", rng, df),
              coverage = .cp_coverage("all", rng, df, vars),
              split_date = NULL)
  class(out) <- c("aeme_calib_periods", "list")

  AEME::cli_inform_safe(c("i" = paste0(
    "Using the whole record: {.val ", nrow(df), "} observations over {.val ",
    length(unique(df$Date)), "} sampling days, ", format(rng[1]), " to ",
    format(rng[2]), ".")))
  out
}

#' Long observation table (Date, var_aeme, value) for the requested
#' variables, drawing water level from `obs$level` and everything else from
#' `obs$lake`.
#' @noRd
.cp_observations <- function(aeme, vars_sim) {

  obs <- AEME::observations(aeme)
  have_lake <- !is.null(obs$lake) && nrow(obs$lake) > 0
  have_lvl <- !is.null(obs$level) && nrow(obs$level) > 0

  cols <- c("Date", "var_aeme", "value", "depth")
  df <- data.frame(Date = as.Date(character()), var_aeme = character(),
                   value = numeric(), depth = numeric(),
                   stringsAsFactors = FALSE)
  if (have_lake) {
    # Same `depth` pest_obs_table() uses (midpoint of any legacy
    # depth_from / depth_to pair), so "a profile" means the same thing here
    # as it does downstream.
    lk <- normalise_lake_obs(obs$lake)
    df <- rbind(df, lk[, cols, drop = FALSE])
  }
  if (have_lvl) {
    lv <- obs$level
    lv$depth <- NA_real_   # water level is a scalar series, not a profile
    df <- rbind(df, lv[, cols, drop = FALSE])
  }

  df <- df[!is.na(df$value) & !is.na(df$Date), , drop = FALSE]
  if (nrow(df) == 0) cli::cli_abort("No observations found in {.arg aeme}.")

  if (!is.null(vars_sim)) {
    missing_vars <- setdiff(vars_sim, unique(df$var_aeme))
    if (length(missing_vars) > 0) {
      cli::cli_abort(c(
        "No observations for {.val {missing_vars}}.",
        "i" = "Available: {.val {sort(unique(df$var_aeme))}}"
      ))
    }
    df <- df[df$var_aeme %in% vars_sim, , drop = FALSE]
  }
  df
}

#' One row of the periods table.
#' @noRd
.cp_period_row <- function(period, rng, df) {
  sub <- df[df$Date >= rng[1] & df$Date <= rng[2], , drop = FALSE]
  data.frame(
    period = period, start = rng[1], stop = rng[2],
    n_days = as.integer(rng[2] - rng[1]) + 1L,
    n_obs = nrow(sub),
    n_dates = length(unique(sub$Date)),
    n_months = length(unique(format(sub$Date, "%m"))),
    stringsAsFactors = FALSE
  )
}

#' Per-variable coverage within one period.
#' @noRd
.cp_coverage <- function(period, rng, df, vars) {
  sub <- df[df$Date >= rng[1] & df$Date <= rng[2], , drop = FALSE]
  do.call(rbind, lapply(vars, function(v) {
    d <- sub$Date[sub$var_aeme == v]
    data.frame(
      period = period, var_aeme = v, n_obs = length(d),
      n_dates = length(unique(d)),
      first = if (length(d)) min(d) else as.Date(NA),
      last = if (length(d)) max(d) else as.Date(NA),
      stringsAsFactors = FALSE
    )
  }))
}
