#' Extract a PEST++ posterior parameter ensemble as runnable parameter sets
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A finished `pestpp-ies` run leaves a **posterior parameter ensemble** -
#' one parameter vector per realisation, the product of an iterative ensemble
#' smoother. This turns that ensemble into a list of `param` dataframes, each
#' one directly runnable by \code{\link{run_aeme_ensemble}} (via its
#' `param_sets` argument) or \code{\link{run_aeme_param}}.
#'
#' Every set is *complete*: parameters that were held fixed during the run -
#' frozen earlier stages of a staged calibration, or anything with
#' `value == min == max` - are carried through unchanged from the template,
#' because the solver's ensemble only contains the adjustable parameters.
#'
#' `iteration = 0` returns the **prior** ensemble instead, for a
#' prior-to-posterior predictive comparison.
#'
#' @param calib A completed calibration: the object from
#'   \code{\link{read_calib}}, a run-directory path, or a
#'   \code{\link{create_pest_control}} object.
#' @param param dataframe; optional parameter template used instead of
#'   `calib$parameter_metadata` (needed when `calib` is a bare path). Must
#'   carry `model`, `file`, `name`, `group`, `index`, `value`, `min`, `max`.
#' @param iteration Integer or `NULL`. Which ensemble to read; `NULL` (the
#'   default) is the last (posterior), `0` is the prior. Passed to
#'   \code{\link{read_pest_ensemble}}.
#' @param include_base Logical. Keep the `base` realisation (the initial
#'   parameter values carried through the run by `ies_include_base`)?
#'   Default `FALSE`.
#' @param n_max Integer or `NULL`. Keep only the first `n_max` realisations,
#'   for a quick look at a large ensemble. Default `NULL` (all).
#'
#' @importFrom stats setNames
#'
#' @return An object of class `aeme_param_sets`: a named list (names are the
#'   realisation ids), each element a long `param` dataframe with columns
#'   `model`, `file`, `name`, `group`, `index`, `value`, `min`, `max`,
#'   `name_full`. `as.data.frame()` stacks it into one long frame with an
#'   `ensemble` id column - the other form \code{\link{run_aeme_ensemble}}
#'   accepts.
#' @seealso \code{\link{run_aeme_ensemble}}, \code{\link{read_pest_ensemble}},
#'   \code{\link{pest_param_summary}}
#' @export
#'
#' @examples
#' \dontrun{
#' calib <- read_calib(ctrl = cal_ctrl, sim_id = cal_id)
#'
#' post <- pest_posterior_params(calib)          # runnable posterior sets
#' post
#'
#' aeme_ens <- run_aeme_ensemble(aeme = aeme, model = "glm_aed", path = path,
#'                               param_sets = post, parallel = TRUE)
#' plot_ensemble(aeme_ens, model = "glm_aed", var_sim = "HYD_temp")
#'
#' prior <- pest_posterior_params(calib, iteration = 0)   # prior band
#' }
pest_posterior_params <- function(calib, param = NULL, iteration = NULL,
                                  include_base = FALSE, n_max = NULL) {

  ctrl <- .pest_locate(calib)

  template <- param
  if (is.null(template) && is.list(calib) &&
      !is.null(calib[["parameter_metadata"]])) {
    template <- calib[["parameter_metadata"]]
  }
  if (is.null(template)) {
    cli::cli_abort(c(
      "No parameter template available.",
      "i" = "Pass a {.arg calib} from {.fn read_calib}, or a {.arg param}
             dataframe alongside a directory / control."
    ))
  }

  need <- c("model", "file", "name", "group", "index", "value", "min", "max")
  miss <- setdiff(need, names(template))
  if (length(miss) > 0) {
    cli::cli_abort("{.arg param} is missing column{?s}: {.val {miss}}.")
  }
  template <- as.data.frame(template)
  template$sim_id <- NULL
  if (!"name_full" %in% names(template)) {
    template$name_full <- encode_param(template$group, template$name,
                                       template$index)
  }
  template <- template[, c(need, "name_full")]

  ens <- read_pest_ensemble(ctrl, iteration = iteration, type = "par")
  if (!include_base) ens <- ens[!ens$is_base, , drop = FALSE]
  if (nrow(ens) == 0) {
    cli::cli_abort("The {.field par} ensemble has no realisations to return.")
  }

  reals <- unique(ens$realisation)
  if (!is.null(n_max) && length(reals) > n_max) {
    reals <- utils::head(reals, n_max)
    AEME::cli_inform_safe(c("i" = paste0(
      "Keeping the first {.val ", n_max, "} of {.val ", length(unique(
        ens$realisation)), "} realisations.")))
  }

  if (!any(template$name_full %in% ens$name_full)) {
    cli::cli_abort(c(
      "None of the template parameters appear in the posterior ensemble.",
      "i" = "The {.arg param} / {.arg calib} pair does not match this run."
    ))
  }

  sets <- stats::setNames(lapply(reals, function(r) {
    ev <- ens[ens$realisation == r, ]
    out <- template
    hit <- match(out$name_full, ev$name_full)
    out$value[!is.na(hit)] <- ev$value[hit[!is.na(hit)]]
    out
  }), reals)

  class(sets) <- c("aeme_param_sets", "list")
  attr(sets, "iteration") <- unique(ens$iteration)
  sets
}

#' @export
print.aeme_param_sets <- function(x, ...) {
  s1 <- x[[1]]
  n_frozen <- sum(s1$min == s1$value & s1$max == s1$value, na.rm = TRUE)
  cli::cli_h1("PEST++ posterior parameter sets")
  print(data.frame(
    sets = length(x),
    parameters = nrow(s1),
    adjustable = nrow(s1) - n_frozen,
    frozen = n_frozen,
    iteration = paste(attr(x, "iteration"), collapse = ", "),
    ids = paste0(utils::head(names(x), 3), collapse = ", "),
    row.names = "", check.names = FALSE
  ))
  invisible(x)
}

#' @export
as.data.frame.aeme_param_sets <- function(x, ...) {
  dplyr::bind_rows(lapply(x, as.data.frame), .id = "ensemble")
}
