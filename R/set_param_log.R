#' Flag parameters for log transformation in a PEST calibration
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Adds (or overwrites) the logical `log` column that
#' \code{\link{pest_param_table}} and \code{\link{pest_prior_cov}} read to
#' decide which parameters `pestpp-ies` samples in log10 space. A parameter
#' is flagged when it is strictly positive and its feasible range spans at
#' least `ratio` (default one order of magnitude) - the point at which a
#' linear prior spends most of its ensemble in the top of the range.
#'
#' \code{\link{pest_param_table}} independently drops the transform for any
#' parameter whose lower bound is not positive, so a flag set here can never
#' produce an invalid control file. The positivity check is repeated only so
#' the column is meaningful on its own.
#'
#' @param param dataframe; as passed to \code{\link{calib_aeme}}, requiring
#'   at least the `min` and `max` columns.
#' @param ratio Numeric `>= 1`. Minimum `max / min` for a parameter to be
#'   flagged. `Inf` flags nothing. Default `10`.
#' @param overwrite Logical. Replace an existing `log` column outright?
#'   Default `TRUE`. When `FALSE`, rows already `TRUE` are kept and only the
#'   remaining rows are set from the rule.
#'
#' @return `param` with a logical `log` column.
#' @seealso [pest_param_table()], [pest_prior_cov()], [freeze_param()]
#' @export
#'
#' @examples
#' param <- data.frame(
#'   model = "glm_aed", file = "glm3.nml", group = "light",
#'   name = c("Kw", "sed_temp_mean"), index = NA_real_,
#'   value = c(0.5, 8), min = c(0.05, 4), max = c(5, 12)
#' )
#' set_param_log(param)$log   # TRUE  (0.05..5 spans 100x)
#'                            # FALSE (4..12 spans 3x)
set_param_log <- function(param, ratio = 10, overwrite = TRUE) {

  if (!is.data.frame(param)) {
    cli::cli_abort("{.arg param} must be a data frame.")
  }
  miss <- setdiff(c("min", "max"), names(param))
  if (length(miss) > 0) {
    cli::cli_abort("{.arg param} is missing column{?s}: {.field {miss}}.")
  }
  if (!is.numeric(ratio) || length(ratio) != 1L || is.na(ratio) || ratio < 1) {
    cli::cli_abort("{.arg ratio} must be a single number `>= 1` (or {.code Inf}).")
  }

  lg <- is.finite(ratio) & is.finite(param$min) & is.finite(param$max) &
    param$min > 0 & (param$max / param$min) >= ratio
  lg[is.na(lg)] <- FALSE

  if (!isTRUE(overwrite) && "log" %in% names(param)) {
    kept <- !is.na(param$log) & param$log
    lg <- lg | kept
  }

  param$log <- lg
  param
}
