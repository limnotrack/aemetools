#' Round candidate parameter values to the precision they are saved with
#'
#' The built-in calibration engines write each generation's parameters (and
#' fit) to the results file rounded to six significant digits. Rounding the
#' candidates themselves before they are scored means the values that were
#' run are exactly the values that were saved, so re-running a saved parameter
#' set reproduces the fit recorded for it. Without this the model can be
#' sensitive enough that the rounding alone changes the fit by a fraction of a
#' percent.
#'
#' Values are kept inside `[min, max]`, since rounding a value that sits right
#' on a bound can nudge it just outside.
#'
#' @param df data frame of candidates, one row per member and one column per
#'   parameter (named by `name_full`). Other columns are left alone.
#' @param param parameter data frame with `name_full`, `min` and `max`.
#' @param digits integer; significant digits. Default `6`, matching what the
#'   results writer saves.
#' @return `df` with its parameter columns rounded.
#' @noRd
round_candidates <- function(df, param, digits = 6) {
  for (p in intersect(names(df), param$name_full)) {
    v <- signif(df[[p]], digits)
    lo <- param$min[param$name_full == p][1]
    hi <- param$max[param$name_full == p][1]
    if (is.finite(lo)) v <- pmax(v, lo)
    if (is.finite(hi)) v <- pmin(v, hi)
    df[[p]] <- v
  }
  df
}
