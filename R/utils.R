#' Round to a specified accuracy
#' @param x A numeric vector.
#' @param accuracy A positive number specifying the rounding accuracy.
#' @param f A rounding function, such as `round`, `floor`, or `ceiling`. 
#' Default is `round`.
#' @noRd
round_any <- function(x, accuracy, f = round) f(x / accuracy) * accuracy

#' Null coalescing operator
#' Returns the left-hand side if it is not NULL, otherwise returns the right-hand side.
#' @param x The value to check for NULL.
#' @param y The value to return if x is NULL.
#' @noRd
`%||%` <- function(x, y) if (!is.null(x)) x else y

#' Ensure a lake observations data frame has a numeric `depth` column
#'
#' AEME (>= 0.4.0) stores lake observations with a single required `depth`
#' column (nominal sampling depth, metres positive-down from the surface).
#' Older Aeme objects and CSV files instead carry the `depth_from` / `depth_to`
#' column pair. This helper accepts either layout and always returns a data
#' frame with a numeric `depth` column, so downstream code does not need to
#' care which schema the observations came from:
#' \itemize{
#'   \item `depth` present - returned as-is (coerced to numeric).
#'   \item `depth_from` and `depth_to` present - `depth` is their midpoint,
#'     the same collapse AEME applies internally.
#'   \item only `depth_from` (or only `depth_mid`) present - used directly.
#' }
#'
#' @param lake data frame of lake observations, or `NULL`.
#' @return `lake` with a numeric `depth` column, or `lake` unchanged when it is
#'   `NULL` / not a data frame / has no depth-like column.
#' @noRd
normalise_lake_obs <- function(lake) {
  if (is.null(lake) || !is.data.frame(lake)) return(lake)
  nms <- names(lake)
  if ("depth" %in% nms) {
    lake$depth <- as.numeric(lake$depth)
  } else if (all(c("depth_from", "depth_to") %in% nms)) {
    lake$depth <- (as.numeric(lake$depth_from) + as.numeric(lake$depth_to)) / 2
  } else if ("depth_from" %in% nms) {
    lake$depth <- as.numeric(lake$depth_from)
  } else if ("depth_mid" %in% nms) {
    lake$depth <- as.numeric(lake$depth_mid)
  }
  lake
}
