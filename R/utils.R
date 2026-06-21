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