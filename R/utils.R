#' Round to a specified accuracy
#' @param x A numeric vector.
#' @param accuracy A positive number specifying the rounding accuracy.
#' @param f A rounding function, such as `round`, `floor`, or `ceiling`. 
#' Default is `round`.
#' @noRd
round_any <- function(x, accuracy, f = round) f(x / accuracy) * accuracy
