#' Set weights for simulated variables
#'
#' @param vars_sim vector; of variables names to be used in the calculation of
#' model fit in `calib_aeme()` or `sa_aeme()`.
#' @param weights numeric vector; of weights for each variable in `vars_sim`.
#' If a single value is provided, it is recycled for all variables. Default is 
#' 1.
#'
#' @returns named numeric vector; of weights for each variable in `vars_sim`.
#' @export
#' 
#' @importFrom cli cli_abort
#'
#' @examples
#' vars_sim <- c("HYD_temp", "HYD_thmcln", "HYD_strat")
#' # Set same weight for all variables
#' weights1 <- set_weights(vars_sim)
#' print(weights1)
#' # Set different weights for each variable
#' weights2 <- set_weights(vars_sim, weights = c(1, 2, 0.5))
#' print(weights2)

set_weights <- function(vars_sim, weights = 1) {
  if (length(weights) == 1) {
    weights <- rep(weights, length(vars_sim))
  }
  if (length(weights) != length(vars_sim)) {
    n_vars <- length(vars_sim)
    cli::cli_abort("Length of weights must be either 1 or {.val {n_vars}} the
                   number of variables in {.arg vars_sim}.")
  }
  names(weights) <- vars_sim
  return(weights)
}
