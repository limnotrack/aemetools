#' Create control list for calibration
#'
#' @param file_type Character. Output type: `"csv"` or `"db"`.
#'   Default `"db"`.
#' @param file_name Character. Output file name. Defaults to
#'   `"results.db"` (db) or `"simulation_metadata.csv"` (csv).
#' @param file_dir Character. Output directory. Default `"calib_sa"`.
#' @param na_value Numeric. Penalty value substituted for \code{NA} fit values
#'   during optimisation to discourage parameter sets that produce invalid 
#'   model output. Default \code{999}.
#' @param parallel Logical. Run in parallel? Default `TRUE`.
#' @param ncore Integer. Number of cores if `parallel = TRUE`.
#'   Default `parallel::detectCores() - 1`.
#' @param timeout Numeric. Max runtime in seconds. Default `Inf`.
#'
#' @param VTR Numeric. Target objective value. Default `-Inf`.
#' @param NP Integer. Population size. Default `NA`.
#' @param itermax Integer. Maximum iterations. Default `200`.
#' @param reltol Numeric. Relative convergence tolerance. Default `0.07`.
#' @param cutoff Numeric. Quantile cutoff (0–1).
#' @param mutate Numeric. Fraction of population to mutate (0–1).
#' @param c_method Character. `"CMAES"` or `"LHC"`. Default `"CMAES"`.
#'
#' @param ... Must be empty. Additional arguments are not allowed.
#'
#' @return A control list.
#' @export
create_calib_control <- function(
    file_type = "db",
    file_name = NULL,
    file_dir = "calib_sa",
    na_value = 999,
    parallel = TRUE,
    ncore = parallel::detectCores() - 1,
    timeout = Inf,
    VTR = -Inf,
    NP = NA,
    itermax = 200,
    reltol = 0.07,
    cutoff = 0.25,
    mutate = 0.1,
    c_method = "CMAES",
    ...
) {
  
  rlang::check_dots_used()
  
  .create_control(
    method = "calib",
    file_type = file_type,
    file_name = file_name,
    file_dir = file_dir,
    na_value = na_value,
    parallel = parallel,
    ncore = ncore,
    timeout = timeout,
    VTR = VTR,
    NP = NP,
    itermax = itermax,
    reltol = reltol,
    cutoff = cutoff,
    mutate = mutate,
    c_method = c_method
  )
}
