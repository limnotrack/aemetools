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
#' @param cutoff Numeric. Quantile cutoff (0–1). Used as-is for every
#'   generation unless `cutoff_final` is set.
#' @param mutate Numeric. Fraction of population to mutate (0–1). Used as-is
#'   for every generation unless `mutate_final` is set.
#' @param cutoff_final Numeric. Quantile cutoff to anneal towards by the last
#'   generation (0–1). If `NULL` (the default), `cutoff` stays fixed for the
#'   whole run, matching prior behaviour. If set, `cutoff` is linearly
#'   interpolated from `cutoff` towards `cutoff_final` over the run, e.g.
#'   starting broad (more exploration) and narrowing towards the best
#'   individuals (more exploitation) as the search progresses.
#' @param mutate_final Numeric. Mutation fraction to anneal towards by the
#'   last generation (0–1). If `NULL` (the default), `mutate` stays fixed for
#'   the whole run, matching prior behaviour.
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
    cutoff_final = NULL,
    mutate_final = NULL,
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
    cutoff_final = cutoff_final,
    mutate_final = mutate_final,
    c_method = c_method
  )
}
