#' Create control list for sensitivity analysis
#'
#' @inheritParams create_calib_control
#'
#' @param N Integer. Base sample size.
#' @param vars_sim Named list describing output variables.
#'
#' @param ... Must be empty. Additional arguments are not allowed.
#'
#' @return A control list.
#' @export
create_sa_control <- function(
    file_type = "db",
    file_name = NULL,
    file_dir = "calib_sa",
    na_value = 999,
    parallel = TRUE,
    ncore = parallel::detectCores() - 1,
    timeout = Inf,
    preflight = TRUE,
    trim_output = TRUE,
    N,
    vars_sim,
    ...
) {

  rlang::check_dots_used()

  .create_control(
    method = "sa",
    file_type = file_type,
    file_name = file_name,
    file_dir = file_dir,
    na_value = na_value,
    parallel = parallel,
    ncore = ncore,
    timeout = timeout,
    preflight = preflight,
    trim_output = trim_output,
    N = N,
    vars_sim = vars_sim
  )
}
