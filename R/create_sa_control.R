#' Create control list for sensitivity analysis
#'
#' @inheritParams create_calib_control
#'
#' @param N Integer. Base sample size.
#' @param vars_sim Named list describing output variables. Each element is
#'   `list(var = <AEME var>, month = <ints>, depth_range = <c(lo, hi)>)`.
#'
#'   `var` can also be one of GLM-AED's sediment / diagnostic outputs in
#'   `AEME::glm_extra_vars` (needs AEME >= 0.4.1 and `model = "glm_aed"`).
#'   These have no depth axis, so `depth_range` is ignored (`month` still
#'   applies):
#'   * lake-wide series such as `"SEDL_Fsed_oxy"` or `"SEDL_frp_dsf"` need
#'     nothing more;
#'   * per-zone variables such as `"SED_Fsed_oxy"` need `zone` (an integer
#'     sediment zone), e.g. `list(var = "SED_Fsed_oxy", zone = 2)`, or use the
#'     area-weighted lake-wide series `"SED_Fsed_oxy_lake"`;
#'   * `"SED_temp"` also takes `sed_layer` (default 1).
#'
#'   Name each element with an underscore, e.g. `sed_oxy`.
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

  # Catch a mis-specified sediment region now rather than at run time
  if (!missing(vars_sim) && is.list(vars_sim)) {
    check_sa_extra_vars(vars_sim = vars_sim)
  }

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
