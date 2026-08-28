#' Create control list for a PEST-driven sensitivity analysis
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Builds the control object for running a global sensitivity analysis with
#' the external `pestpp-sen` engine instead of the built-in Sobol' sampler
#' used by [sa_aeme()] / [create_sa_control()]. The returned object is a
#' `calib_sa_control` with `method = "sa"` and `engine = "pest"`, so the
#' existing sensitivity read/plot pipeline still recognises the run while
#' [sa_aeme()] hands the evaluation loop to PEST++.
#'
#' The only method currently supported is the **Method of Morris**
#' (elementary effects): global screening at `(n_par + 1) * morris_r` model
#' runs, tolerant of a noisy or nonlinear model response. It returns, per
#' parameter per variable, `mu_star` (overall influence) and `sigma`
#' (nonlinearity / interaction).
#'
#' Unlike the calibration path, the sensitivity path always samples the
#' parameters **linearly** on `[min, max]` (`partrans = "none"`), so the
#' Morris indices are directly comparable with a built-in [sa_aeme()] run on
#' the same `param`.
#'
#' @inheritParams create_pest_control
#' @param sen_method Character. Currently only `"morris"`.
#' @param morris_r Integer. Number of Morris trajectories (repetitions).
#'   Default `4`. Model-run cost is `(n_par + 1) * morris_r`.
#' @param morris_p Integer. Number of levels in the Morris sampling grid.
#'   Default `4`.
#' @param morris_delta Numeric. Morris step, as a fraction of the parameter
#'   range. `NULL` (the default) lets `pestpp-sen` pick `p / (2 * (p - 1))`.
#' @param vars_sim Named list describing the output variables, exactly as for
#'   [create_sa_control()]. Each element is
#'   `list(var = <AEME var>, month = <ints>, depth_range = <c(lo, hi)>)`; one
#'   Morris response is produced per named element.
#'
#' @param ... Must be empty. Additional arguments are not allowed.
#'
#' @return A `calib_sa_control` list with `method = "sa"`, `engine = "pest"`.
#' @seealso [sa_aeme()], [create_sa_control()], [read_sen()], [plot_sen()],
#'   [install_pest()]
#' @export
#'
#' @examples
#' ctrl <- create_sen_control(
#'   morris_r = 8, ncore = 4,
#'   vars_sim = list(surf_temp = list(var = "HYD_temp", month = c(12, 1, 2),
#'                                    depth_range = c(0, 2)))
#' )
#' ctrl
create_sen_control <- function(
    file_type = "db",
    file_name = NULL,
    file_dir = "calib_sa",
    na_value = 999,
    parallel = TRUE,
    ncore = parallel::detectCores() - 1,
    timeout = Inf,
    sen_method = "morris",
    morris_r = 4,
    morris_p = 4,
    morris_delta = NULL,
    pest_dir = "pest",
    case = "aeme",
    port = 4004,
    solver_timeout = 24 * 3600,
    stall_minutes = 10,
    derinc = 0.01,
    derinc_lb = 1e-6,
    pestpp_options = list(),
    overwrite = TRUE,
    keep_files = TRUE,
    trim_output = TRUE,
    vars_sim,
    ...
) {

  rlang::check_dots_used()
  sen_method <- rlang::arg_match(sen_method, "morris")

  if (missing(vars_sim) || !is.list(vars_sim) || is.null(names(vars_sim)) ||
      any(!nzchar(names(vars_sim)))) {
    cli::cli_abort("{.arg vars_sim} must be a fully named list, as in
                   {.fn create_sa_control}.")
  }
  # write_simulation_output() long-formats the per-response columns with a
  # `contains("_")` pivot (the same one that sweeps up `run_failed`), so a
  # sub-region name without an underscore would be silently dropped from the
  # stored results. The built-in create_sa_control() relies on this
  # convention too; here it is enforced.
  no_us <- names(vars_sim)[!grepl("_", names(vars_sim))]
  if (length(no_us) > 0) {
    cli::cli_abort(c(
      "Every {.arg vars_sim} name must contain an underscore: {.val {no_us}}.",
      "i" = "e.g. {.val surf_temp}, {.val bot_temp} - the results writer keys
             on it."
    ))
  }
  if (!is.list(pestpp_options) ||
      (length(pestpp_options) > 0 && is.null(names(pestpp_options)))) {
    cli::cli_abort("{.arg pestpp_options} must be a named list.")
  }
  .pest_check_options(pestpp_options)

  # pestpp-sen ignores NOPTMAX, but the control-data line still needs a
  # value; 1 is used rather than 0 because 0 is the PEST++ "single run at
  # initial values" convention.
  ctrl <- .create_control(
    method = "sa",
    engine = "pest",
    file_type = file_type,
    file_name = file_name,
    file_dir = file_dir,
    na_value = na_value,
    parallel = parallel,
    ncore = ncore,
    timeout = timeout,
    c_method = "PESTPP-SEN",
    exe = "pestpp-sen",
    sen_method = sen_method,
    obj_mode = "sa",
    noptmax = 1L,
    morris_r = morris_r,
    morris_p = morris_p,
    morris_delta = morris_delta,
    pest_dir = pest_dir,
    case = case,
    port = port,
    solver_timeout = solver_timeout,
    stall_minutes = stall_minutes,
    derinc = derinc,
    derinc_lb = derinc_lb,
    prior_cov = NULL,
    pestpp_options = pestpp_options,
    overwrite = overwrite,
    keep_files = keep_files,
    trim_output = trim_output,
    vars_sim = vars_sim
  )

  # itermax drives the stall-salvage shortcut in .pest_wait(): if the logged
  # run count reaches it, a non-exiting master is treated as "done and
  # complete". For an ensemble smoother a truncated ensemble is merely
  # smaller, but a truncated Morris design is structurally invalid, so leave
  # itermax NA and let a genuine stall abort instead. The run-count estimate
  # is reported to the user from sa_aeme_pest(), not stored here.
  ctrl$itermax <- NA_real_
  ctrl
}
