#' Run a `pestpp-sen` sensitivity analysis for one model
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The `ctrl$engine == "pest"` branch of \code{\link{sa_aeme}}. Assembles a
#' PEST++ interface whose "observations" are the per-`vars_sim` fit values
#' that \code{\link{sa_aeme}} analyses, runs `pestpp-sen` (Method of
#' Morris), imports every model evaluation into the same sensitivity result
#' tables the built-in Sobol' sampler writes, and stores the solver's own
#' elementary-effects indices in a `sensitivity_indices` table for
#' \code{\link{read_sen}} / \code{\link{plot_sen}}.
#'
#' Reuses the calibration engine's file-writing, launch and run-import
#' helpers unchanged; the only sensitivity-specific pieces are the
#' observation table (\code{\link{pest_sa_obs_table}}), the `obj_mode = "sa"`
#' forward-run path, and the index parser
#' (\code{\link{read_pest_sen_indices}}).
#'
#' @inheritParams calib_aeme_pest
#' @param vars_sim Character vector of the unique AEME variables referenced
#'   by `ctrl$vars_sim` (not the `names(ctrl$vars_sim)` sub-regions).
#'
#' @return The simulation id, as \code{\link{sa_aeme}} returns.
#' @noRd
sa_aeme_pest <- function(aeme, param, m, path, lake_dir, vars_sim,
                         FUN_list, weights, model_controls, ctrl,
                         include_wlev = FALSE) {

  t0 <- Sys.time()
  exe <- pest_exe_path(ctrl$exe)

  # Resolve pest_dir against the lake directory (so runs for different lakes
  # cannot collide) then give each model its own subdirectory - mirrors
  # calib_aeme_pest().
  if (!.pest_is_abs(ctrl$pest_dir)) {
    ctrl$pest_dir <- file.path(lake_dir, ctrl$pest_dir)
  }
  ctrl$pest_dir <- file.path(ctrl$pest_dir, m)
  if (ctrl$overwrite) unlink(ctrl$pest_dir, recursive = TRUE, force = TRUE)
  dir.create(ctrl$pest_dir, recursive = TRUE, showWarnings = FALSE)

  # Cache the netCDF date/depth indices once per sub-region, exactly as
  # sa_aeme() does for its own workers.
  var_indices <- list()
  if (any(vars_sim != "LKE_lvlwtr")) {
    AEME::cli_inform_safe(c("i" = paste0("Extracting variable indices for ",
                                         "{.val ", m, "} [",
                                         format(Sys.time()), "]")))
    suppressMessages(
      var_indices <- run_and_fit(aeme = aeme, param = param, model = m,
                                 path = path, FUN_list = FUN_list,
                                 model_controls = model_controls,
                                 vars_sim = vars_sim, weights = weights,
                                 return_indices = TRUE,
                                 include_wlev = include_wlev,
                                 method = "sa", sa_ctrl = ctrl, fit = FALSE,
                                 timeout = ctrl$timeout)
    )
  }

  param <- param[param$model == m, ]
  if (!"name_full" %in% names(param)) {
    param$name_full <- encode_param(param$group, param$name, param$index)
  }

  # Report the model-run budget (display only - deliberately not written to
  # ctrl$itermax, see create_sen_control()).
  n_runs <- .pest_expected_runs(ctrl, n_par = nrow(param))
  if (!is.na(n_runs)) {
    AEME::cli_inform_safe(c("i" = sprintf(
      "%s will run the model ~%d time%s (%d parameter%s, %d trajectories).",
      ctrl$exe, n_runs, if (n_runs == 1) "" else "s", nrow(param),
      if (nrow(param) == 1) "" else "s", ctrl$morris_r)))
  }

  # Build the interface. Force partrans = "none": pestpp-sen samples in
  # transformed space, and a log parameter sampled in log space would not be
  # comparable with a built-in sa_aeme() run.
  par_tbl <- pest_param_table(param, transform = FALSE)
  obs_tbl <- pest_sa_obs_table(ctrl = ctrl, weights = weights)

  tpl <- write_pest_tpl(par_tbl, ctrl)
  ins <- write_pest_ins(obs_tbl, ctrl)
  write_pest_forward_run(aeme = aeme, param = param, par_tbl = par_tbl,
                         obs_tbl = obs_tbl, model = m, vars_sim = vars_sim,
                         FUN_list = FUN_list, weights = weights, ctrl = ctrl,
                         model_controls = model_controls,
                         var_indices = var_indices,
                         include_wlev = include_wlev)

  rscript <- file.path(R.home("bin"), "Rscript")
  cmd <- paste0("\"", rscript, "\" forward_run.R")
  pst <- write_pst(par_tbl = par_tbl, obs_tbl = obs_tbl, ctrl = ctrl,
                   tpl_files = tpl, ins_files = ins, model_command = cmd)

  # Every run directory needs its own copy of the model configuration.
  .pest_stage_model(ctrl$pest_dir, lake_dir = lake_dir, model = m)
  .pest_preflight(ctrl = ctrl, par_tbl = par_tbl, obs_tbl = obs_tbl,
                  tpl = tpl, ins = ins)

  procs <- .pest_procs()
  on.exit(.pest_cleanup(procs), add = TRUE)
  .pest_launch(pst = pst, exe = exe, ctrl = ctrl, lake_dir = lake_dir, m = m,
               procs = procs)

  # Import every forward run. The run log carries one column per
  # names(ctrl$vars_sim) sub-region, not per AEME variable.
  fit_names <- names(ctrl$vars_sim)
  res <- read_pest_results(ctrl = ctrl, param = param, vars_sim = fit_names)
  if (nrow(res) == 0) {
    cli::cli_abort(c(
      "{.val {ctrl$exe}} produced no completed model runs.",
      "i" = "Check {.file {file.path(ctrl$pest_dir, paste0(ctrl$case, '.rec'))}}."
    ))
  }
  res$run_failed <- as.integer(!is.finite(res$fit) | res$fit == ctrl$na_value)

  # Register "run_failed" as a response variable for this write only, exactly
  # as sa_aeme() does, so write_simulation_output()'s long-format pivot picks
  # it up with no schema change.
  ctrl_out <- ctrl
  ctrl_out$vars_sim$run_failed <- list(var = "run_failed", month = NA_real_,
                                       depth_range = NA_real_)
  x <- res[, c(param$name_full, fit_names, "run_failed", "fit", "gen"),
           drop = FALSE]
  ctrl$sim_id <- write_simulation_output(x = x, ctrl = ctrl_out,
                                         FUN_list = FUN_list, aeme = aeme,
                                         model = m, param = param,
                                         append_metadata = TRUE)

  # Parse and store the solver's own sensitivity indices.
  idx <- read_pest_sen_indices(ctrl = ctrl, param = param,
                               vars_sim = vars_sim, model = m)
  .sen_write_indices(ctrl = ctrl, sim_id = ctrl$sim_id, idx = idx)

  # calibration_metadata is not read back on the sensitivity path, but
  # writing it records `engine` and the resolved `pest_dir` for a later
  # generic lookup, and is NA-tolerant for a control lacking search knobs.
  write_calib_metadata(ctrl = ctrl, nsim = nrow(res), t0 = t0)

  AEME::cli_safe(paste0("{.val ", ctrl$exe, "} sensitivity analysis complete ",
                        "for {.val ", m, "}: {.val ", nrow(res),
                        "} model runs. [", format(Sys.time()), "]"),
                 FUN = cli::cli_alert_success)

  if (ctrl$keep_files) {
    AEME::cli_inform_safe(c("i" = paste0("PEST++ files kept in {.file ",
                                         ctrl$pest_dir, "}")))
  } else {
    unlink(ctrl$pest_dir, recursive = TRUE, force = TRUE)
  }
  ctrl$sim_id
}

#' Store the parsed Morris indices alongside the run results.
#'
#' Written as its own long table rather than columns on `simulation_data`,
#' whose grain is one (run, parameter, fit_type) row - a per-parameter index
#' has no run, and write_simulation_output()'s pivot would drop it.
#' @noRd
.sen_write_indices <- function(ctrl, sim_id, idx) {
  if (is.null(idx) || nrow(idx) == 0) {
    AEME::cli_safe(paste0("No sensitivity indices were parsed from the ",
                          "{.val ", ctrl$exe, "} output; ",
                          "{.fn plot_sen} will have nothing to show."),
                   FUN = cli::cli_alert_warning)
    return(invisible(NULL))
  }
  idx <- cbind(sim_id = sim_id, idx, pest_dir = ctrl$pest_dir,
               sen_method = ctrl$sen_method, stringsAsFactors = FALSE)
  path <- ctrl$file_dir
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  output <- list(sensitivity_indices = idx)
  if (identical(ctrl$file_type, "csv")) {
    write_to_csv(output = output, path = path, sim_id = sim_id, gen_n = 1)
  } else {
    write_to_db(file = ctrl$file_name, path = path, output = output)
  }
  AEME::cli_inform_safe(c("i" = paste0("Stored {.val ", nrow(idx),
                                       "} sensitivity index row",
                                       if (nrow(idx) == 1) "" else "s",
                                       ".")))
  invisible(NULL)
}
