#' Calibrate AEME with an external PEST++ solver
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The `ctrl$method == "pest"` branch of \code{\link{calib_aeme}}. Assembles
#' the PEST interface files, launches the solver (optionally with PANTHER
#' workers), then imports every model evaluation into the same results
#' database the built-in methods write to, so `get_best_params()`,
#' `update_param()` and the `plot_calib_*()` family work unchanged.
#'
#' @inheritParams calib_aeme
#' @param m Character. The single model being calibrated, e.g. `"glm_aed"`.
#' @param lake_dir Character. Lake directory from `AEME::get_lake_dir()`.
#'
#' @return The simulation id, as \code{\link{calib_aeme}} returns.
#' @noRd
calib_aeme_pest <- function(aeme, param, m, path, lake_dir, vars_sim,
                            FUN_list, weights, model_controls, ctrl,
                            include_wlev = FALSE) {

  t0 <- Sys.time()
  exe <- pest_exe_path(ctrl$exe)

  # Resolve pest_dir against the lake directory so runs for different lakes
  # cannot collide, then give each model its own subdirectory.
  #
  # calib_aeme() passes one control to every model in turn, so without the
  # model suffix a two-model call would run both in the same directory and,
  # with overwrite = TRUE, the second would delete the first's files. The
  # first model's results are already in the database by then, but its
  # recorded pest_dir would point at the second model's ensembles - so
  # reading its posterior afterwards would silently return the wrong ones.
  if (!.pest_is_abs(ctrl$pest_dir)) {
    ctrl$pest_dir <- file.path(lake_dir, ctrl$pest_dir)
  }
  ctrl$pest_dir <- file.path(ctrl$pest_dir, m)
  if (ctrl$overwrite) unlink(ctrl$pest_dir, recursive = TRUE, force = TRUE)
  dir.create(ctrl$pest_dir, recursive = TRUE, showWarnings = FALSE)

  # Cache the netCDF date/depth indices once, exactly as the built-in
  # methods do, so every forward run skips re-deriving them.
  var_indices <- list()
  if (any(vars_sim != "LKE_lvlwtr")) {
    AEME::cli_inform_safe(c("i" = paste0("Extracting indices for {.val ", m,
                                         "} modelled variables [",
                                         format(Sys.time()), "]")))
    suppressMessages(
      var_indices <- run_and_fit(aeme = aeme, param = param, model = m,
                                 path = path, FUN_list = FUN_list,
                                 model_controls = model_controls,
                                 vars_sim = vars_sim, weights = weights,
                                 return_indices = TRUE,
                                 include_wlev = include_wlev, fit = FALSE)
    )
  }

  param <- param[param$model == m, ]

  # Now that the parameter count is known the run budget can be reported -
  # and for pestpp-glm, whose cost is driven by the Jacobian, computed at
  # all. Recorded on ctrl so write_calib_metadata() stores it alongside the
  # itermax of a built-in run.
  ctrl$itermax <- .pest_expected_runs(ctrl, n_par = nrow(param))
  if (!is.na(ctrl$itermax)) {
    approx <- if (identical(ctrl$exe, "pestpp-glm")) "at least " else ""
    msg <- sprintf("%s will run the model %s%d time%s (%d iteration%s, %d parameters).",
                   ctrl$exe, approx, ctrl$itermax,
                   if (ctrl$itermax == 1) "" else "s",
                   ctrl$noptmax, if (ctrl$noptmax == 1) "" else "s",
                   nrow(param))
    AEME::cli_inform_safe(c("i" = msg))
  }

  # Build the interface ----
  par_tbl <- pest_param_table(param)
  obs_tbl <- pest_obs_table(aeme = aeme, vars_sim = vars_sim,
                            weights = weights, obj_mode = ctrl$obj_mode,
                            var_indices = var_indices)

  # Optionally generate the prior parameter ensemble and the observation
  # (noise) ensemble in R, adding the matching `++ies_*` options to `ctrl`
  # before write_pst() emits them.
  ctrl <- .pest_setup_ensembles(ctrl, par_tbl = par_tbl, obs_tbl = obs_tbl,
                                param = param)

  # Turn the parameter/variable declaration into an ies_localizer, now that
  # the parameter and observation groups it maps between are known.
  ctrl <- .pest_setup_localizer(ctrl, par_tbl = par_tbl, obs_tbl = obs_tbl,
                                param = param, vars_sim = vars_sim)

  tpl <- write_pest_tpl(par_tbl, ctrl)
  ins <- write_pest_ins(obs_tbl, ctrl)
  write_pest_forward_run(aeme = aeme, param = param, par_tbl = par_tbl,
                         obs_tbl = obs_tbl, model = m, vars_sim = vars_sim,
                         FUN_list = FUN_list, weights = weights, ctrl = ctrl,
                         model_controls = model_controls,
                         var_indices = var_indices,
                         include_wlev = include_wlev)

  # PEST invokes this through the shell, so quote the interpreter path -
  # R is installed under "Program Files" on most Windows machines.
  rscript <- file.path(R.home("bin"), "Rscript")
  cmd <- paste0("\"", rscript, "\" forward_run.R")

  pst <- write_pst(par_tbl = par_tbl, obs_tbl = obs_tbl, ctrl = ctrl,
                   tpl_files = tpl, ins_files = ins, model_command = cmd)

  # Every run directory needs its own copy of the model configuration,
  # because AEME models write their output alongside their inputs.
  .pest_stage_model(ctrl$pest_dir, lake_dir = lake_dir, model = m)

  # Verify the interface before handing over to the solver ----
  .pest_preflight(ctrl = ctrl, par_tbl = par_tbl, obs_tbl = obs_tbl,
                  tpl = tpl, ins = ins)

  # Solve ----
  # The master and its agents run detached, so an abort in .pest_wait() or a
  # user interrupt would otherwise leave them (and the models they spawned)
  # running and holding the PANTHER port. The holder is created and the
  # cleanup registered before launching, so it covers every exit path.
  procs <- .pest_procs()
  on.exit(.pest_cleanup(procs), add = TRUE)
  .pest_launch(pst = pst, exe = exe, ctrl = ctrl, lake_dir = lake_dir, m = m,
               procs = procs)

  # Import ----
  res <- read_pest_results(ctrl = ctrl, param = param, vars_sim = vars_sim)
  if (nrow(res) == 0) {
    cli::cli_abort(c(
      "PEST++ produced no completed model runs.",
      "i" = "Check {.file {file.path(ctrl$pest_dir, paste0(ctrl$case, '.rec'))}}."
    ))
  }

  ctrl$sim_id <- write_simulation_output(x = res, ctrl = ctrl,
                                         FUN_list = FUN_list, aeme = aeme,
                                         model = m, param = param,
                                         append_metadata = TRUE)

  # write_simulation_output() numbers runs by row order, so posterior
  # membership computed on `res` rows lines up with the stored `run`.
  post <- pest_posterior_runs(ctrl = ctrl, param = param, res = res)
  .pest_write_posterior(ctrl = ctrl, sim_id = ctrl$sim_id, post = post)

  write_calib_metadata(ctrl = ctrl, nsim = nrow(res), t0 = t0)

  best <- res[which.min(res$fit), ]
  AEME::cli_safe(paste0("{.val ", ctrl$exe, "} complete for {.val ", m,
                        "}: {.val ", nrow(res), "} runs, best fit {.val ",
                        signif(min(res$fit), 4), "}. [", format(Sys.time()),
                        "]"), FUN = cli::cli_alert_success)

  if (ctrl$keep_files) {
    # `pest_dir` was resolved against the lake directory above, so the
    # control the caller still holds does not know where the files went.
    # Say where they are, and it is also recorded in calibration_metadata
    # for the reading functions to pick up.
    AEME::cli_inform_safe(c("i" = paste0("PEST++ files kept in {.file ",
                                         ctrl$pest_dir, "}")))
  } else {
    unlink(ctrl$pest_dir, recursive = TRUE, force = TRUE)
  }
  ctrl$sim_id
}

#' Import PEST++ model runs into the aemetools results shape
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Reads the run log written by \code{\link{pest_forward_run}} and returns a
#' dataframe in the shape \code{\link{calib_aeme}} produces: one row per
#' model evaluation, one column per `param$name_full`, one column per
#' `vars_sim` entry, plus `fit` and `gen`.
#'
#' The run log is used in preference to PEST++'s own output files because
#' those differ between solvers (`pestpp-ies` writes parameter and
#' observation ensembles, `pestpp-glm` writes a single best-parameter file)
#' and their names have changed between releases. The log also captures
#' *every* evaluation, including rejected ones, which is what the results
#' schema records.
#'
#' Generation numbers are recovered by matching logged parameter vectors
#' against the per-iteration ensembles `<case>.<n>.par.csv` when the solver
#' writes them; otherwise every run is reported as generation 1.
#'
#' @param ctrl list; from \code{\link{create_pest_control}}.
#' @param param dataframe; the parameters that were calibrated.
#' @param vars_sim character vector; simulated variables.
#'
#' @return A dataframe of model runs, or a zero-row dataframe if none ran.
#' @export
read_pest_results <- function(ctrl, param, vars_sim) {

  ctrl <- utils::modifyList(as.list(ctrl), .pest_locate(ctrl))
  log_dir <- file.path(ctrl$pest_dir, "runlog")
  logs <- list.files(log_dir, pattern = "^runlog_.*\\.csv$", full.names = TRUE)
  if (length(logs) == 0) return(data.frame())

  res <- dplyr::bind_rows(lapply(logs, utils::read.csv,
                                 stringsAsFactors = FALSE,
                                 check.names = FALSE))
  if (nrow(res) == 0) return(data.frame())

  if (!"name_full" %in% names(param)) {
    param$name_full <- encode_param(param$group, param$name, param$index)
  }
  missing_cols <- setdiff(c(param$name_full, vars_sim, "fit"), names(res))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Run log is missing column{?s}: {.val {missing_cols}}")
  }

  res <- res[order(res$time), , drop = FALSE]
  res$gen <- .pest_assign_gen(res, ctrl = ctrl, param = param)

  res[, c(param$name_full, vars_sim, "fit", "gen")]
}

# Internal helpers -------------------------------------------------------

#' @noRd
.pest_is_abs <- function(p) grepl("^(/|~|[A-Za-z]:)", p)

#' Locate the final parameter and simulated-observation ensembles of a
#' previous `pestpp-ies` run, for a warm start.
#' @param x a run directory, a `read_calib()` object, or a control object.
#' @return list of `par`, `obs` (file paths) and `iteration`.
#' @noRd
.pest_restart_files <- function(x) {
  loc <- .pest_locate(x)
  pe <- .pest_ensemble_files(loc$pest_dir, loc$case, "par")
  oe <- .pest_ensemble_files(loc$pest_dir, loc$case, "obs")
  if (nrow(pe) == 0 || nrow(oe) == 0) {
    cli::cli_abort("No {.val pestpp-ies} ensembles to restart from in
                   {.file {loc$pest_dir}}.")
  }
  it <- max(pe$iteration)
  ob <- oe$path[oe$iteration == it]
  if (length(ob) == 0) ob <- oe$path[which.max(oe$iteration)]
  list(par = pe$path[pe$iteration == it][1], obs = ob[1], iteration = it)
}

#' Decide whether to generate the prior parameter ensemble in R.
#'
#' Returns a path (use that CSV as-is), `TRUE` (generate), or `FALSE` (leave
#' it to pestpp-ies). `NULL` in the control means "auto": generate when a
#' seed, a covariance matrix, a non-uniform prior, or a noise ensemble makes
#' an aligned, reproducible ensemble necessary.
#' @noRd
.pest_want_par_en <- function(ctrl) {
  ppe <- ctrl$prior_par_ensemble
  if (is.character(ppe) && nzchar(ppe)) return(ppe)
  if (isTRUE(ppe)) return(TRUE)
  if (isFALSE(ppe)) return(FALSE)
  any(!is.null(ctrl$seed),
      is.matrix(ctrl$prior_cov),
      !identical(ctrl$prior_dist %||% "uniform", "uniform"),
      (!is.null(ctrl$noise_sd) &&
         identical(ctrl$noise_method %||% "ensemble", "ensemble")))
}

#' Generate the prior parameter and observation ensembles (when requested)
#' and record the matching `++ies_*` options on `ctrl`.
#' @noRd
.pest_setup_ensembles <- function(ctrl, par_tbl, obs_tbl, param) {

  if (!identical(ctrl$exe, "pestpp-ies")) return(ctrl)

  opts <- ctrl$pestpp_options %||% list()
  n <- ctrl$ies_num_reals
  include_base <- ctrl$include_base %||% TRUE
  par_en_file <- NULL

  # Warm-start: resume from a previous run's final ensembles. Takes
  # precedence over any prior-ensemble generation and over noise_sd.
  if (!is.null(ctrl$restart_from)) {
    rf <- .pest_restart_files(ctrl$restart_from)
    pe <- .pest_read_ens_file(rf$par)
    oe <- .pest_read_ens_file(rf$obs)

    miss_p <- setdiff(par_tbl$parnme, names(pe))
    miss_o <- setdiff(obs_tbl$obsnme, names(oe))
    if (length(miss_p) || length(miss_o)) {
      cli::cli_abort(c(
        "The run in {.arg restart_from} does not match this problem.",
        "x" = if (length(miss_p)) "Missing parameter{?s}: {.val {miss_p}}." else
          "Missing observation{?s}: {.val {miss_o}}."
      ))
    }
    pe <- pe[, c(names(pe)[1], par_tbl$parnme)]
    oe <- oe[, c(names(oe)[1], obs_tbl$obsnme)]
    # pestpp-ies builds its "obs+noise" ensemble with sequential integer
    # realisation names and then requires the restart ensembles to match, so
    # rename both (same iteration, same row order) to 0..N-1.
    seq_names <- as.character(seq_len(nrow(pe)) - 1L)
    pe[[1]] <- seq_names
    oe[[1]] <- seq_names
    utils::write.csv(pe, file.path(ctrl$pest_dir, "restart_par.csv"),
                     row.names = FALSE, quote = FALSE)
    utils::write.csv(oe, file.path(ctrl$pest_dir, "restart_obs.csv"),
                     row.names = FALSE, quote = FALSE)

    opts$ies_parameter_ensemble <- "restart_par.csv"
    opts$ies_restart_observation_ensemble <- "restart_obs.csv"
    opts$ies_num_reals <- nrow(pe)
    opts$ies_include_base <- tolower(as.character(isTRUE(include_base)))
    ctrl$pestpp_options <- opts
    ctrl$ies_num_reals <- nrow(pe)
    AEME::cli_safe(
      paste0("Restarting {.val pestpp-ies} from iteration {.val ", rf$iteration,
             "} of {.file ", dirname(rf$par), "} ({.val ", nrow(pe),
             "} realisations)."),
      FUN = cli::cli_alert_info)
    return(ctrl)
  }

  want <- .pest_want_par_en(ctrl)
  if (is.character(want)) {
    if (!file.exists(want)) {
      cli::cli_abort("{.arg prior_par_ensemble} file not found: {.file {want}}")
    }
    par_en_file <- file.path(ctrl$pest_dir, "prior_par_en.csv")
    file.copy(want, par_en_file, overwrite = TRUE)
    opts$ies_parameter_ensemble <- basename(par_en_file)

  } else if (isTRUE(want)) {
    par_en_file <- file.path(ctrl$pest_dir, "prior_par_en.csv")
    pest_prior_ensemble(
      param, n = n, dist = ctrl$prior_dist %||% "uniform",
      cov = if (is.matrix(ctrl$prior_cov)) ctrl$prior_cov else NULL,
      seed = ctrl$seed, include_base = include_base, file = par_en_file
    )
    opts$ies_parameter_ensemble <- basename(par_en_file)
  }

  if (!is.null(ctrl$noise_sd) && identical(ctrl$noise_method, "standard_deviation")) {
    # Hand pestpp-ies a diagonal observation-noise covariance and let it draw
    # its own noise ensemble from it (reproducible via ++ies_noise_seed).
    ocv <- file.path(ctrl$pest_dir, paste0(ctrl$case, "_obscov.unc"))
    .pest_write_obscov(obs_tbl, ctrl$noise_sd, ocv)
    opts$obscov <- basename(ocv)
    # pestpp-ies refuses a user-supplied obscov alongside ies_drop_conflicts.
    # `opts` holds the user's options only - the package defaults are added
    # later, in .pest_plusplus_lines(), where this now defaults to "false" -
    # so an absent entry means there is nothing to disable.
    if (.pest_opt_true(opts$ies_drop_conflicts)) {
      opts$ies_drop_conflicts <- "false"
      AEME::cli_safe(
        "Disabling {.code ies_drop_conflicts}: {.val pestpp-ies} does not
         allow it together with a supplied {.code obscov}. Use
         {.fn pest_prior_data_conflict} on the finished run to see conflicts.",
        FUN = cli::cli_alert_info)
    }

  } else if (!is.null(ctrl$noise_sd) && !identical(ctrl$noise_method, "none")) {
    rns <- if (!is.null(par_en_file) && file.exists(par_en_file)) {
      utils::read.csv(par_en_file, check.names = FALSE,
                      stringsAsFactors = FALSE)[[1]]
    } else {
      AEME::cli_safe(
        "Generating an observation ensemble without a matching parameter
         ensemble; realisation names may not align with the pestpp-ies draw.",
        FUN = cli::cli_alert_warning)
      NULL
    }
    obs_en_file <- file.path(ctrl$pest_dir, "obs_en.csv")
    pest_obs_ensemble(
      obs_tbl, n = n, noise_sd = ctrl$noise_sd,
      seed = if (is.null(ctrl$seed)) NULL else ctrl$seed + 1L,
      include_base = include_base, real_names = rns, file = obs_en_file
    )
    opts$ies_observation_ensemble <- basename(obs_en_file)
  }

  if (isTRUE(ctrl$ies_no_noise)) opts$ies_no_noise <- "true"

  # Only pin ies_include_base when it matters: the user opted out, or we
  # supplied an ensemble whose last realisation is the base.
  if (!isTRUE(include_base) || !is.null(opts$ies_parameter_ensemble) ||
      !is.null(opts$ies_observation_ensemble)) {
    opts$ies_include_base <- tolower(as.character(isTRUE(include_base)))
  }

  if (isTRUE(as.logical(opts$ies_save_binary %||%
                        ctrl$pestpp_options$ies_save_binary %||% FALSE))) {
    AEME::cli_safe(
      "{.code ies_save_binary} is on; per-iteration ensembles will be read
       back from the {.file .jcb} files.",
      FUN = cli::cli_alert_info)
  }

  ctrl$pestpp_options <- opts
  ctrl
}

#' Match rows of a PEST++ parameter ensemble to logged model runs.
#'
#' Matching is on value rather than on order, because PANTHER agents
#' complete out of order and the run log is interleaved across processes.
#'
#' It has to be *tolerant*, though. PEST++ writes its ensemble CSVs at
#' about six significant digits while the run log records full double
#' precision - `0.753399` against `0.753399343801413` - so comparing
#' exactly, or on a rounded string, matches nothing at all. Worse, that
#' fails silently: the posterior comes back empty and every run is filed
#' under the first generation.
#'
#' Each ensemble row is therefore matched to its nearest logged run by
#' maximum relative difference across the parameters, and accepted only if
#' that difference is within `tol`. Distinct realisations differ by far
#' more than the precision loss, so this is unambiguous in practice.
#'
#' @param ens dataframe; one ensemble, columns named as `cols`.
#' @param res dataframe; the logged runs.
#' @param cols character; parameter columns to compare on.
#' @param tol numeric; maximum relative difference. The default is several
#'   orders of magnitude looser than the ~5e-7 the ensemble's precision
#'   implies, and far tighter than the spacing between realisations.
#' @return Integer vector, one per row of `ens`, indexing `res`; `NA`
#'   where no run matched.
#' @noRd
.pest_match_rows <- function(ens, res, cols, tol = 1e-4) {
  if (nrow(ens) == 0 || nrow(res) == 0) return(integer(0))
  E <- as.matrix(ens[, cols, drop = FALSE])
  R <- as.matrix(res[, cols, drop = FALSE])

  vapply(seq_len(nrow(E)), function(i) {
    e <- E[i, ]
    d <- apply(R, 1, function(r) {
      max(abs(r - e) / pmax(abs(r), abs(e), 1e-12))
    })
    j <- which.min(d)
    if (length(j) == 0 || !is.finite(d[j]) || d[j] > tol) {
      NA_integer_
    } else {
      as.integer(j)
    }
  }, integer(1))
}

#' Resolve where a PEST++ run's files actually are.
#'
#' `pest_dir` is relative by default and is resolved against the lake
#' directory when the run starts, so the control object the caller still
#' holds points at `"pest"` while the files are under
#' `<lake_dir>/pest`. The resolved path is recorded in
#' `calibration_metadata$pest_dir`, so the reading functions accept
#' whichever of these the caller has to hand:
#'
#' * the object from \code{\link{read_calib}} - the resolved path is read
#'   straight out of its metadata, which is the intended route;
#' * a directory path;
#' * a control from \code{\link{create_pest_control}}, which works when
#'   `pest_dir` was given as an absolute path or the working directory is
#'   the run directory.
#'
#' @param x a calib list, a directory path, or a control object.
#' @param case Character; overrides the case name, otherwise taken from the
#'   control or inferred from the `.pst` file present.
#' @return A list with `pest_dir` and `case`.
#' @noRd
.pest_locate <- function(x, case = NULL) {

  from_ctrl <- FALSE
  if (is.character(x)) {
    dir <- x[[1]]
  } else if (inherits(x, "calib_sa_control")) {
    dir <- x$pest_dir
    case <- case %||% x$case
    from_ctrl <- TRUE
  } else if (is.list(x) && !is.null(x[["calibration_metadata"]])) {
    pd <- x[["calibration_metadata"]][["pest_dir"]]
    pd <- pd[!is.na(pd)]
    if (length(pd) == 0) {
      cli::cli_abort(c(
        "No PEST++ directory recorded for this calibration.",
        "i" = "Only runs made with {.fn create_pest_control} record one."
      ))
    }
    dir <- pd[[length(pd)]]
  } else if (is.list(x) && !is.null(x[["pest_dir"]])) {
    # Already resolved. The plot functions locate once and then hand the
    # result to a reader that locates again, so this must be idempotent.
    dir <- x[["pest_dir"]]
    case <- case %||% x[["case"]]
  } else {
    cli::cli_abort("{.arg ctrl} must be a calib list, a directory path, or a
                   {.fn create_pest_control} object.")
  }

  if (!dir.exists(dir)) {
    hint <- if (from_ctrl) {
      "{.arg pest_dir} is resolved against the lake directory when the run
       starts, so the control still says {.file {dir}} while the files are
       under {.file <lake_dir>/{dir}}. Pass the object from
       {.fn read_calib} instead, or the full path."
    } else {
      "No such directory."
    }
    cli::cli_abort(c("Cannot find the PEST++ run directory {.file {dir}}.",
                     "i" = hint))
  }

  if (is.null(case)) {
    pst <- list.files(dir, pattern = "[.]pst$")
    case <- if (length(pst) > 0) sub("[.]pst$", "", pst[[1]]) else "aeme"
  }
  list(pest_dir = dir, case = case)
}

#' Read the parnme -> name_full map written beside the control file.
#' @noRd
.pest_par_map <- function(ctrl) {
  f <- file.path(ctrl$pest_dir, paste0(ctrl$case, "_par_map.csv"))
  if (!file.exists(f)) return(NULL)
  utils::read.csv(f, stringsAsFactors = FALSE)
}

#' Read a PEST++ parameter ensemble CSV, renaming its columns back to
#' aemetools parameter names. Returns NULL when the file does not carry
#' every calibrated parameter.
#' @noRd
.pest_read_ensemble <- function(f, par_map, param) {
  ens <- tryCatch(.pest_read_ens_file(f), error = function(e) NULL)
  if (is.null(ens)) return(NULL)
  cols <- par_map$parnme[par_map$parnme %in% names(ens)]
  if (length(cols) != nrow(param)) return(NULL)
  names(ens)[match(cols, names(ens))] <-
    par_map$name_full[match(cols, par_map$parnme)]
  ens
}

#' Identify which logged runs form the solver's final (posterior) ensemble.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The posterior parameter ensemble is the product of an iterative ensemble
#' smoother run - it is what gets pushed back through the model to give
#' predictive uncertainty - and it is **not** recoverable by filtering the
#' results for the last generation.
#'
#' The run log records every forward run, and each iteration contains the
#' lambda/scale-factor test runs on a subset of realisations as well as the
#' remainder run at the winning combination. Most of those candidates are
#' rejected. The accepted ensemble is specifically
#' `<case>.<final>.par.csv`, a curated subset, so filtering by generation
#' returns a plausible-looking ensemble that is not the posterior.
#'
#' @param ctrl list; from \code{\link{create_pest_control}}.
#' @param param dataframe; the parameters that were calibrated.
#' @param res dataframe; from \code{\link{read_pest_results}}, in the row
#'   order it will be written to the database (row number becomes `run`).
#'
#' @return A dataframe of `run`, `realisation` and `iteration`, or a
#'   zero-row dataframe when the solver wrote no ensembles.
#' @export
pest_posterior_runs <- function(ctrl, param, res) {

  ctrl <- utils::modifyList(as.list(ctrl), .pest_locate(ctrl))
  empty <- data.frame(run = integer(), realisation = character(),
                      iteration = integer(), stringsAsFactors = FALSE)
  if (nrow(res) == 0) return(empty)

  ef <- .pest_ensemble_files(ctrl$pest_dir, ctrl$case, "par")
  if (nrow(ef) == 0) return(empty)

  final <- ef$path[which.max(ef$iteration)]
  final_it <- max(ef$iteration)

  if (!"name_full" %in% names(param)) {
    param$name_full <- encode_param(param$group, param$name, param$index)
  }
  par_map <- .pest_par_map(ctrl)
  if (is.null(par_map)) return(empty)
  ens <- .pest_read_ensemble(final, par_map, param)
  if (is.null(ens)) return(empty)

  # First column of a PEST++ ensemble CSV is the realisation name
  # (Ensemble::to_csv writes "real_name" then the parameter columns).
  real_col <- names(ens)[1]

  idx <- .pest_match_rows(ens, res, param$name_full)
  ok <- !is.na(idx)

  if (!any(ok)) {
    # Silence here would produce an empty posterior that looks like a
    # legitimate result, which is the exact failure this function exists to
    # prevent - so say so.
    AEME::cli_safe(
      paste0("Could not match any realisation in {.file ", basename(final),
             "} to a logged model run; the posterior ensemble will be empty."),
      FUN = cli::cli_alert_warning
    )
    return(empty)
  }

  data.frame(run = as.integer(idx[ok]),
             realisation = as.character(ens[[real_col]][ok]),
             iteration = final_it,
             stringsAsFactors = FALSE)
}

#' Store the posterior ensemble membership alongside the run results.
#'
#' Written as its own table rather than a column on `simulation_data`,
#' because that table's grain is one (run, parameter, fit_type) row and a
#' per-run flag would be duplicated across every parameter - and because
#' write_simulation_output()'s pivot would silently drop an unrecognised
#' column.
#' @noRd
.pest_write_posterior <- function(ctrl, sim_id, post) {

  if (nrow(post) == 0) return(invisible(NULL))
  post <- cbind(sim_id = sim_id, post)
  output <- list(pest_posterior = post)
  path <- ctrl$file_dir
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  if (ctrl$file_type == "csv") {
    write_to_csv(output = output, path = path, sim_id = sim_id, gen_n = 1)
  } else {
    write_to_db(file = ctrl$file_name, path = path, output = output)
  }
  AEME::cli_inform_safe(c("i" = paste0("Stored posterior ensemble of ",
                                       nrow(post), " realisation",
                                       if (nrow(post) == 1) "" else "s",
                                       " (iteration ", post$iteration[1], ").")))
  invisible(NULL)
}

#' Recover the solver iteration each logged run belongs to.
#'
#' Matches on a rounded string encoding of the parameter vector rather than
#' on run order, because PANTHER agents complete out of order and the log is
#' interleaved across processes.
#' @noRd
.pest_assign_gen <- function(res, ctrl, param) {

  ef <- .pest_ensemble_files(ctrl$pest_dir, ctrl$case, "par")
  if (nrow(ef) == 0) return(rep(1L, nrow(res)))

  par_map <- .pest_par_map(ctrl)
  if (is.null(par_map)) return(rep(1L, nrow(res)))

  # `.pest_ensemble_files()` already orders by numeric iteration, so
  # iteration 10 sorts after iteration 2.
  files <- ef$path
  its <- ef$iteration

  gen <- rep(NA_integer_, nrow(res))
  for (k in seq_along(files)) {
    ens <- .pest_read_ensemble(files[k], par_map, param)
    if (is.null(ens)) next
    idx <- .pest_match_rows(ens, res, param$name_full)
    idx <- idx[!is.na(idx)]
    idx <- idx[is.na(gen[idx])]
    # PEST iterations are 0-based (0 is the prior ensemble); generations in
    # the results schema are 1-based.
    if (length(idx)) gen[idx] <- its[k] + 1L
  }
  gen[is.na(gen)] <- 1L
  gen
}

#' Copy the model configuration into a PEST run directory.
#'
#' Mirrors the file filtering in `make_temp_dir()`: outputs, restart files
#' and netCDF from a previous run must not be carried in, or the forward run
#' can read stale output.
#'
#' The `output/` directory is not copied (it only holds a previous run's
#' results) but it *is* recreated empty: GLM 4.0.0, as bundled with AEME
#' 0.4.0, does not create its own output directory and aborts during
#' initialisation if it is missing.
#' @noRd
.pest_stage_model <- function(dir, lake_dir, model) {
  src <- file.path(lake_dir, model)
  dest <- file.path(dir, basename(lake_dir), model)
  unlink(dest, recursive = TRUE, force = TRUE)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  fils <- list.files(src, full.names = TRUE)
  keep <- !basename(fils) %in% "output" & !grepl("restart|nc$", basename(fils))
  file.copy(fils[keep], dest, recursive = TRUE)
  dir.create(file.path(dest, "output"), showWarnings = FALSE)
  invisible(dest)
}

#' A mutable holder for the processes a PANTHER run spawns.
#'
#' `calib_aeme_pest()` registers `on.exit(.pest_cleanup(procs))` *before*
#' calling `.pest_launch()`, so the holder has to exist first and be filled
#' in as processes are spawned - that way any abort (in `.pest_launch()`
#' itself, in `.pest_wait()`, or a user interrupt) tears down whatever is
#' already running.
#' @noRd
.pest_procs <- function() {
  e <- new.env(parent = emptyenv())
  e$master <- NULL
  e$agents <- list()
  e
}

#' Environment overrides for every process this module spawns.
#'
#' Each PANTHER agent is a full R process (`forward_run.R` does
#' `library(aemetools)`). With the default multi-threaded BLAS, N agents each
#' start a thread pool sized to the machine, and N x ncores threads plus
#' their buffers exhausts memory - "OpenBLAS error: Memory allocation still
#' failed after 10 retries, giving up." The model is a Fortran binary that
#' does no BLAS, so pin every spawned process to a single BLAS/OpenMP
#' thread.
#' @noRd
.pest_env <- function() {
  c("current",
    OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1", GOTO_NUM_THREADS = "1",
    RCPP_PARALLEL_NUM_THREADS = "1")
}

#' The PANTHER argument vector processx wants (`system2` took one string).
#' @noRd
.pest_args <- function(case, host, port) {
  if (missing(host)) return(case)
  c(case, "/h", paste0(host, ":", port))
}

#' Spawn a PEST++ process with its whole descendant tree tracked.
#'
#' `cleanup_tree = TRUE` + `$kill_tree()` reaches the model processes an
#' agent starts - `tools::pskill()` on the agent PID alone never did, which
#' is how a crashed agent left GLM behind and successive benchmark
#' configurations piled up orphans until the machine fell over.
#' `supervise = TRUE` adds a watchdog that kills the tree if this R session
#' dies without cleaning up.
#' @noRd
.pest_spawn <- function(exe, args, wd, tag) {
  processx::process$new(
    command = exe, args = args, wd = wd,
    stdout = file.path(wd, paste0(tag, ".out")),
    stderr = file.path(wd, paste0(tag, ".err")),
    env = .pest_env(),
    cleanup_tree = TRUE, supervise = TRUE)
}

#' Launch the solver, serially or as a PANTHER master plus workers.
#'
#' On the PANTHER path every spawned process is stored in `procs` (an
#' environment shared with the caller's `on.exit(.pest_cleanup())`).
#' @noRd
.pest_launch <- function(pst, exe, ctrl, lake_dir, m, procs = .pest_procs()) {

  case <- basename(pst)
  wd <- ctrl$pest_dir

  # PEST++ parallelises only through a run manager, so `parallel` selects
  # PANTHER vs a serial run in which the master evaluates the model itself.
  if (!ctrl$parallel) {
    AEME::cli_inform_safe(c(">" = paste0("Running {.val ", ctrl$exe,
                                         "} serially [", format(Sys.time()),
                                         "]")))
    res <- processx::run(exe, .pest_args(case), wd = wd, env = .pest_env(),
                         error_on_status = FALSE, echo = FALSE,
                         stdout = "", stderr = "")
    if (res$status != 0) {
      cli::cli_abort("{.val {ctrl$exe}} exited with status {.val {res$status}}.")
    }
    return(invisible(procs))
  }

  AEME::cli_inform_safe(c("i" = paste0(
    "Running {.val ", ctrl$exe, "} with PANTHER on port {.val ", ctrl$port,
    "} [", format(Sys.time()), "]")))

  # Master first, so it is listening before the agents try to connect; they
  # retry on their own until it is up.
  procs$master <- .pest_spawn(exe, .pest_args(case, "", ctrl$port), wd,
                              "run_master")

  for (i in seq_len(ctrl$ncore)) {
    agent <- file.path(wd, sprintf("agent_%02d", i))
    unlink(agent, recursive = TRUE, force = TRUE)
    dir.create(agent, recursive = TRUE, showWarnings = FALSE)
    fils <- list.files(wd, full.names = TRUE)
    # Don't copy other agents' dirs, the shared run log, or the master's
    # own stdout/stderr into a fresh agent.
    fils <- fils[!grepl("^(agent_|runlog$|run_master\\.)", basename(fils))]
    file.copy(fils, agent, recursive = TRUE)
    procs$agents[[i]] <- .pest_spawn(
      exe, .pest_args(case, "localhost", ctrl$port), agent, "run_agent")
  }

  .pest_wait(procs, wd, case, ctrl)
  invisible(procs)
}

#' Kill any PEST++ process this run started that is still alive, tree and
#' all (the agents' model subprocesses included).
#' @noRd
.pest_cleanup <- function(procs) {
  if (!is.environment(procs)) return(invisible(0L))
  killed <- 0L
  for (p in c(list(procs$master), procs$agents)) {
    if (inherits(p, "process") && isTRUE(tryCatch(p$is_alive(),
                                                  error = function(e) FALSE))) {
      tryCatch({
        p$kill_tree()
        killed <- killed + 1L
      }, error = function(e) NULL)
    }
  }

  if (killed > 0) {
    # {cli::qty()} before {.val N}: cli reads the plural quantity from a glue
    # substitution, and {.val <literal>} pasted in by paste0() is not one.
    AEME::cli_safe(paste0("Stopped {cli::qty(", killed, ")}{.val ", killed,
                          "} running PEST++ process tree{?s}."),
                   FUN = cli::cli_alert_info)
  }
  invisible(killed)
}

#' Phrases PEST++ writes to the record file when it is aborting.
#'
#' Derived from the PEST++ sources rather than from observed output, because
#' matching the wrong thing fails in both directions - too loose aborts a
#' healthy run, too tight polls forever through a real failure.
#'
#' * `EnsembleMethod::throw_em_error()` (pestpp-ies/-da/-mou) writes
#'   `"   ************   "` then `"    <alg_tag> error: <message>"`. There
#'   are ~90 call sites and `alg_tag` is a constructor argument defaulting
#'   to `"EnsembleMethod"`, so matching any single message or tag would miss
#'   most failures. The stable part is `" error: "`.
#' * pest++/pestpp-glm write `"Error processing control file: "` or
#'   `"Model run failed.  No results were recorded."` and exit.
#'
#' Requiring whitespace on *both* sides of `error:` is what keeps this off
#' the options block PEST++ echoes at the top of every record, where option
#' names such as `panther_agent_restart_on_error: 0` embed the word with an
#' underscore before it.
#' @noRd
.pest_fatal_pattern <- paste(
  "[[:space:]]error:[[:space:]]",
  "Error processing control file",
  "Model run failed\\.",
  sep = "|"
)

#' Wait for the detached PANTHER master to finish, reporting progress.
#'
#' Polls for the exit-code sentinel, a fatal line in the record file, or the
#' overall `solver_timeout`. Watching only for success - as an earlier
#' version did - means a solver that aborts (for example
#' "all realizations failed during initial evaluation") leaves this polling
#' forever, because the marker it is waiting for will never be written.
#'
#' Each poll also reads the solver's per-iteration progress and reports it
#' through `AEME::cli_inform_safe()`, but only when it has changed, so the
#' user sees one line per iteration rather than a message every `poll`
#' seconds. `AEME.inform = FALSE` silences it like every other message in
#' the package.
#' @noRd
.pest_wait <- function(procs, wd, case, ctrl, poll = 5) {

  rec <- file.path(wd, sub("\\.pst$", ".rec", case))
  deadline <- Sys.time() + ctrl$solver_timeout
  master <- procs$master

  # Progress reporting. The solver appends a row to its phi CSV once per
  # iteration, so report only when that line actually changes rather than
  # on every poll - a 5-second heartbeat would produce thousands of
  # identical lines on a run that can last hours. One message per iteration
  # matches how report_generation() reports the built-in search.
  status <- NULL

  # Stall detection. The solver can finish every model run, terminate its
  # agents, and then fail to exit - PANTHER's shutdown has a race that
  # leaves the master spinning at 100% CPU with its output unflushed. That
  # is indistinguishable from "still working" if we only watch for the exit
  # sentinel, so a run that has stopped making progress must be capped on
  # its own, far shorter, clock than `solver_timeout`.
  stall_secs <- (ctrl$stall_minutes %||% 10) * 60
  last_sig <- NULL
  last_change <- Sys.time()

  repeat {
    Sys.sleep(poll)

    sig <- .pest_progress_sig(wd, case, ctrl)
    if (!identical(sig, last_sig)) {
      last_sig <- sig
      last_change <- Sys.time()
    }

    new_status <- .pest_status(wd, case, ctrl)
    if (!is.null(new_status) && !identical(new_status, status)) {
      status <- new_status
      # Values are pasted in rather than left as `{var}` for cli to
      # interpolate: cli_inform_safe() is a wrapper, so glue resolves in
      # *its* frame, where these locals do not exist. This matches how
      # every other cli_inform_safe() call in the package is written.
      AEME::cli_inform_safe(c("i" = paste0("{.val ", ctrl$exe, "}: ", status,
                                           " [", format(Sys.time()), "]")))
    }

    # The master process is authoritative: once it has exited, its exit
    # status says how it ended.
    if (!master$is_alive()) {
      st <- tryCatch(master$get_exit_status(), error = function(e) NA_integer_)
      if (isTRUE(st == 0L)) return(invisible(NULL))

      why <- .pest_fatal_reason(rec)
      cli::cli_abort(c(
        "{.val {ctrl$exe}} exited with status {.val {st %||% NA}}.",
        "x" = why %||% "No error message in the record file.",
        "i" = "Full record: {.file {rec}}"
      ))
    }

    # A solver can also report a fatal error and then sit rather than exit,
    # so keep checking the record while waiting for the sentinel.
    why <- .pest_fatal_reason(rec)
    if (!is.null(why)) {
      cli::cli_abort(c("{.val {ctrl$exe}} stopped with an error.",
                       "x" = why, "i" = "Full record: {.file {rec}}"))
    }

    if (as.numeric(difftime(Sys.time(), last_change, units = "secs")) >
        stall_secs) {
      n <- .pest_logged_runs(ctrl)
      mins <- round(stall_secs / 60)

      # If every run the configuration called for is already in the run
      # log, the solve is done in every sense that matters here and the
      # results are complete - salvage them rather than discarding a whole
      # calibration because the master would not exit.
      if (!is.na(ctrl$itermax) && n >= ctrl$itermax) {
        AEME::cli_safe(
          paste0("{.val ", ctrl$exe, "} completed all ", n,
                 " model runs but did not exit after ", mins,
                 " min. Continuing with the logged results."),
          FUN = cli::cli_alert_warning)
        return(invisible(NULL))
      }

      cli::cli_abort(c(
        "{.val {ctrl$exe}} stopped making progress
         ({.val {mins}} min without a completed run).",
        "x" = "{.val {n}} model run{?s} logged{if (is.na(ctrl$itermax)) '' else paste0(' of ', ctrl$itermax, ' expected')}.",
        "i" = "Raise {.arg stall_minutes} in {.fn create_pest_control} if the
               model legitimately takes longer, or check {.file {rec}}."
      ))
    }

    if (Sys.time() > deadline) {
      cli::cli_abort(c(
        "{.val {ctrl$exe}} did not finish within
         {.val {ctrl$solver_timeout}} seconds.",
        "i" = "Raise {.arg solver_timeout} in {.fn create_pest_control}, or
               check {.file {rec}}."
      ))
    }
  }
}

#' Number of model runs recorded in the run log so far.
#' @noRd
.pest_logged_runs <- function(ctrl) {
  logs <- list.files(file.path(ctrl$pest_dir, "runlog"),
                     pattern = "^runlog_.*[.]csv$", full.names = TRUE)
  if (length(logs) == 0) return(0L)
  sum(vapply(logs, function(f) {
    n <- tryCatch(length(readLines(f, warn = FALSE)), error = function(e) 1L)
    max(0L, n - 1L)   # drop the header
  }, integer(1)))
}

#' A cheap signature of "has anything happened", for stall detection.
#'
#' Combines the number of completed model runs with the size of the record
#' file, so either a new run finishing or the solver writing anything at
#' all counts as progress.
#' @noRd
.pest_progress_sig <- function(wd, case, ctrl) {
  rec <- file.path(wd, sub("[.]pst$", ".rec", case))
  paste(.pest_logged_runs(ctrl),
        if (file.exists(rec)) file.info(rec)$size else -1)
}

#' One-line description of how far the solver has got, or NULL if it has
#' not reported anything yet.
#'
#' Read from `<case>.phi.actual.csv`, which `L2PhiHandler::write()` appends
#' one row to per iteration with the documented header
#' `iteration,total_runs,mean,standard_deviation,min,max,<realisations...>`.
#' That is a far steadier contract than scraping the record file, and
#' `total_runs` is the solver's own count of model runs rather than a
#' guess from the run log.
#'
#' The file is being appended to by another process, so a read can land
#' mid-line; on any failure this returns NULL and the caller keeps showing
#' the previous status rather than flickering or erroring.
#' @noRd
.pest_status <- function(wd, case, ctrl) {

  f <- file.path(wd, paste0(sub("\\.pst$", "", case), ".phi.actual.csv"))
  if (!file.exists(f)) return(NULL)

  df <- tryCatch(
    utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL, warning = function(w) NULL
  )
  if (is.null(df) || nrow(df) == 0 ||
      !all(c("iteration", "total_runs", "min", "mean") %in% names(df))) {
    return(NULL)
  }

  last <- df[nrow(df), ]
  # noptmax <= 0 are the special PEST++ modes (single run, FOSM only, prior
  # ensemble only), where "iteration N of M" would be meaningless.
  iter <- if (ctrl$noptmax > 0) {
    paste0("iteration ", last$iteration, "/", ctrl$noptmax)
  } else {
    paste0("iteration ", last$iteration)
  }

  msg <- paste0(iter, " | ", last$total_runs, " model runs | best phi ",
                signif(last$min, 4), " (mean ", signif(last$mean, 4), ")")
  # The caller pastes this into a cli string, so strip any brace that a
  # malformed CSV could smuggle in and cli would try to evaluate as glue.
  gsub("[{}]", "", msg)
}

#' First fatal line in a record file, with its following context, or NULL
#' if there is none.
#'
#' PEST++ routinely puts the headline on one line and the detail on the
#' next - "the following '++' args were not accepted:" is useless without
#' the line naming them - so return the match plus the non-blank lines
#' immediately after it.
#' @noRd
.pest_fatal_reason <- function(rec, context = 3L) {
  if (!file.exists(rec)) return(NULL)
  txt <- readLines(rec, warn = FALSE)
  # `perl = TRUE`: R's default TRE engine intermittently fails to compile a
  # case-insensitive POSIX bracket class ("[[:space:]]") on Windows with
  # "pattern compilation error 'Out of memory'". PCRE compiles it reliably.
  i <- grep(.pest_fatal_pattern, txt, ignore.case = TRUE, perl = TRUE)[1]
  if (is.na(i)) return(NULL)

  out <- trimws(txt[i])
  for (j in seq_len(context)) {
    k <- i + j
    if (k > length(txt) || !nzchar(trimws(txt[k]))) break
    out <- c(out, trimws(txt[k]))
  }
  paste(out, collapse = " ")
}

#' Run the forward-run script once before handing over to the solver.
#'
#' PEST++ reports a broken interface only as "all realizations failed during
#' initial evaluation", after it has spent a full iteration discovering it,
#' and with nothing about the underlying cause. One run here turns that into
#' an immediate, actionable error, and costs a model evaluation the solver
#' would have made anyway.
#' @noRd
.pest_preflight <- function(ctrl, par_tbl, obs_tbl, tpl, ins) {

  AEME::cli_inform_safe(c("i" = "Checking the forward run before starting
                                 the solver."))

  # Stand in for PEST's template substitution at the initial values.
  utils::write.csv(data.frame(parnme = par_tbl$parnme, value = par_tbl$parval1),
                   file.path(ctrl$pest_dir, unname(tpl)), row.names = FALSE)

  out_file <- file.path(ctrl$pest_dir, unname(ins))
  rscript <- file.path(R.home("bin"), "Rscript")

  # One retry: a forward-run R process can die for reasons that have
  # nothing to do with the interface - a transient failure to spawn the
  # model, a momentary out-of-memory when the machine is loaded from a
  # previous configuration - and PANTHER would simply re-queue such a run.
  # A genuinely broken setup fails both times.
  run <- NULL
  for (attempt in 1:2) {
    unlink(out_file)
    run <- processx::run(rscript, "forward_run.R", wd = ctrl$pest_dir,
                         env = .pest_env(), error_on_status = FALSE,
                         echo = FALSE)
    if (file.exists(out_file)) break
    if (attempt == 1L) {
      AEME::cli_inform_safe(c("!" = "Pre-flight forward run wrote no output
                              (exit status {run$status}); retrying once."))
      Sys.sleep(2)
    }
  }
  log <- strsplit(paste0(run$stdout, run$stderr), "\r?\n")[[1]]

  fail <- function(why) {
    cli::cli_abort(c(
      "The forward run failed before PEST++ was started: {why}",
      "x" = paste(utils::tail(log, 12), collapse = " / "),
      "i" = "Forward-run R process exit status: {.val {run$status}}.",
      "i" = "Reproduce with {.code Rscript forward_run.R} in
             {.file {ctrl$pest_dir}}."
    ))
  }

  if (!file.exists(out_file)) fail("it wrote no output file.")
  got <- readLines(out_file, warn = FALSE)
  if (length(got) != nrow(obs_tbl)) {
    fail(paste0("it wrote ", length(got), " value(s), not ", nrow(obs_tbl),
                "."))
  }
  # A failed forward run now writes no file at all, and a partial one is
  # failed too, so the missing-file and wrong-length checks above already
  # cover it - nothing can arrive here as a vector of sentinels.

  # The preflight went through the same forward-run script as a real
  # evaluation, so it appended a row to the run log. Discard it: the
  # imported results should be exactly the runs the solver asked for, and
  # this one duplicates the "base" realisation at parval1 that pestpp-ies
  # includes in its ensemble anyway.
  unlink(file.path(ctrl$pest_dir, "runlog"), recursive = TRUE, force = TRUE)

  invisible(TRUE)
}
