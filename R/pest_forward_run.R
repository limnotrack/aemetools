#' Run one AEME forward model evaluation for PEST++
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The worker behind the `forward_run.R` stub written by
#' \code{\link{write_pest_forward_run}}. PEST++ calls it once per model
#' evaluation, in the agent's own working directory:
#'
#' 1. read the parameter CSV that PEST wrote from the template,
#' 2. splice those values into the aemetools `param` dataframe,
#' 3. run the model and extract simulated equivalents via
#'    \code{\link{run_and_fit}} - the same code path the built-in
#'    calibration methods use, so the two cannot diverge,
#' 4. write one `<obsnme> <value>` line per observation, in the order the
#'    instruction file expects.
#'
#' A run that fails - the model crashed or timed out, or produced no
#' simulated equivalent for some observation - writes **no output file at
#' all**, and that is deliberate. PEST++ has its own mechanism for this:
#' PANTHER marks the run failed, retries it up to `max_run_fail` times, and
#' `EnsembleMethod` then drops that realisation from the parameter and
#' observation ensembles. Verified against pestpp-ies 5.2.16: of six
#' realisations with one crashing, the prior observation ensemble came back
#' with five and the run completed normally.
#'
#' Writing a penalty value instead - as this used to - defeats that. PEST
#' cannot tell a sentinel from a simulated value, so `999` does not mean
#' "this run failed", it means "the model simulated 999 degrees here". The
#' realisation then survives into the ensemble statistics and the empirical
#' covariance that computes the parameter update, until some `ies_bad_phi`
#' threshold happens to catch it - and both of those thresholds are
#' disabled by default.
#'
#' The run is still recorded in the run log with `na_value`, so it reaches
#' the results database as `NA` and stays visible as a failure, exactly as
#' a failed run does under the built-in methods.
#'
#' @param payload Character. Path to the `.rds` written by
#'   \code{\link{write_pest_forward_run}}.
#' @param par_file,out_file Character. Override the parameter input and
#'   simulated output file names. Default to `<case>_pars.csv` and
#'   `<case>_sim.out` in the working directory.
#' @param path Character. Directory holding the AEME lake directory for this
#'   agent. Defaults to the working directory.
#'
#' @importFrom utils read.csv
#' @importFrom dplyr left_join mutate
#'
#' @return Invisibly, the dataframe of `obsnme`/`sim` values written.
#' @seealso [write_pest_forward_run()], [write_pest_ins()]
#' @export
pest_forward_run <- function(payload, par_file = NULL, out_file = NULL,
                             path = ".") {

  p <- readRDS(payload)
  par_file <- par_file %||% paste0(p$case, "_pars.csv")
  out_file <- out_file %||% paste0(p$case, "_sim.out")

  # A failure returns NULL rather than a vector of sentinels, so that no
  # output file is written and PEST++ can apply its own failed-run handling
  # (see @description).
  sim <- tryCatch({

    pars <- utils::read.csv(par_file, stringsAsFactors = FALSE)
    pars$parnme <- trimws(pars$parnme)

    param <- .pest_apply_params(param = p$param, pars = pars,
                                par_map = p$par_map)

    if (identical(p$obj_mode, "sa")) {
      .pest_run_sa(p = p, param = param, path = path)
    } else if (identical(p$obj_mode, "fit")) {
      .pest_run_fit(p = p, param = param, path = path)
    } else {
      .pest_run_residual(p = p, param = param, path = path)
    }
  }, error = function(e) {
    # Not cli_abort: this must not stop the script before the run is logged.
    message("aemetools forward run failed: ", conditionMessage(e))
    NULL
  })

  fits <- attr(sim, "fits")

  # A stale file from a previous evaluation in this directory would be read
  # as though it were this run's result, so clear it either way.
  unlink(out_file)

  if (is.null(sim) || anyNA(sim) || !all(is.finite(sim))) {
    message("aemetools forward run produced no usable output; ",
            "leaving no result file so PEST++ treats this run as failed.")
    .pest_log_run(p = p, par_file = par_file, fits = NULL)
    return(invisible(NULL))
  }

  df <- data.frame(obsnme = p$obsnme, sim = as.numeric(sim),
                   stringsAsFactors = FALSE)
  writeLines(paste(df$obsnme, formatC(df$sim, format = "g", digits = 10)),
             out_file)

  .pest_log_run(p = p, par_file = par_file, fits = fits)

  invisible(df)
}

#' Append this evaluation to the run log.
#'
#' PEST++'s own output files differ between solvers and have changed names
#' between releases, so rather than parsing them for the run history,
#' every forward run records its own parameter values and fit components.
#' The log is what \code{\link{read_pest_results}} imports into the results
#' database, which means the recorded `fit` is computed by the same
#' `FUN_list` the built-in methods use and is directly comparable with a
#' CMAES or MOEDA run of the same setup.
#'
#' Each process writes its own file keyed on PID, so concurrent PANTHER
#' agents never contend for a single file and no locking is needed.
#' @noRd
.pest_log_run <- function(p, par_file, fits) {

  if (is.null(p$log_dir) || !nzchar(p$log_dir)) return(invisible(NULL))
  dir.create(p$log_dir, recursive = TRUE, showWarnings = FALSE)

  pars <- utils::read.csv(par_file, stringsAsFactors = FALSE)
  vals <- stats::setNames(as.list(pars$value), trimws(pars$parnme))
  names(vals) <- p$par_map$name_full[match(names(vals), p$par_map$parnme)]

  # A sensitivity run logs one column per names(ctrl$vars_sim) sub-region;
  # every other mode logs one per AEME variable.
  log_vars <- p$fit_names %||% p$vars_sim
  if (is.null(fits)) {
    fits <- stats::setNames(rep(p$na_value, length(log_vars)), log_vars)
  }
  total <- if (any(!is.finite(unlist(fits))) ||
               any(unlist(fits) == p$na_value)) {
    p$na_value
  } else {
    sum(unlist(fits))
  }

  row <- as.data.frame(c(vals, as.list(fits),
                         list(fit = total, time = format(Sys.time()))),
                       check.names = FALSE)

  f <- file.path(p$log_dir, paste0("runlog_", Sys.getpid(), ".csv"))
  utils::write.table(row, f, sep = ",", row.names = FALSE,
                     col.names = !file.exists(f), append = file.exists(f),
                     qmethod = "double")
  invisible(f)
}

# Internal helpers -------------------------------------------------------

#' Splice PEST's parameter values into the aemetools `param` dataframe.
#'
#' `param$name_full` is the join key rather than row position, so a
#' reordering of either table cannot silently misassign values. Any
#' parameter PEST did not supply keeps its initial value.
#' @noRd
.pest_apply_params <- function(param, pars, par_map) {

  if (!"name_full" %in% names(param)) {
    param$name_full <- encode_param(param$group, param$name, param$index)
  }

  vals <- dplyr::left_join(par_map, pars, by = "parnme")
  if (anyNA(vals$value)) {
    stop("PEST parameter file is missing values for: ",
         paste(vals$parnme[is.na(vals$value)], collapse = ", "))
  }

  idx <- match(param$name_full, vals$name_full)
  param$value[!is.na(idx)] <- vals$value[idx[!is.na(idx)]]
  param
}

#' Residual mode: one simulated value per observation.
#' @noRd
.pest_run_residual <- function(p, param, path) {

  comp <- run_and_fit(aeme = p$aeme, param = param, model = p$model,
                      vars_sim = p$vars_sim, path = path,
                      model_controls = p$model_controls,
                      FUN_list = p$FUN_list, weights = p$weights,
                      na_value = p$na_value, var_indices = p$var_indices,
                      include_wlev = p$include_wlev, return_df = TRUE,
                      method = "calib", timeout = p$timeout)

  # run_and_fit() returns its na_value list rather than a dataframe when the
  # run failed, the netCDF could not be opened, or no observations overlap.
  if (!is.data.frame(comp)) return(NULL)

  # run_and_fit() fills these in for LKE_lvlwtr when include_wlev is set;
  # mirror that here so the run-log fit component does not depend on the
  # caller having added an explicit entry for it.
  if (isTRUE(p$include_wlev) && "LKE_lvlwtr" %in% p$vars_sim) {
    if (!"LKE_lvlwtr" %in% names(p$FUN_list))
      p$FUN_list[["LKE_lvlwtr"]] <- p$FUN_list[[1]]
    if (!"LKE_lvlwtr" %in% names(p$weights))
      p$weights[["LKE_lvlwtr"]] <- 1
  }

  # PEST minimises weighted SSR over the residuals, but the run log records
  # the FUN_list value so that a PEST run and a CMAES run of the same setup
  # are directly comparable in the results database.
  fits <- lapply(stats::setNames(p$vars_sim, p$vars_sim), \(v) {
    sub <- comp[comp$var_aeme == v, ]
    if (nrow(sub) == 0) return(p$na_value)
    p$FUN_list[[v]](sub) * p$weights[[v]]
  })

  # Depth is a computed midpoint on both sides, so join on a rounded copy
  # rather than trusting exact floating-point equality.
  key <- function(d) {
    paste(d$var_aeme, as.character(as.Date(d$Date)),
          ifelse(is.na(d$depth), "NA", formatC(round(d$depth, 6), format = "f",
                                               digits = 6)))
  }
  hit <- match(key(p$obs_map), key(comp))

  # A partial run - the model produced nothing for some observations, having
  # dried out or stopped early - is failed too, rather than padded with a
  # penalty value. The instruction file demands a value for every
  # observation, so there is no way to say "missing" in the output file:
  # anything written there is read as a simulated value. Padding would
  # either flatter the fit (writing the observed value) or corrupt the
  # ensemble statistics (writing a sentinel), and both are silent. Failing
  # the run hands it to PEST++'s own handling, which drops the realisation.
  if (anyNA(hit)) {
    message(sum(is.na(hit)), " of ", length(hit),
            " observations had no simulated equivalent; failing this run.")
    return(NULL)
  }

  structure(comp$model[hit], fits = fits)
}

#' Fit mode: one simulated value per variable, being the FUN_list output
#' that PEST drives towards the zero "observation".
#' @noRd
.pest_run_fit <- function(p, param, path) {

  fit <- run_and_fit(aeme = p$aeme, param = param, model = p$model,
                     vars_sim = p$vars_sim, path = path,
                     model_controls = p$model_controls,
                     FUN_list = p$FUN_list, weights = p$weights,
                     na_value = p$na_value, var_indices = p$var_indices,
                     include_wlev = p$include_wlev, return_df = FALSE,
                     method = "calib", timeout = p$timeout)

  vals <- unlist(fit[p$obs_map$var_aeme])
  vals[is.na(vals)] <- p$na_value
  structure(unname(vals), fits = fit[p$vars_sim])
}

#' Sensitivity mode: one simulated value per `names(sa_ctrl$vars_sim)`
#' sub-region - the `FUN_list` output that `pestpp-sen` measures the
#' parameter sensitivity of. Mirrors `.pest_run_fit()` but routes through
#' `run_and_fit(method = "sa")` so each sub-region's month/depth window is
#' applied, exactly as \code{\link{sa_aeme}}'s own workers do.
#' @noRd
.pest_run_sa <- function(p, param, path) {

  fit <- run_and_fit(aeme = p$aeme, param = param, model = p$model,
                     vars_sim = p$vars_sim, path = path,
                     model_controls = p$model_controls,
                     FUN_list = p$FUN_list, weights = p$weights,
                     na_value = p$na_value, var_indices = p$var_indices,
                     include_wlev = p$include_wlev, return_df = FALSE,
                     method = "sa", sa_ctrl = p$sa_ctrl, timeout = p$timeout)

  nm <- p$fit_names
  vals <- unlist(fit[nm])
  vals[is.na(vals)] <- p$na_value
  structure(unname(vals), fits = fit[nm])
}
