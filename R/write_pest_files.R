#' Write the PEST template (`.tpl`) file
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Writes a PEST template file for a single side-car parameter CSV, rather
#' than templating the model configuration files (`glm4.nml`, `gotm.yaml`,
#' `simstrat.par`, ...) directly.
#'
#' This is a deliberate choice. `AEME::input_model_parameters()` does not
#' simply substitute numbers into config files:
#'
#' * parameters whose `file` is `"met"`, `"inf"` or `"wdr"` are **multipliers
#'   applied to entire driver time series**, and rewrite the meteorology,
#'   inflow and outflow data files row by row. There is no single field in a
#'   text file for PEST to write into.
#' * config parameters are written via `read_nml()`/`set_nml()`/`write_nml()`,
#'   `yaml::write_yaml()` and `write_aed_param_csv()`, so the on-disk number
#'   formatting (precision, scientific notation, field width) is decided by
#'   those writers and could change between AEME versions - silently breaking
#'   any template built by matching on the previous formatting.
#' * `aed2_phyto_pars.nml` / `aed2_zoop_pars.nml` parameters are located by
#'   index into a group list read from `aed2.nml`, not by name.
#'
#' Templating one CSV that the forward run reads sidesteps all of this: PEST
#' owns a file whose format we control completely, and
#' \code{\link{pest_forward_run}} hands the values to the same
#' `input_model_parameters()` path that \code{\link{run_aeme_param}} already
#' uses, so there is no duplicated file-writing logic to drift.
#'
#' @param par_tbl dataframe; from \code{\link{pest_param_table}}.
#' @param ctrl list; from \code{\link{create_pest_control}}.
#' @param width Integer. Total width of each template field, including the
#'   two `~` delimiters. PEST writes the parameter value right-justified into
#'   this field, so the width caps the precision PEST can express: 10 is the
#'   documented minimum and 23 (the default here) is wide enough for a
#'   full-precision double, which matters for parameters whose plausible
#'   range spans orders of magnitude.
#'
#' @return Invisibly, a length-one named character vector
#'   `c("<template file>" = "<model input file>")`, ready to pass to
#'   \code{\link{write_pst}} as `tpl_files`.
#' @seealso [write_pest_ins()], [write_pest_forward_run()]
#' @export
write_pest_tpl <- function(par_tbl, ctrl, width = 23L) {

  if (width < 10L) {
    cli::cli_abort("{.arg width} must be at least 10; PEST cannot write a
                   value into a narrower field.")
  }

  tpl <- file.path(ctrl$pest_dir, paste0(ctrl$case, "_pars.csv.tpl"))
  csv <- paste0(ctrl$case, "_pars.csv")
  dir.create(ctrl$pest_dir, recursive = TRUE, showWarnings = FALSE)

  # "ptf ~" declares the parameter-template delimiter. Each field is
  # ~<padded name>~ occupying exactly `width` characters; PEST overwrites
  # the whole field, delimiters included.
  inner <- width - 2L
  if (any(nchar(par_tbl$parnme) > inner)) {
    cli::cli_abort("{.arg width} too small for parameter names.")
  }
  fields <- formatC(par_tbl$parnme, width = inner, flag = " ")

  writeLines(c(
    "ptf ~",
    "parnme,value",
    paste0(par_tbl$parnme, ",~", fields, "~")
  ), tpl)

  AEME::cli_safe(paste0("Wrote PEST template {.file ", tpl, "}"),
                 FUN = cli::cli_alert_success)

  stats::setNames(csv, basename(tpl))
}

#' Write the PEST instruction (`.ins`) file
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Describes how PEST reads simulated values back out of the file written by
#' \code{\link{pest_forward_run}}. That file has one whitespace-delimited
#' `<obsnme> <value>` line per observation, in the exact order of `obs_tbl`,
#' so each instruction line is `l1 w !obsnme!`: advance one line, skip the
#' name field, read the value.
#'
#' @param obs_tbl dataframe; from \code{\link{pest_obs_table}}.
#' @param ctrl list; from \code{\link{create_pest_control}}.
#'
#' @return Invisibly, a length-one named character vector
#'   `c("<instruction file>" = "<model output file>")`, ready to pass to
#'   \code{\link{write_pst}} as `ins_files`.
#' @seealso [write_pest_tpl()], [pest_forward_run()]
#' @export
write_pest_ins <- function(obs_tbl, ctrl) {

  ins <- file.path(ctrl$pest_dir, paste0(ctrl$case, "_sim.ins"))
  out <- paste0(ctrl$case, "_sim.out")
  dir.create(ctrl$pest_dir, recursive = TRUE, showWarnings = FALSE)

  writeLines(c("pif ~", paste0("l1 w !", obs_tbl$obsnme, "!")), ins)

  AEME::cli_safe(paste0("Wrote PEST instruction file {.file ", ins, "}"),
                 FUN = cli::cli_alert_success)

  stats::setNames(out, basename(ins))
}

#' Write the PEST forward-run script and its payload
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Writes the script PEST++ invokes for a single model evaluation, plus the
#' `.rds` payload it needs. The script itself is a three-line stub that calls
#' \code{\link{pest_forward_run}}; all of the logic lives in the package
#' where it can be tested and versioned, rather than in generated text.
#'
#' @inheritParams calib_aeme
#' @param par_tbl,obs_tbl dataframes; from \code{\link{pest_param_table}} and
#'   \code{\link{pest_obs_table}}, carrying their `map` attributes.
#' @param ctrl list; from \code{\link{create_pest_control}}.
#' @param var_indices list; from `run_and_fit(return_indices = TRUE)`. Cached
#'   so that every forward run skips re-deriving the netCDF date/depth
#'   indices, exactly as \code{\link{calib_aeme}} does for its own workers.
#' @param include_wlev Logical. Include water level in the fit.
#'
#' @return Invisibly, the path of the generated script.
#' @seealso [pest_forward_run()]
#' @export
write_pest_forward_run <- function(aeme, param, par_tbl, obs_tbl, model,
                                   vars_sim, FUN_list, weights, ctrl,
                                   model_controls = NULL, var_indices = list(),
                                   include_wlev = FALSE) {

  dir.create(ctrl$pest_dir, recursive = TRUE, showWarnings = FALSE)
  script <- file.path(ctrl$pest_dir, "forward_run.R")
  payload <- file.path(ctrl$pest_dir, paste0(ctrl$case, "_fwd.rds"))

  # For a sensitivity run the forward run routes through
  # run_and_fit(method = "sa"), which needs the sub-region definitions, and
  # the run log carries one column per names(ctrl$vars_sim) rather than per
  # AEME variable. Only `vars_sim` (the `vars_sim` element, reached by
  # partial matching in run_and_fit) is needed from the control.
  is_sa <- identical(ctrl$obj_mode, "sa")

  saveRDS(list(
    aeme = aeme, param = param, model = model, vars_sim = vars_sim,
    FUN_list = FUN_list, weights = weights, model_controls = model_controls,
    var_indices = var_indices, include_wlev = include_wlev,
    par_map = attr(par_tbl, "map"), obs_map = attr(obs_tbl, "map"),
    obsnme = obs_tbl$obsnme, obj_mode = ctrl$obj_mode,
    sa_ctrl = if (is_sa) ctrl["vars_sim"] else NULL,
    fit_names = if (is_sa) names(ctrl$vars_sim) else NULL,
    na_value = ctrl$na_value, timeout = ctrl$timeout, case = ctrl$case,
    # Absolute, so that PANTHER agents running in their own subdirectories
    # all append to the same run-log directory.
    log_dir = normalizePath(file.path(ctrl$pest_dir, "runlog"),
                            winslash = "/", mustWork = FALSE)
  ), payload)

  # Under pkgload (devtools::test(), load_all()) there may be no installed
  # aemetools for the forward run to library(), or an older one without
  # these functions - either way every model run would fail. Point the stub
  # at the same source tree the calling session is using.
  dev <- requireNamespace("pkgload", quietly = TRUE) &&
    isTRUE(try(pkgload::is_dev_package("aemetools"), silent = TRUE))
  load_call <- if (dev) {
    sprintf("suppressMessages(pkgload::load_all(%s, quiet = TRUE))",
            deparse1(pkgload::pkg_path()))
  } else {
    "suppressMessages(library(aemetools))"
  }

  # PANTHER copies the whole working directory to each agent, so the stub
  # must resolve everything relative to its own working directory and must
  # not assume the library path of the master process.
  writeLines(c(
    "# Generated by aemetools::write_pest_forward_run() - do not edit.",
    "# PEST++ runs this once per model evaluation, in its own directory.",
    # deparse1(), not deparse(): deparse() breaks its output into multiple
    # elements at width.cutoff, which sprintf() then vectorises over into
    # several syntactically broken lines.
    sprintf(".libPaths(%s)", deparse1(.libPaths())),
    load_call,
    sprintf("pest_forward_run(payload = \"%s_fwd.rds\")", ctrl$case)
  ), script)

  # A stub that does not parse produces no output file, which PEST++ reports
  # only as "all realizations failed" after it has burned a whole iteration.
  # Catch it here instead.
  tryCatch(parse(script), error = function(e) {
    cli::cli_abort(c("Generated forward-run script does not parse.",
                     "x" = conditionMessage(e)))
  })

  AEME::cli_safe(paste0("Wrote forward-run script {.file ", script, "}"),
                 FUN = cli::cli_alert_success)
  invisible(script)
}
