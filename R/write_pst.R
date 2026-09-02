#' Build the PEST parameter table from an aemetools `param` dataframe
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Translates the aemetools parameter dataframe into the columns of the
#' `* parameter data` section of a PEST control file, and assigns each
#' parameter a short, safe PEST name.
#'
#' aemetools names parameters as `group/name[index]` (see
#' \code{\link{encode_param}}), which contains characters (`/`, `[`, `]`)
#' that collide with PEST template-file delimiters and with the fixed-width
#' name conventions of the older PEST utilities. Rather than escaping them,
#' each parameter is given a synthetic name `p001`, `p002`, ... and the
#' mapping back to `name_full` is returned as the `map` attribute so results
#' can be translated back afterwards.
#'
#' @param param dataframe; as passed to \code{\link{calib_aeme}}, requiring
#'   the columns `model`, `file`, `group`, `name`, `index`, `value`, `min`,
#'   `max` and optionally `log`.
#' @param transform Logical. Honour `param$log` by writing `partrans = "log"`
#'   for strictly-positive parameters? Default `TRUE`. Set `FALSE` for a
#'   sensitivity analysis, where every parameter must be sampled linearly on
#'   `[min, max]` so the indices are comparable with a built-in
#'   \code{\link{sa_aeme}} run.
#'
#' A parameter with `value == min == max` is written with
#' `partrans = "fixed"`: PEST keeps it in the control file - and hence in the
#' parameter map, the `pestpp-ies` ensembles and the sensitivity output - but
#' holds it at `value`. This is how a frozen earlier stage of a staged
#' calibration is carried through visibly instead of being baked into the
#' model configuration and dropped from every PEST table.
#'
#' @return A dataframe with columns `parnme`, `partrans`, `parchglim`,
#'   `parval1`, `parlbnd`, `parubnd`, `pargp`, `scale`, `offset`, `dercom`,
#'   carrying a `map` attribute (a dataframe of `parnme`/`name_full`/`model`).
#' @export
pest_param_table <- function(param, transform = TRUE) {

  if (!"name_full" %in% names(param)) {
    param$name_full <- encode_param(param$group, param$name, param$index)
  }

  # A parameter with value == min == max is held fixed: PEST keeps it in the
  # control file (and so in the parameter map, the ensemble CSVs, the
  # sensitivity output and pest_posterior_params()) but never perturbs it -
  # `partrans = "fixed"`. This is what carries a frozen earlier stage of a
  # staged calibration through visibly, rather than baking it into the model
  # config and dropping it from every PEST table.
  is_fixed <- !is.na(param$value) & !is.na(param$min) & !is.na(param$max) &
    param$value == param$min & param$value == param$max
  is_fixed[is.na(is_fixed)] <- FALSE

  viol <- (param$min >= param$max) & !is_fixed
  if (any(viol)) {
    bad <- param$name_full[viol]
    cli::cli_abort("PEST requires {.field min} < {.field max} for an adjustable
                   parameter; violated by {.val {bad}}.")
  }

  parnme <- sprintf("p%03d", seq_len(nrow(param)))

  # PEST can only log-transform a parameter whose whole feasible range is
  # strictly positive; silently fall back to no transform otherwise rather
  # than letting PEST abort on the control file. `transform = FALSE`
  # (sensitivity analysis) forces every parameter to linear sampling.
  log_req <- if (isTRUE(transform) && "log" %in% names(param)) {
    as.logical(param$log)
  } else {
    rep(FALSE, nrow(param))
  }
  log_req[is.na(log_req)] <- FALSE
  log_ok <- log_req & param$min > 0 & !is_fixed
  if (any(log_req & !log_ok & !is_fixed)) {
    AEME::cli_safe(
      paste0("Log transform dropped for parameters whose lower bound is not ",
             "positive: {.val ", paste(param$name_full[log_req & !log_ok],
                                       collapse = ", "), "}"),
      FUN = cli::cli_alert_warning
    )
  }

  # PARCHGLIM "factor" limits are undefined for a parameter that is zero or
  # whose range straddles zero, so use "relative" limits for those - and for
  # a fixed parameter, whose range is a single point.
  straddles <- param$min <= 0 & param$max >= 0
  parchglim <- ifelse(!is_fixed & (log_ok | !straddles), "factor", "relative")

  # PEST ignores the bounds of a fixed parameter but still parses them; keep
  # them equal to the value so the row is self-consistent.
  parlbnd <- ifelse(is_fixed, param$value, param$min)
  parubnd <- ifelse(is_fixed, param$value, param$max)

  tbl <- data.frame(
    parnme    = parnme,
    partrans  = ifelse(is_fixed, "fixed", ifelse(log_ok, "log", "none")),
    parchglim = parchglim,
    parval1   = param$value,
    parlbnd   = parlbnd,
    parubnd   = parubnd,
    # One PEST parameter group per aemetools group keeps the derivative
    # settings and the group-wise sensitivity output interpretable.
    pargp     = .pest_safe_name(ifelse(is.na(param$group), "misc", param$group)),
    scale     = 1,
    offset    = 0,
    dercom    = 1,
    stringsAsFactors = FALSE
  )

  attr(tbl, "map") <- data.frame(parnme = parnme, name_full = param$name_full,
                                 model = param$model, file = param$file,
                                 stringsAsFactors = FALSE)
  tbl
}

#' Build the PEST observation table from an AEME object
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams calib_aeme
#' @param obj_mode Character. `"residual"` writes one PEST observation per
#'   observed value; `"fit"` writes a single observation per variable whose
#'   simulated value is the `FUN_list` output and whose observed value is
#'   zero. See \code{\link{create_pest_control}}.
#' @param var_indices list; from `run_and_fit(return_indices = TRUE)`. When
#'   supplied, observations are restricted to the dates the model actually
#'   wrote. Otherwise the simulation window from `AEME::time()` is used.
#'   Observations the model cannot simulate must not become PEST
#'   observations: they carry weight, so they would contribute to phi while
#'   never being reproducible.
#' @param weight_method Character. `"balanced"` (default) scales each
#'   observation's weight by `sqrt(weights[v] / n_v) / sd_v`, so that each
#'   variable contributes roughly `weights[v]` to the initial objective
#'   function regardless of how many observations it has or what units it is
#'   in - the closest PEST analogue of the per-variable `weights` used by
#'   \code{\link{run_and_fit}}. `"unit"` uses `weights[v]` directly for every
#'   observation of that variable.
#'
#' @importFrom stats sd
#' @importFrom AEME observations
#'
#' @return A dataframe with columns `obsnme`, `obsval`, `weight`, `obgnme`,
#'   carrying a `map` attribute linking each `obsnme` back to its
#'   `Date`/`depth`/`var_aeme`.
#' @export
pest_obs_table <- function(aeme, vars_sim, weights, obj_mode = "residual",
                           weight_method = "balanced", var_indices = NULL) {

  obj_mode <- rlang::arg_match(obj_mode, c("residual", "fit"))
  weight_method <- rlang::arg_match(weight_method, c("balanced", "unit"))
  if (missing(weights)) weights <- set_weights(vars_sim = vars_sim)

  if (obj_mode == "fit") {
    # One "observation" per variable: the forward run reports the FUN_list
    # value and PEST drives it towards zero. Weights apply directly.
    tbl <- data.frame(
      obsnme = .pest_safe_name(vars_sim),
      obsval = 0,
      weight = as.numeric(weights[vars_sim]),
      obgnme = .pest_safe_name(vars_sim),
      stringsAsFactors = FALSE
    )
    attr(tbl, "map") <- data.frame(obsnme = tbl$obsnme, var_aeme = vars_sim,
                                   Date = as.Date(NA), depth = NA_real_,
                                   stringsAsFactors = FALSE)
    return(tbl)
  }

  # run_and_fit(return_df = TRUE) returns the gridded comparison built from
  # obs$lake and, when water level is requested, one row per observed lake
  # level (var_aeme "LKE_lvlwtr", depth NA) drawn from obs$level. The forward
  # run reports a simulated value for each, so LKE_lvlwtr is a first-class
  # residual-mode variable - as long as the lake carries observed level to
  # match against.
  want_lvl  <- "LKE_lvlwtr" %in% vars_sim
  grid_vars <- setdiff(vars_sim, "LKE_lvlwtr")

  obs <- AEME::observations(aeme)
  have_lake <- !is.null(obs$lake) && nrow(obs$lake) > 0
  have_lvl  <- !is.null(obs$level) && nrow(obs$level) > 0

  if ((length(grid_vars) > 0 && !have_lake) || (!have_lake && !have_lvl)) {
    cli::cli_abort("No lake observations found in {.arg aeme}.")
  }
  if (want_lvl && !have_lvl) {
    cli::cli_abort(c(
      "{.val LKE_lvlwtr} is in {.arg vars_sim} but
       {.code observations(aeme)$level} has no rows.",
      "i" = "Residual-mode water level is matched against observed lake
             level - supply it, drop {.val LKE_lvlwtr} from {.arg vars_sim},
             or use {.code obj_mode = \"fit\"}."
    ))
  }

  df <- data.frame(Date = as.Date(character()), depth = numeric(),
                   var_aeme = character(), value = numeric(),
                   stringsAsFactors = FALSE)
  if (have_lake) {
    obs$lake <- normalise_lake_obs(obs$lake)
    df <- obs$lake |>
      dplyr::filter(var_aeme %in% grid_vars, !is.na(value)) |>
      dplyr::select(Date, depth, var_aeme, value)
  }

  if (want_lvl) {
    # Same vertical datum as the modelled level the forward run reports (see
    # .wlev_obs_to_model_datum() / .raf_wlev()), and restricted to the
    # simulation window here because the var_indices filter below has no
    # entry for a non-gridded variable.
    tme  <- AEME::time(aeme)
    hyps <- AEME::input(aeme)$hypsograph
    lvl <- obs$level |>
      dplyr::filter(!is.na(value), Date >= as.Date(tme$start),
                    Date <= as.Date(tme$stop)) |>
      dplyr::mutate(depth = NA_real_, var_aeme = "LKE_lvlwtr",
                    value = .wlev_obs_to_model_datum(value, hyps)) |>
      dplyr::select(Date, depth, var_aeme, value)
    df <- dplyr::bind_rows(df, lvl)
  }

  # Keep only observations the model can actually produce a value for.
  #
  # An observation outside the simulation period still carries a non-zero
  # weight in the control file, so it contributes to phi - but the model
  # will never simulate it. Left in, the objective being minimised is
  # dominated by residuals against observations the model was never asked
  # to reproduce. In the AEME test lake that is 99 of 224.
  #
  # `var_indices` carries the dates the model actually wrote, which is the
  # exact answer; the simulation window is the fallback when it is absent.
  n_before <- nrow(df)
  # A failed index run hands back AEME's model-error object rather than a
  # per-variable list. Left unchecked it reaches the lookup below and dies
  # as "subscript out of bounds", which says nothing about what went wrong.
  if (!is.null(var_indices) && length(var_indices) > 0 &&
      (isTRUE(AEME::is_model_error(var_indices)) ||
       !all(vapply(var_indices, is.list, logical(1))))) {
    cli::cli_abort(c(
      "{.arg var_indices} does not describe the dates the model wrote.",
      "x" = "The run that produces them failed, so there is nothing to
             build a {.field .pst} against.",
      "i" = "Check the model runs at the initial parameter values before
             calibrating."
    ))
  }
  if (!is.null(var_indices) && length(var_indices) > 0) {
    keep <- vapply(seq_len(nrow(df)), function(i) {
      # Split the lookup: `var_indices` has no entry for a non-gridded
      # variable - LKE_lvlwtr is compared against the modelled surface, not
      # read off the output grid - and `NULL[["dates"]]` is a subscript
      # error rather than NULL, so the is.null() guard below never ran.
      vi <- var_indices[[df$var_aeme[i]]]
      d <- if (is.null(vi)) NULL else vi[["dates"]]
      is.null(d) || df$Date[i] %in% as.Date(d)
    }, logical(1))
    df <- df[keep, , drop = FALSE]
  } else {
    tme <- AEME::time(aeme)
    df <- df[df$Date >= as.Date(tme$start) & df$Date <= as.Date(tme$stop), ,
             drop = FALSE]
  }
  if (nrow(df) < n_before) {
    AEME::cli_safe(
      paste0("Excluded {.val ", n_before - nrow(df), "} of {.val ", n_before,
             "} observations the model does not simulate (outside the ",
             "simulation period)."),
      FUN = cli::cli_alert_info)
  }
  if (nrow(df) == 0) {
    cli::cli_abort(c(
      "No observations fall within the simulation period.",
      "i" = "Check {.code AEME::time(aeme)} against the observation dates."
    ))
  }

  missing_vars <- setdiff(vars_sim, unique(df$var_aeme))
  if (length(missing_vars) > 0) {
    cli::cli_abort("No observations for {.val {missing_vars}} within the
                   simulation period.")
  }

  df <- df |> dplyr::arrange(var_aeme, Date, depth)

  scal <- df |>
    dplyr::group_by(var_aeme) |>
    dplyr::summarise(n = dplyr::n(), s = stats::sd(value, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::mutate(
      # Guard against a zero/NA spread (a single observation, or a constant
      # series) collapsing every weight in the group to Inf.
      s = ifelse(!is.finite(s) | s <= 0, 1, s),
      w = if (weight_method == "balanced") {
        sqrt(as.numeric(weights[var_aeme]) / n) / s
      } else {
        as.numeric(weights[var_aeme])
      }
    )

  df <- dplyr::left_join(df, scal[, c("var_aeme", "w")], by = "var_aeme")
  obsnme <- sprintf("o%06d", seq_len(nrow(df)))

  tbl <- data.frame(
    obsnme = obsnme,
    obsval = df$value,
    weight = df$w,
    obgnme = .pest_safe_name(df$var_aeme),
    stringsAsFactors = FALSE
  )
  attr(tbl, "map") <- data.frame(obsnme = obsnme, var_aeme = df$var_aeme,
                                 Date = df$Date, depth = df$depth,
                                 stringsAsFactors = FALSE)
  tbl
}

#' Build the PEST observation table for a sensitivity analysis
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' One PEST "observation" per entry in `names(ctrl$vars_sim)` - i.e. per
#' sub-region that \code{\link{sa_aeme}} analyses separately (a
#' surface-temperature window, a bottom-temperature window, ...), not per
#' AEME variable. The forward run reports the corresponding `FUN_list`
#' value; `pestpp-sen` measures its sensitivity to the parameters. The
#' observed value is `0` and each observation is its own group, so the
#' Morris output is reported per sub-region.
#'
#' @param ctrl list; from \code{\link{create_sen_control}}.
#' @param weights named numeric; per-AEME-variable weights. Defaults to `1`
#'   for every variable.
#'
#' @return A dataframe with columns `obsnme`, `obsval`, `weight`, `obgnme`,
#'   carrying a `map` attribute of `obsnme` / sub-region `name` / `var_aeme`.
#' @seealso \code{\link{pest_obs_table}}, \code{\link{create_sen_control}}
#' @export
pest_sa_obs_table <- function(ctrl, weights) {

  nmes <- names(ctrl$vars_sim)
  if (is.null(nmes) || length(nmes) == 0) {
    cli::cli_abort("{.arg ctrl$vars_sim} must be a non-empty named list.")
  }
  vars <- vapply(ctrl$vars_sim, function(v) v$var, character(1))

  if (missing(weights) || is.null(weights)) {
    weights <- set_weights(vars_sim = unique(vars))
  }
  w <- as.numeric(weights[vars])
  w[!is.finite(w)] <- 1

  obsnme <- .pest_safe_name(nmes)
  if (anyDuplicated(obsnme)) {
    cli::cli_abort("Sub-region names collide once sanitised for PEST:
                   {.val {nmes}}.")
  }

  tbl <- data.frame(
    obsnme = obsnme,
    obsval = 0,
    weight = w,
    obgnme = obsnme,
    stringsAsFactors = FALSE
  )
  attr(tbl, "map") <- data.frame(obsnme = obsnme, name = nmes,
                                 var_aeme = unname(vars),
                                 Date = as.Date(NA), depth = NA_real_,
                                 stringsAsFactors = FALSE)
  tbl
}

#' Write a PEST control (`.pst`) file
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Writes a PEST/PEST++ control file from the parameter and observation
#' tables built by \code{\link{pest_param_table}} and
#' \code{\link{pest_obs_table}}. Section ordering and the fixed line
#' structure of the `* control data` block follow the PEST control-file
#' specification, which PEST++ reads unchanged.
#'
#' @param par_tbl dataframe; from \code{\link{pest_param_table}}.
#' @param obs_tbl dataframe; from \code{\link{pest_obs_table}}.
#' @param ctrl list; from \code{\link{create_pest_control}}.
#' @param tpl_files Named character vector; template files as
#'   `c("<template file>" = "<model input file>")`.
#' @param ins_files Named character vector; instruction files as
#'   `c("<instruction file>" = "<model output file>")`.
#' @param model_command Character. The command PEST++ runs for a single
#'   forward model evaluation, e.g.
#'   `"Rscript forward_run.R"`.
#' @param file Character. Path of the `.pst` file to write. Defaults to
#'   `<ctrl$pest_dir>/<ctrl$case>.pst`.
#' @param prior_info Character vector. Optional prior-information equations
#'   written verbatim to the `* prior information` section (Tikhonov
#'   regularisation).
#'
#' @return Invisibly, the path of the file written.
#' @seealso [create_pest_control()], [install_pest()]
#' @export
write_pst <- function(par_tbl, obs_tbl, ctrl, tpl_files, ins_files,
                      model_command, file = NULL, prior_info = character()) {

  file <- file %||% file.path(ctrl$pest_dir, paste0(ctrl$case, ".pst"))
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

  pargp <- unique(par_tbl$pargp)
  obgnme <- unique(obs_tbl$obgnme)

  # PESTMODE is "estimation" unless prior-information equations are present,
  # in which case the run is a regularised inversion.
  pestmode <- if (length(prior_info) > 0) "regularization" else "estimation"

  l <- c(
    "pcf",
    "* control data",
    paste("restart", pestmode),
    paste(nrow(par_tbl), nrow(obs_tbl), length(pargp), length(prior_info),
          length(obgnme)),
    paste(length(tpl_files), length(ins_files), "single point 1 0 0"),
    # RLAMBDA1 RLAMFAC PHIRATSUF PHIREDLAM NUMLAM
    "10.0 -3.0 0.3 0.03 10",
    # RELPARMAX FACPARMAX FACORIG
    "10.0 10.0 0.001",
    # PHIREDSWH
    "0.1",
    # NOPTMAX PHIREDSTP NPHISTP NPHINORED RELPARSTP NRELPAR
    paste(ctrl$noptmax, "0.005 4 4 0.005 4"),
    # ICOV ICOR IEIG
    "1 1 1",

    "* parameter groups",
    # PARGPNME INCTYP DERINC DERINCLB FORCEN DERINCMUL DERMTHD
    paste(pargp, "relative", ctrl$derinc, ctrl$derinc_lb, "switch 2.0",
          "parabolic"),

    "* parameter data",
    .pest_fmt_rows(par_tbl),

    "* observation groups",
    obgnme,

    "* observation data",
    .pest_fmt_rows(obs_tbl),

    "* model command line",
    model_command,

    "* model input/output",
    paste(names(tpl_files), unname(tpl_files)),
    paste(names(ins_files), unname(ins_files))
  )

  if (length(prior_info) > 0) {
    l <- c(l, "* prior information", prior_info)
  }

  l <- c(l, .pest_plusplus_lines(ctrl))

  writeLines(l, file)

  # Persist the synthetic-name mappings next to the control file so results
  # can be translated back to aemetools parameter/observation identities
  # without rebuilding the tables.
  if (!is.null(attr(par_tbl, "map"))) {
    utils::write.csv(attr(par_tbl, "map"),
                     file.path(dirname(file), paste0(ctrl$case, "_par_map.csv")),
                     row.names = FALSE)
  }
  if (!is.null(attr(obs_tbl, "map"))) {
    utils::write.csv(attr(obs_tbl, "map"),
                     file.path(dirname(file), paste0(ctrl$case, "_obs_map.csv")),
                     row.names = FALSE)
  }

  AEME::cli_safe(paste0("Wrote PEST control file {.file ", file, "} with ",
                        "{cli::qty(", nrow(par_tbl), ")}{.val ", nrow(par_tbl),
                        "} parameter{?s} and {cli::qty(", nrow(obs_tbl), ")}",
                        "{.val ", nrow(obs_tbl), "} observation{?s}."),
                 FUN = cli::cli_alert_success)
  invisible(file)
}

# Internal helpers -------------------------------------------------------

#' Coerce a string into a PEST-safe lowercase name.
#' PEST names are case-insensitive and must not contain whitespace or the
#' template/instruction delimiter characters.
#' @noRd
.pest_safe_name <- function(x) {
  x <- tolower(as.character(x))
  gsub("[^a-z0-9_]+", "_", x)
}

#' Format a dataframe as whitespace-delimited PEST section rows.
#' @noRd
.pest_fmt_rows <- function(df) {
  cols <- lapply(df, \(x) {
    if (is.numeric(x)) formatC(x, format = "g", digits = 10) else as.character(x)
  })
  do.call(paste, cols)
}

#' Build the trailing `++` option lines.
#' Solver-specific options are emitted first, then any user overrides from
#' `ctrl$pestpp_options`, so the user always wins on a duplicated key.
#' @noRd
.pest_plusplus_lines <- function(ctrl) {

  opts <- list()
  if (identical(ctrl$exe, "pestpp-ies")) {
    opts$ies_num_reals <- ctrl$ies_num_reals
    # A failed forward run writes no output file, so PEST++ marks the run
    # failed and drops that realisation itself - this no longer has to be
    # caught by a phi threshold. Kept as a backstop against a run that
    # succeeds but fits absurdly badly.
    opts$ies_bad_phi_sigma <- 2.0
    # Off by default, because with the noise off - which is what
    # pestpp-ies silently does unless an obs-noise-specific option is
    # supplied, see create_pest_control() - the observation ensemble is a
    # point mass at the observed values, so the conflict test flags every
    # observation the (narrow) prior simulated ensemble does not straddle.
    # Dropping those can leave nothing to fit at all, and pestpp-ies then
    # aborts with "all non-zero weighted observations in conflict state,
    # cannot continue" rather than running. Conflicts are still detected
    # and written to the pdc file, where pest_prior_data_conflict() reads
    # them; turn this on through `pestpp_options` once `noise_sd` makes
    # the conflict test meaningful.
    opts$ies_drop_conflicts <- "false"
    # The ensemble readers (read_pest_ensemble(), pest_posterior_runs(),
    # .pest_assign_gen()) parse the per-iteration CSVs. Binary ensembles are
    # still readable via .pest_read_ensemble_bin(), but keep CSV the default
    # so a run is inspectable without it.
    opts$ies_save_binary <- "false"
  }
  if (identical(ctrl$exe, "pestpp-sen")) {
    # Method of Morris. `noptmax` is ignored by pestpp-sen, so the design is
    # driven entirely by these. Per-observation-group sensitivities are
    # requested (one group per names(ctrl$vars_sim) sub-region) rather than a
    # single pooled figure.
    opts$gsa_method <- ctrl$sen_method %||% "morris"
    if (!is.null(ctrl$morris_r)) opts$gsa_morris_r <- ctrl$morris_r
    if (!is.null(ctrl$morris_p)) opts$gsa_morris_p <- ctrl$morris_p
    if (!is.null(ctrl$morris_delta)) opts$gsa_morris_delta <- ctrl$morris_delta
    opts$gsa_morris_pooled_obs <- "false"
    opts$gsa_morris_obs_sen <- "true"
  }
  # A prior covariance given as a file path is handed to PEST++ as `parcov`;
  # a matrix is instead turned into an explicit `ies_parameter_ensemble` by
  # .pest_setup_ensembles(), so it needs no option here.
  if (!is.null(ctrl$prior_cov) && is.character(ctrl$prior_cov)) {
    opts$parcov <- ctrl$prior_cov
  }
  if (is.finite(ctrl$timeout)) {
    opts$overdue_giveup_minutes <- ctrl$timeout / 60
  }
  opts$panther_agent_freeze_on_fail <- "false"

  # A forward run that fails does so because the model crashed or produced
  # no simulated equivalent for some observation - both deterministic in
  # the parameters, so retrying gains nothing and costs a model run.
  # PEST++ defaults to 3 attempts; one is enough here.
  opts$max_run_fail <- 1

  user <- ctrl$pestpp_options
  opts[names(user)] <- user

  if (length(opts) == 0) return(character())
  paste0("++", names(opts), "(",
         vapply(opts, \(v) {
           if (is.logical(v)) tolower(as.character(v)) else as.character(v)
         }, character(1)), ")")
}
