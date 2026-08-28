#' Create control list for PEST++ calibration
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Builds the control object used when calibrating with an external PEST++
#' engine instead of the built-in `"CMAES"`/`"LHC"`/`"MOEDA"` methods. The
#' returned object is a `calib_sa_control` like those from
#' \code{\link{create_calib_control}}, so the existing result-writing and
#' plotting functions keep working; the extra fields describe how the
#' `.pst` control file is written and how the solver is invoked.
#'
#' @inheritParams create_calib_control
#'
#' @param exe Character. Which PEST++ solver to run. One of `"pestpp-ies"`
#'   (iterative ensemble smoother; the recommended default for AEME, since it
#'   tolerates noisy model responses and returns a posterior parameter
#'   ensemble), `"pestpp-glm"` (Gauss-Levenberg-Marquardt), `"pestpp-sen"`
#'   (Morris/Sobol global sensitivity) or `"pestpp-swp"` (parallel sweep of a
#'   supplied parameter table).
#' @param obj_mode Character. How the objective function is expressed in the
#'   `.pst` file.
#'   * `"residual"` (default) writes one PEST observation per
#'     observed value, so PEST++ minimises the usual weighted sum of squared
#'     residuals. This is the idiomatic PEST formulation and is what enables
#'     regularisation, FOSM uncertainty and per-observation diagnostics -
#'     but it **ignores `FUN_list`**.
#'   * `"fit"` writes one PEST observation per variable in `vars_sim`, whose
#'     "simulated" value is the output of the corresponding `FUN_list`
#'     function and whose observed value is `0`. This preserves your existing
#'     NSE/MAE objectives exactly, at the cost of losing residual-level
#'     diagnostics and making the finite-difference Jacobian much less
#'     informative (so pair it with `"pestpp-ies"`, not `"pestpp-glm"`).
#' @param noptmax Integer. Maximum number of solver iterations. Special
#'   values follow the PEST++ convention: `0` runs the model once at the
#'   initial parameter values, `-1` computes a Jacobian and FOSM uncertainty
#'   without updating parameters, `-2` runs the prior parameter ensemble
#'   only. Default `6`, a common starting point for `pestpp-ies`.
#' @param ies_num_reals Integer. Ensemble size for `"pestpp-ies"`. Default
#'   `50`. Unlike the built-in methods, run count scales with this rather
#'   than with the number of parameters.
#' @param pest_dir Character. Working directory for the generated `.pst`,
#'   `.tpl`, `.ins` and forward-run files. Defaults to `"pest"` under the
#'   lake directory. This directory is created and, if `overwrite = TRUE`,
#'   cleared on each run.
#' @param case Character. Basename for the generated files, so the control
#'   file is `<case>.pst`. Default `"aeme"`.
#' @param parallel Logical. Run the model evaluations in parallel? Default
#'   `TRUE`. PEST++ parallelises only through a run manager, so this selects
#'   the PANTHER manager (a master plus `ncore` agents) rather than a serial
#'   run in which the master evaluates the model itself. aemetools does
#'   **not** create its own `parallel::makeCluster()` for a PEST++ run;
#'   `ncore` is the number of PANTHER agents.
#' @param port Integer. TCP port for the PANTHER master. Default `4004`.
#' @param stall_minutes Numeric. Abort if the solver goes this many minutes
#'   without completing a model run or writing to its record file. Default
#'   `10`. This is the practical guard: PEST++ can finish every run,
#'   terminate its agents and then fail to exit, spinning at 100% CPU with
#'   its output unflushed, which `solver_timeout` alone would not catch for
#'   a further day. If every expected run is already logged the results are
#'   kept and the run continues; otherwise it aborts. Raise this if a single
#'   model evaluation legitimately takes longer than 10 minutes.
#' @param solver_timeout Numeric. Maximum seconds to wait for the solver to
#'   finish before aborting. Default `86400` (24 hours). This is a backstop
#'   against a detached PANTHER master that neither completes nor reports an
#'   error; it is not a per-model-run limit, which is `timeout`.
#' @param derinc,derinc_lb Numeric. Finite-difference increment (as a
#'   fraction of the parameter value) and its absolute lower bound, used by
#'   `"pestpp-glm"` and `"pestpp-sen"`. Defaults `0.01` and `1e-6`.
#' @param prior_cov Optional matrix or character. A prior parameter
#'   covariance matrix (from \code{\link{pest_prior_cov}}) or the path to a
#'   PEST `.cov`/`.unc` file. A matrix is used to draw a correlated Gaussian
#'   prior ensemble in R (implies `prior_dist = "normal"`); a path is passed
#'   to `pestpp-ies` as `++parcov(...)`. When `NULL` (the default) the prior
#'   is independent uniforms on the bounds.
#' @param prior_dist Character. Shape of the prior parameter ensemble when it
#'   is generated in R: `"uniform"` (default, and the only case where the
#'   ensemble is left for `pestpp-ies` to draw), `"normal"` (Gaussian about
#'   the initial values) or `"triangular"`. See
#'   \code{\link{pest_prior_ensemble}}.
#' @param prior_par_ensemble Logical, character or `NULL`. Controls the
#'   `++ies_parameter_ensemble`. `NULL` (default) generates one in R only
#'   when `seed`, `prior_cov` or a non-uniform `prior_dist` / `noise_sd`
#'   makes it necessary, and otherwise leaves `pestpp-ies` to draw its own.
#'   `TRUE`/`FALSE` force or suppress generation; a path uses that CSV
#'   as-is.
#' @param seed Integer or `NULL`. Random seed for the prior parameter and
#'   observation ensembles, for reproducible runs. Setting it triggers
#'   in-R generation of the parameter ensemble.
#' @param noise_sd Named numeric vector or `NULL`. Measurement standard
#'   deviation per `vars_sim` variable, in data units, e.g.
#'   `c(HYD_temp = 0.5)`. When given, an explicit observation (noise)
#'   ensemble is written and passed as `++ies_observation_ensemble`, so the
#'   noise is decoupled from the balanced PEST weights. See
#'   \code{\link{pest_obs_ensemble}}.
#' @param noise_method Character. How `noise_sd` is applied. `"ensemble"`
#'   (default) draws an explicit observation ensemble in R and passes it as
#'   `++ies_observation_ensemble`, so realisations align with a generated
#'   parameter ensemble. `"standard_deviation"` instead writes the
#'   per-variable standard deviations as a diagonal observation-noise
#'   covariance (`++obscov`) and lets `pestpp-ies` draw the noise itself.
#'   `"none"` ignores `noise_sd`.
#' @param ies_no_noise Logical. Emit `++ies_no_noise(true)`, so `pestpp-ies`
#'   fits the raw observed values with no measurement-noise ensemble.
#'   Default `FALSE`.
#' @param include_base Logical. Keep the initial parameter values as a
#'   `base` realisation in the ensemble (`++ies_include_base`). Default
#'   `TRUE`, matching `pestpp-ies`.
#' @param restart_from Optional. A previous `pestpp-ies` run to resume from -
#'   a run directory, a \code{\link{read_calib}} object, or a
#'   \code{\link{create_pest_control}} object. Its final parameter and
#'   simulated-observation ensembles are handed to the new run as
#'   `++ies_parameter_ensemble` and `++ies_restart_observation_ensemble`, so
#'   iteration 0 is not re-evaluated. The parameter and observation sets must
#'   be unchanged from that run. Takes precedence over `prior_par_ensemble`.
#' @param localizer Optional. A `pestpp-ies` localizer, declaring which
#'   parameters each variable's observations may update. Accepts anything
#'   \code{\link{as_param_var_matrix}} takes - most conveniently a list keyed
#'   by variable, e.g.
#'   `list(HYD_temp = c("light", "mixing"), CHM_oxy = "sediment")` - or the
#'   path to a localizer file already in PEST matrix format, which is passed
#'   through unchanged. A variable the specification does not mention is left
#'   linked to every parameter. `NULL` (the default) runs without
#'   localization. Only `"pestpp-ies"` localizes; see
#'   \code{\link{pest_localizer}} for what the entries mean and for the
#'   cost of a localized upgrade.
#' @param pestpp_options Named list. Additional `++` options written verbatim
#'   to the control file, e.g.
#'   `list(ies_bad_phi_sigma = 2.0, ies_autoadaloc = TRUE)`. Names are
#'   emitted as `++name(value)`, and override the defaults aemetools sets.
#'
#'   `ies_drop_conflicts` is one worth knowing about. It is **off** by
#'   default: `pestpp-ies` runs with no observation noise unless an
#'   obs-noise-specific option is given, and against noiseless observations
#'   the prior-data-conflict test flags nearly everything - dropping which
#'   can leave nothing to fit, aborting the run. Conflicts are still
#'   detected and readable with \code{\link{pest_prior_data_conflict}}. Turn
#'   it on once `noise_sd` makes the test meaningful.
#' @param overwrite Logical. Clear `pest_dir` before writing? Default `TRUE`.
#' @param keep_files Logical. Retain the PEST working directory after the run
#'   completes, for inspection or restart. Default `TRUE`.
#' @param trim_output Logical. Before staging the model for PEST++, restrict
#'   each model's written output to `vars_sim` via
#'   \code{\link[AEME]{set_output_vars}} (and switch off GLM's
#'   `mass_balance.csv`). Default `TRUE`. Rewrites the model configuration in
#'   the lake directory; the trimmed config is what gets copied to the PANTHER
#'   agent directories.
#'
#' @param ... Must be empty. Additional arguments are not allowed.
#'
#' @return A `calib_sa_control` list with `method = "pest"`.
#' @seealso [install_pest()], [write_pst()], [calib_aeme()]
#' @export
#'
#' @examples
#' ctrl <- create_pest_control(exe = "pestpp-ies", noptmax = 6,
#'                             ies_num_reals = 50, ncore = 4)
#' ctrl
create_pest_control <- function(
    file_type = "db",
    file_name = NULL,
    file_dir = "calib_sa",
    na_value = 999,
    parallel = TRUE,
    ncore = parallel::detectCores() - 1,
    timeout = Inf,
    exe = "pestpp-ies",
    obj_mode = "residual",
    noptmax = 6,
    ies_num_reals = 50,
    pest_dir = "pest",
    case = "aeme",
    port = 4004,
    solver_timeout = 24 * 3600,
    stall_minutes = 10,
    derinc = 0.01,
    derinc_lb = 1e-6,
    prior_cov = NULL,
    prior_dist = "uniform",
    prior_par_ensemble = NULL,
    seed = NULL,
    noise_sd = NULL,
    noise_method = "ensemble",
    ies_no_noise = FALSE,
    include_base = TRUE,
    restart_from = NULL,
    localizer = NULL,
    pestpp_options = list(),
    overwrite = TRUE,
    keep_files = TRUE,
    trim_output = TRUE,
    ...
) {

  rlang::check_dots_used()
  exe <- rlang::arg_match(exe, c("pestpp-ies", "pestpp-glm", "pestpp-sen",
                                 "pestpp-swp"))
  obj_mode <- rlang::arg_match(obj_mode, c("residual", "fit"))
  prior_dist <- rlang::arg_match(prior_dist,
                                 c("uniform", "normal", "triangular"))
  noise_method <- rlang::arg_match(noise_method,
                                   c("ensemble", "standard_deviation", "none"))

  if (!is.null(seed)) {
    seed <- suppressWarnings(as.integer(seed))
    if (is.na(seed)) cli::cli_abort("{.arg seed} must be a whole number.")
  }
  if (!is.null(noise_sd) &&
      (!is.numeric(noise_sd) || is.null(names(noise_sd)))) {
    cli::cli_abort("{.arg noise_sd} must be a named numeric vector keyed by
                   variable, e.g. {.code c(HYD_temp = 0.5)}.")
  }
  if (is.matrix(prior_cov) && identical(prior_dist, "uniform")) {
    prior_dist <- "normal"
    AEME::cli_safe("{.arg prior_cov} is a matrix; setting
                   {.code prior_dist = \"normal\"}.",
                   FUN = cli::cli_alert_info)
  }

  if (!is.null(localizer)) {
    # Only pestpp-ies localizes; the others would ignore the file silently.
    if (!identical(exe, "pestpp-ies")) {
      cli::cli_abort(c(
        "{.arg localizer} applies to {.val pestpp-ies} only, not
         {.val {exe}}.",
        "i" = "Drop {.arg localizer}, or set {.code exe = \"pestpp-ies\"}."
      ))
    }
    if (!is.data.frame(localizer) && !is.matrix(localizer) &&
        !is.list(localizer) &&
        !(is.character(localizer) && length(localizer) == 1)) {
      cli::cli_abort("{.arg localizer} must be a dataframe, a named list, a
                     matrix or a single file path.")
    }
    # The parameter and observation tables do not exist yet, so the
    # specification is only checked for shape here; it is resolved against
    # them in calib_aeme_pest().
  }

  if (!is.list(pestpp_options) ||
      (length(pestpp_options) > 0 && is.null(names(pestpp_options)))) {
    cli::cli_abort("{.arg pestpp_options} must be a named list.")
  }
  .pest_check_options(pestpp_options)

  # Turning conflict-dropping back on without giving pestpp-ies any
  # observation noise is the combination that aborts a run outright.
  #
  # pestpp-ies does not fall back to drawing noise from the weights: with
  # no obs-noise-specific option it reports "no obs-noise-specific options
  # have been passed, resetting to `ies_no_noise` to true" in the record
  # file and builds an observation ensemble with no spread at all. Every
  # observation is then a point mass, so the prior-data-conflict test flags
  # any the prior simulated ensemble does not straddle - routinely all of
  # them - and dropping those leaves nothing to fit: "all non-zero weighted
  # observations in conflict state, cannot continue".
  if (identical(exe, "pestpp-ies") &&
      .pest_opt_true(pestpp_options$ies_drop_conflicts) &&
      (is.null(noise_sd) || identical(noise_method, "none") ||
       isTRUE(ies_no_noise))) {
    AEME::cli_safe(c(
      "!" = "{.code ies_drop_conflicts} is on but this run has no
             observation noise.",
      "i" = "{.val pestpp-ies} does not draw noise from the weights - it
             switches noise off entirely - so the conflict test compares
             the simulated ensemble against the observed values alone and
             will flag nearly everything.",
      "i" = "Set {.arg noise_sd} (e.g. {.code c(HYD_temp = 0.5)}), or leave
             {.code ies_drop_conflicts} off and inspect conflicts with
             {.fn pest_prior_data_conflict}."
    ), FUN = cli::cli_bullets)
  }

  # A finite-difference Jacobian over a single aggregated fit value per
  # variable gives GLM almost nothing to work with, and the aggregation is
  # rarely smooth in the parameters. Steer users to the ensemble solver.
  if (identical(obj_mode, "fit") && identical(exe, "pestpp-glm")) {
    cli::cli_abort(c(
      "{.code obj_mode = \"fit\"} is not compatible with {.val pestpp-glm}.",
      "i" = "Use {.val pestpp-ies}, or {.code obj_mode = \"residual\"} to give
             the gradient solver per-observation residuals."
    ))
  }

  ctrl <- .create_control(
    # A PEST++ run is still a calibration: keeping `method = "calib"` is
    # what lets read_simulation_output() load calibration_metadata,
    # write_simulation_output() stem the sim_id as "C" rather than "S", and
    # the plot_calib_*() family apply unchanged. The solver is recorded in
    # `engine` and `c_method`.
    method = "calib",
    engine = "pest",
    file_type = file_type,
    file_name = file_name,
    file_dir = file_dir,
    na_value = na_value,
    parallel = parallel,
    ncore = ncore,
    timeout = timeout,
    c_method = toupper(exe),
    exe = exe,
    obj_mode = obj_mode,
    noptmax = noptmax,
    ies_num_reals = ies_num_reals,
    pest_dir = pest_dir,
    case = case,
    port = port,
    solver_timeout = solver_timeout,
    stall_minutes = stall_minutes,
    derinc = derinc,
    derinc_lb = derinc_lb,
    prior_cov = prior_cov,
    prior_dist = prior_dist,
    prior_par_ensemble = prior_par_ensemble,
    seed = seed,
    noise_sd = noise_sd,
    noise_method = noise_method,
    ies_no_noise = ies_no_noise,
    include_base = include_base,
    restart_from = restart_from,
    localizer = localizer,
    pestpp_options = pestpp_options,
    overwrite = overwrite,
    keep_files = keep_files,
    trim_output = trim_output
  )

  # Same meaning as `itermax` in create_calib_control(): the model-run
  # budget, not the iteration count (`noptmax` is the iteration count).
  # Derived rather than supplied, because with PEST++ the budget is a
  # consequence of the solver settings rather than something you can cap.
  ctrl$itermax <- .pest_expected_runs(ctrl)
  ctrl
}

#' Expected number of forward model runs for a PEST++ control
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' How many times the model will actually be run, so the cost of a
#' configuration is visible before it is started. Unlike the built-in
#' methods - where `itermax` is a budget you set and `ngen` follows from it
#' - a PEST++ run's cost falls out of the solver settings, so this is
#' computed rather than chosen.
#'
#' For `"pestpp-ies"` the figure is exact, and follows the accounting in
#' `EnsembleMethod::solve()`:
#'
#' * the prior ensemble costs `ies_num_reals` runs. `ies_include_base`
#'   does not add to that - `add_bases()` drops the last realisation before
#'   appending the `base` one - so the ensemble is always `ies_num_reals`.
#' * each iteration then tests every lambda/scale-factor combination on a
#'   *subset* of realisations, costing
#'   `n_lambda x n_scale x subset_size` runs, and applies the winning
#'   combination to the rest, costing `ies_num_reals - subset_size`.
#'
#' A negative `ies_subset_size` (the default, `-10`) is a percentage of the
#' ensemble, floored at 4 realisations. That floor is why very small
#' ensembles are inefficient: at `ies_num_reals = 6` the subset is 4 of 6,
#' so lambda testing costs more runs than there are realisations.
#'
#' For `"pestpp-glm"` the cost is driven by the finite-difference Jacobian,
#' `n_par + 1` runs per iteration, and `n_par` is only known once the
#' parameters are, so `n_par` must be supplied. The result is a lower
#' bound: each iteration may add up to `NUMLAM` further upgrade runs.
#'
#' For `"pestpp-sen"` with the Method of Morris the figure is exact once
#' `n_par` is known: `(n_par + 1) * gsa_morris_r` trajectories. `"pestpp-swp"`
#' sweeps a user-supplied table the control knows nothing about, so returns
#' `NA`.
#'
#' @param ctrl list; from \code{\link{create_pest_control}} or
#'   \code{\link{create_sen_control}}.
#' @param n_par Integer. Number of adjustable parameters. Needed for
#'   `"pestpp-glm"` and `"pestpp-sen"`.
#'
#' @return Numeric; expected model runs, or `NA` when it cannot be
#'   determined from the control alone.
#' @export
#'
#' @examples
#' # Defaults: 50 prior runs, then 6 iterations of (1 x 3 x 5) + (50 - 5).
#' pest_expected_runs(create_pest_control(ncore = 1))
pest_expected_runs <- function(ctrl, n_par = NULL) {
  .pest_expected_runs(ctrl = ctrl, n_par = n_par)
}

#' Option names that are easy to get wrong, mapped to what PEST++ accepts.
#'
#' PEST++ parses `++` options with `forgive_unknown_args` false by default,
#' so a single bad name aborts the whole run - after the control file has
#' been written and the solver launched. Worse, the name it prints when
#' rejecting one is its *internal member* name, which for the lambda
#' multipliers (`ies_lam_mults`) differs from the keyword the parser
#' actually accepts (`ies_lambda_mults`), sending you back to the same
#' wrong name. Catching these at control-creation costs nothing.
#' @noRd
.pest_option_aliases <- c(
  ies_lam_mults = "ies_lambda_mults",
  ies_subset = "ies_subset_size",
  ies_num_real = "ies_num_reals",
  lambda_scale_factor = "lambda_scale_fac",
  ies_bad_phi_sig = "ies_bad_phi_sigma"
)

#' Is a `++` option value truthy?
#'
#' PEST++ writes these as `true`/`false`, but a user setting one through
#' `pestpp_options` is as likely to reach for an R logical.
#' @noRd
.pest_opt_true <- function(x) {
  if (is.null(x) || length(x) != 1) return(FALSE)
  if (is.na(x)) return(FALSE)
  if (is.logical(x)) return(isTRUE(x))
  isTRUE(tolower(trimws(as.character(x))) %in% c("true", "t", "yes", "1"))
}

#' @noRd
.pest_check_options <- function(opts) {
  if (length(opts) == 0) return(invisible(NULL))
  bad <- intersect(names(opts), names(.pest_option_aliases))
  if (length(bad) > 0) {
    fix <- unname(.pest_option_aliases[bad])
    cli::cli_abort(c(
      "PEST++ does not accept {cli::qty(length(bad))}{?this/these} option
       name{?s}: {.val {bad}}.",
      "i" = "Use {.val {fix}} instead.",
      "x" = "PEST++ aborts the run on an unrecognised {.code ++} option
             rather than ignoring it."
    ))
  }
  invisible(NULL)
}

#' @noRd
.pest_expected_runs <- function(ctrl, n_par = NULL) {

  # noptmax 0 is a single run at the initial values; negative values are
  # the FOSM / prior-ensemble-only modes, whose cost is not a simple
  # function of the control.
  if (isTRUE(ctrl$noptmax == 0)) return(1)
  if (isTRUE(ctrl$noptmax < 0)) return(NA_real_)

  opts <- ctrl$pestpp_options %||% list()

  if (identical(ctrl$exe, "pestpp-ies")) {
    n <- ctrl$ies_num_reals
    subset <- opts$ies_subset_size %||% -10
    if (subset < 0) subset <- max(4, floor(n * (-subset) / 100))
    subset <- min(subset, n)
    # The control-file keyword is `ies_lambda_mults`. PEST++'s internal
    # member is `ies_lam_mults`, and that shorter name is what it prints
    # back when rejecting an unknown option, so it is easy to use by
    # mistake - see .pest_check_options().
    #
    # PEST++ defaults it to {0.1, 1, 10} (three multipliers), not to the
    # single 1.0 that EnsembleMethod::solve() falls back to only when the
    # option has been explicitly cleared. Assuming one understated the
    # budget threefold.
    n_lam <- length(opts$ies_lambda_mults %||% c(0.1, 1.0, 10.0))
    n_scale <- length(opts$lambda_scale_fac %||% c(0.75, 1.0, 1.1))
    per_iter <- n_lam * n_scale * subset + (n - subset)
    return(n + ctrl$noptmax * per_iter)
  }

  if (identical(ctrl$exe, "pestpp-glm")) {
    if (is.null(n_par)) return(NA_real_)
    return(ctrl$noptmax * (n_par + 1))
  }

  if (identical(ctrl$exe, "pestpp-sen")) {
    if (is.null(n_par)) return(NA_real_)
    method <- opts$gsa_method %||% ctrl$sen_method %||% "morris"
    if (identical(method, "sobol")) {
      n <- opts$gsa_sobol_samples %||% ctrl$gsa_sobol_samples %||% 500
      return((n_par + 2) * n)
    }
    # Method of Morris: r trajectories, each of n_par + 1 model runs.
    r <- opts$gsa_morris_r %||% ctrl$morris_r %||% 4
    return((n_par + 1) * r)
  }

  NA_real_
}
