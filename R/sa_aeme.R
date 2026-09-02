#' Run sensitivity analysis on AEME model parameters
#'
#' @name sa_aeme
#' @description
#' `calib_model()` runs the model and compares it against observations provided.
#' It can run in parallel by using multiple cores availlable on your computer
#' to run quicker.
#'
#'
#' @inheritParams calib_aeme
#' @inheritParams AEME::build_aeme
#' @param FUN_list list of functions; named according to the variables in the
#'  `vars_sim`. Funtions are of the form `function(df)` which will be used
#'  to calculate model fit. If NULL, uses mean absolute error (MAE).
#'
#' @importFrom AEME lake get_aeme_path list_models check_aeme check_model
#' @importFrom AEME configuration get_lake_dir
#' @importFrom parallel stopCluster clusterExport parLapply detectCores
#' @importFrom parallel makeCluster
#' @importFrom utils write.csv write.table
#' @importFrom stats runif
#' @importFrom FME Latinhyper
#' @importFrom dplyr bind_rows mutate filter
#' @importFrom sensobol sobol_matrices
#'
#' @return string of simulation id to be used to read the simulation output.
#'
#' @examples
#' \dontrun{
#'   # Run sensitivity analysis
#'   tmpdir <- tempdir()
#'   aeme_dir <- system.file("extdata/lake/", package = "AEME")
#'   # Copy files from package into tempdir
#'   file.copy(aeme_dir, tmpdir, recursive = TRUE)
#'   path <- file.path(tmpdir, "lake")
#'   aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
#'   model_controls <- AEME::get_model_controls()
#'   inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
#'   outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
#'   model <- c("glm_aed")
#'   aeme <- AEME::build_aeme(path = path, aeme = aeme,
#'                                     model = model, model_controls = model_controls,
#'                                     inf_factor = inf_factor, ext_elev = 5,
#'                                     use_bgc = FALSE)
#'
#'   # Load parameters
#'   data("aeme_parameters", package = "AEME")
#'   param <- aeme_parameters |>
#'     dplyr::filter(file != "wdr")
#'
#'   # Function to calculate fitness
#'   fit <- function(df) {
#'     mean(df$model)
#'   }
#'
#'   # Assign function to variable
#'   FUN_list <- list(HYD_temp = fit)
#'
#'   # Set up control parameters for surface and bottom temperature
#'   ctrl <- create_control(method = "sa", N = 2^3, ncore = 2L,
#'                          parallel = TRUE,
#'                          vars_sim = list(
#'                                    surf_temp = list(var = "HYD_temp",
#'                                                     month = c(10:12, 1:3),
#'                                                     depth_range = c(0, 2)
#'                                                     ),
#'                                    bot_temp = list(var = "HYD_temp",
#'                                                    month = c(10:12, 1:3),
#'                                                    depth_range = c(10, 13)
#'                                                    )
#'                                    )
#'   )
#'
#'   # Run sensitivity analysis AEME model
#'   ctrl <- sa_aeme(aeme = aeme, path = path, param = param,
#'                   model = model, ctrl = ctrl, model_controls = model_controls,
#'                   FUN_list = FUN_list)
#' }
#'
#' @export

sa_aeme <- function(aeme, model, param, FUN_list, path,
                    model_controls = NULL, ctrl, param_df = NULL) {

  aeme <- AEME::check_aeme(aeme)
  if (missing(model)) {
    model <- AEME::list_models(aeme)
  } else {
    model <- AEME::check_model(model)
  }
  if (missing(path)) {
    path <- AEME::get_aeme_path(aeme = aeme)
  } else {
    path <- AEME::check_path(path = path)
  }
  if (missing(ctrl) || is.null(ctrl)) {
    stop("ctrl must be supplied")
  }
  if (is.null(model_controls)) {
    config <- AEME::configuration(aeme = aeme)
    model_controls <- config$model_controls
  }
  # Check if vars_sim and weights are the same length
  vars_sim <- sapply(ctrl$vars_sim, \(v) v$var) |>
    unique()
  weights <- set_weights(vars_sim = vars_sim)

  if (missing(FUN_list) || is.null(FUN_list)) {
    # Default to mean
    AEME::cli_safe("No FUN_list supplied, defaulting to mean function for
                        all variables.", FUN = cli::cli_alert_info)
    FUN_list <- list()
    for (v in vars_sim) {
      FUN_list[[v]] <- function(df) {
        mean(df$model)
      }
    }
  }

  if (!all(vars_sim %in% names(FUN_list)))
    cli::cli_abort("FUN_list must have names that match vars_sim")

  if (is.null(ctrl$na_value)) {
    ctrl$na_value <- 999
  }

  # Add a unique full name to each parameter ("group/name[index]") so
  # parameter columns - including indexed (vector) parameters - can be
  # matched back onto `param$value` unambiguously.
  param <- param |>
    dplyr::mutate(name_full = encode_param(group, name, index))

  # Check for parameters where value, min, max are equal - these can't be
  # perturbed, so drop them from the design and bake their fixed value in.
  eq_pars <- param[param$value == param$min & param$value == param$max, ]
  if (nrow(eq_pars) > 0) {
    AEME::cli_safe(paste0("The following parameters have the same value, ",
                          "min, and max and will not be updated during the ",
                          "sensitivity analysis: ",
                          "{.val ", paste(eq_pars$name, collapse = ", "), "}"),
                   FUN = cli::cli_alert_warning)
    AEME::input_model_parameters(aeme = aeme, model = model, param = eq_pars,
                                 path = path)

    param <- param |>
      dplyr::filter(!name_full %in% eq_pars$name_full)
  }

  include_wlev <- ifelse("LKE_lvlwtr" %in% vars_sim, TRUE, FALSE)
  lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)

  # Restrict each model's output to the SA target variables before any run
  # (also picked up by the PEST staging below). Default on; see
  # `?create_sa_control`.
  if (isTRUE(ctrl$trim_output)) {
    aeme <- apply_trim_output(aeme = aeme, model = model, vars_sim = vars_sim,
                              path = path)
  }

  # PEST++ owns the sampling design, the parallelism and the run history, so
  # it replaces the Sobol' matrix and the parallel evaluation loop below.
  if (identical(ctrl$engine, "pest")) {
    names(model) <- model
    return(sapply(model, \(m) {
      sa_aeme_pest(aeme = aeme, param = param, m = m, path = path,
                   lake_dir = lake_dir, vars_sim = vars_sim,
                   FUN_list = FUN_list, weights = weights,
                   model_controls = model_controls, ctrl = ctrl,
                   include_wlev = include_wlev)
    }))
  }

  names(model) <- model
  sapply(model, \(m) {
    # One model run at the initial parameters, so a broken setup fails now
    # rather than after a whole SA of NA responses. Default on; see
    # `?create_sa_control`.
    if (isTRUE(ctrl$preflight)) {
      calib_preflight(aeme = aeme, param = param, m = m, path = path,
                      vars_sim = vars_sim, FUN_list = FUN_list,
                      weights = weights, model_controls = model_controls,
                      ctrl = ctrl, include_wlev = include_wlev,
                      method = "sa", sa_ctrl = ctrl)
    }

    var_indices <- NULL
    if (any(vars_sim != "LKE_lvlwtr")) {
      # Extract indices for modelled variables
      AEME::cli_inform_safe(c("i" = paste0("Extracting variable indices for ",
                                           "{.val ", m, "} modelled variables ",
                                           "{.val ", paste(vars_sim, collapse = ", "),
                                           "}. [", format(Sys.time()), "]")))
      suppressMessages(
        var_indices <- run_and_fit(aeme = aeme, param = param,
                                   model = m, path = path, FUN_list = FUN_list,
                                   model_controls = model_controls,
                                   vars_sim = vars_sim,
                                   weights = weights,
                                   return_indices = TRUE,
                                   include_wlev = include_wlev,
                                   method = "sa", sa_ctrl = ctrl,
                                   fit = FALSE, timeout = ctrl$timeout)
      )
      AEME::cli_inform_safe(c("v" = paste0("Variable indices extracted for ",
                                           "{.val ", m, "}. [",
                                           format(Sys.time()), "]")))
    }

    # Extract parameters for the model ----
    param <- param[param$model == m, ]

    # Generate parameters for sensitivity analysis ----
    if (is.null(param_df)) {
      ## Create sample matrix to compute first and total-order indices:
      mat <- sensobol::sobol_matrices(N = ctrl$N, params = param$name_full)
      param_df <- mat
      for (i in 1:ncol(mat)) {
        param_df[, i] <- param$min[i] + (param$max[i] - param$min[i]) * mat[, i]
      }
      colnames(param_df) <- param$name_full
      param_df <- as.data.frame(param_df)
    }

    if (is.null(ctrl$ncore)) {
      ctrl$ncore <- parallel::detectCores() - 1
    }
    # Bound ncore by the number of available cores and parameter sets so
    # the cluster size and the split below use the same value.
    ctrl$ncore <- min(ctrl$ncore, (parallel::detectCores() - 1),
                      nrow(param_df))

    suppressWarnings({
      param_list <- split(param_df, 1:ctrl$ncore)
    })

    nmes <- names(ctrl$vars_sim)

    pr_df <- data.frame(rbind(signif(apply(param_df, 2, mean), 4),
                              signif(apply(param_df, 2, median), 4),
                              signif(apply(param_df, 2, sd), 4)),
                        row.names = c("mean", "median", "sd"))
    names(pr_df) <- gsub("NA/", "", names(param_df))
    if (isTRUE(getOption("AEME.inform", TRUE))) print(pr_df)

    # Set up parallel cluster (or serial paths) ----
    if (ctrl$parallel) {
      temp_dirs <- make_temp_dir(m, lake_dir, n = ctrl$ncore)
      paths <- temp_dirs
      AEME::cli_safe(paste0("Starting parallel sensitivity analysis for ",
                            "{.val ", m, "} using {.val ", ctrl$ncore,
                            "} cores with {.val ", nrow(param_df),
                            "} parameter sets. [", format(Sys.time()), "]"),
                     FUN = cli::cli_alert_info)
      unlink("parallel.log")
      cl <- aeme_make_cluster(ctrl$ncore)
      on.exit(parallel::stopCluster(cl))
      varlist <- list("param", "aeme", "paths", "m", "vars_sim", "FUN_list",
                      "model_controls", "var_indices", "ctrl", "weights",
                      "include_wlev", "nmes")
      parallel::clusterExport(cl, varlist = varlist,
                              envir = environment())
    } else {
      AEME::cli_safe(paste0("Starting serial sensitivity analysis for ",
                            "{.val ", m, "} with {.val ", nrow(param_df),
                            "} parameter sets. [", format(Sys.time()), "]"),
                     FUN = cli::cli_alert_info)
      paths <- rep(path, ctrl$ncore)
      cl <- NULL
    }

    # Evaluate every parameter set, dispatching to the cluster when running
    # in parallel and in-process (lapply) otherwise - the two paths share the
    # same per-chunk evaluation logic so they can't diverge ----
    worker_fun <- \(pars, i) {
      aemetools:::eval_param_chunk_sa(pars_i = pars[[i]], path = paths[i],
                                      aeme = aeme, param = param, model = m,
                                      vars_sim = vars_sim, FUN_list = FUN_list,
                                      model_controls = model_controls,
                                      ctrl = ctrl, var_indices = var_indices,
                                      weights = weights,
                                      include_wlev = include_wlev,
                                      nmes = nmes, parallel = ctrl$parallel)
    }
    model_out <- if (is.null(cl)) {
      lapply(seq_along(param_list), worker_fun, pars = param_list)
    } else {
      parallel::parLapply(cl, seq_along(param_list), worker_fun,
                          pars = param_list)
    }

    AEME::cli_safe(paste0(ifelse(ctrl$parallel, "Parallel", "Serial"),
                          " sensitivity analysis for {.val ", m,
                          "} completed. [", format(Sys.time()), "]"),
                   FUN = cli::cli_alert_success)

    g1 <- dplyr::bind_rows(model_out)
    out_df <- apply(g1, 2, signif, digits = 6)

    # Register "run_failed" as a response variable for this write only (not
    # in `ctrl$vars_sim` itself, which drives the model-output extraction in
    # run_and_fit() and must stay limited to real simulated variables). Its
    # name deliberately contains an underscore so write_simulation_output()'s
    # existing long-format pivot (matched on `contains("_")`) picks it up
    # with no schema changes to the (already long-format) simulation_data /
    # sensitivity_metadata tables - old databases just won't have any
    # "run_failed" rows, which read_sa() already handles gracefully.
    ctrl_out <- ctrl
    ctrl_out$vars_sim$run_failed <- list(var = "run_failed",
                                         month = NA_real_,
                                         depth_range = NA_real_)
    ctrl$sim_id <- write_simulation_output(x = out_df, ctrl = ctrl_out,
                                           FUN_list = FUN_list,
                                           aeme = aeme, model = m,
                                           param = param,
                                           append_metadata = TRUE)
    ctrl$sim_id
  })
}
