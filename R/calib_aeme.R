#' Calibrate AEME model parameters using observations
#'
#' @name calib_aeme
#' @description
#' `calib_aeme()` runs the model and compares it against observations provided.
#' It can run in parallel by using multiple cores available on your computer
#' to run quicker.
#'
#'
#' @inheritParams AEME::build_aeme
#' @param param dataframe; of parameters read in from a csv file. Requires the
#' columns c("model", "file", "group", "name", "index", "value", "min", "max", "log")
#' @param vars_sim vector; of variables names to be used in the calculation of
#' model fit.
#' @param FUN_list list of functions; named according to the variables in the
#'  `vars_sim`. Funtions are of the form `function(df)` which will be used
#'  to calculate model fit. If nor provided, uses mean absolute error (MAE).
#' @param ctrl list; of controls for sensitivity analysis function created using
#'  the \code{\link{create_control}} function. See \link{create_control} for
#'  more details.
#' @param weights a named vector; of weights for each variable in vars_sim. If not
#' provided, defaults to 1 for each variable.
#' @param param_var_matrix a declaration of which parameters are associated
#' with which response variables. Either the dataframe
#' \code{\link{create_param_var_matrix}} returns - parameters as rows,
#' response variables as logical columns - or the shorthand list keyed by
#' variable, e.g. `list(HYD_temp = c("light", "mixing"), CHM_oxy =
#' "sediment")`; see \code{\link{as_param_var_matrix}} for every accepted
#' form and for how unmentioned variables and parameters are treated.
#'
#' How it is used depends on the engine. With `ctrl$c_method = "MOEDA"` it
#' decides which parameters are resampled together in each generation, and
#' the two are mutually required. With `ctrl$engine = "pest"` and
#' `exe = "pestpp-ies"` it becomes the solver's localizer (see
#' \code{\link{pest_localizer}}); it is equivalent to, and takes the place
#' of, `localizer` in \code{\link{create_pest_control}}. Any parameter it
#' associates with no variable at all is dropped from the calibration, with
#' a warning.
#' @param param_df dataframe; of parameters to be used in the calibration.
#' Requires the columns c("model", "file", "name", "value", "min", "max"). This
#' is used to restart from a previous calibration.
#'
#' @importFrom parallel stopCluster clusterExport parLapply makeCluster
#' @importFrom parallel detectCores
#' @importFrom utils write.csv write.table
#' @importFrom stats runif
#' @importFrom FME Latinhyper
#' @importFrom dplyr mutate bind_rows
#' @importFrom AEME check_aeme check_model check_path get_lake_dir observations
#' @importFrom AEME set_vars_sim get_aeme_path configuration
#'
#' @return string of simulation id to be used to read the simulation output.
#' 
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' model_controls <- AEME::get_model_controls()
#' model <- c("glm_aed", "gotm_wet")
#' path <- "aeme"
#' aeme <- AEME::build_aeme(aeme = aeme, model = model, path = path,
#'                          model_controls = model_controls, ext_elev = 5) |>
#'   AEME::run_aeme()
#' 
#' data("aeme_parameters", package = "AEME")
#' param <- aeme_parameters
#' 
#' # Function to calculate fitness (nse_loss = -1 * NSE, minimised by calib_aeme)
#' FUN_list <- list(HYD_temp = nse_loss, LKE_lvlwtr = nse_loss)
#' 
#' ctrl <- create_control(method = "calib", NP = 10, itermax = 20, ncore = 2,
#'                        parallel = TRUE, file_type = "db",
#'                        file_name = "results.db")
#' 
#' vars_sim <- c("HYD_temp", "LKE_lvlwtr")
#' weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 1)
#' 
#' # Calibrate AEME model
#' sim_id <- calib_aeme(aeme = aeme, model = model, path = path,
#'                      param = param, FUN_list = FUN_list, ctrl = ctrl,
#'                      vars_sim = vars_sim, weights = weights)
#'
#' @export

calib_aeme <- function(aeme, model, param, path, vars_sim = "HYD_temp", FUN_list, 
                       weights, model_controls = NULL, ctrl = NULL,
                       param_var_matrix = NULL, param_df = NULL) {
  
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

  if (missing(FUN_list)) {
    message(strwrap("No FUN_list supplied. Defaulting to mean absolute error for all
            variables."))
    # Default to mean absolute error
    fit_fun <- function(df) {
      mean(abs(df$obs - df$model), na.rm = TRUE)
    }
    FUN_list <- list()
    for (v in vars_sim) {
      FUN_list[[v]] <- fit_fun
    }
  }
  if (missing(weights)) {
    AEME::cli_inform_safe("No weights supplied. Defaulting to 1 for all variables.")
    weights <- set_weights(vars_sim = vars_sim)
  }
  # Check if vars_sim are in weights names
  if (!all(vars_sim %in% names(weights))){
    missing_weights <- setdiff(vars_sim, names(weights))
    cli::cli_abort("The following variables in vars_sim are not in weights: 
                   {.val {missing_weights}}")
  }

  if (!all(vars_sim %in% names(FUN_list))) {
    missing_FUN <- setdiff(vars_sim, names(FUN_list))
    cli::cli_abort("The following variables in vars_sim are not in FUN_list: 
                   {.val {missing_FUN}}")
  }

  if (is.null(model_controls)) {
    config <- AEME::configuration(aeme = aeme)
    model_controls <- config$model_controls
  }
  
  # Ensure all the target variables are switched on in model_controls
  model_controls <- AEME::set_vars_sim(model_controls = model_controls, 
                                       vars_sim = vars_sim, simulate = TRUE,
                                       exclusive = TRUE)

  if (is.null(ctrl)) {
    ctrl <- create_control(method = "calib", NP = NA, itermax = 200)
  }
  if (is.null(ctrl$na_value)) {
    ctrl$na_value <- 999
  }

  # param_var_matrix has two consumers, and which one applies depends on the
  # engine: MOEDA's Pareto-front resampling in next_gen_params(), and
  # pestpp-ies's localizer. Every other method has nowhere to put it.
  is_pest <- identical(ctrl$engine, "pest")
  if (!is.null(param_var_matrix) && is_pest &&
      !identical(ctrl$exe, "pestpp-ies")) {
    cli::cli_abort(c(
      "{.arg param_var_matrix} becomes a localizer, which only
       {.val pestpp-ies} uses - not {.val {ctrl$exe}}.",
      "i" = "Express the structure through observation groups and weights
             instead, or set {.code exe = \"pestpp-ies\"}."
    ))
  }

  # For the built-in engines, param_var_matrix and c_method = "MOEDA" are
  # mutually required: MOEDA's Pareto-front selection needs
  # param_var_matrix to know the objective structure, and
  # param_var_matrix's joint-covariance resampling in next_gen_params() is
  # only honoured when c_method is "MOEDA".
  if (!is.null(param_var_matrix) && !is_pest &&
      !identical(ctrl$c_method, "MOEDA")) {
    cli::cli_abort("{.arg param_var_matrix} requires {.code ctrl$c_method} to
                   be {.val MOEDA}, not {.val {ctrl$c_method}}.")
  }
  if (is.null(param_var_matrix) && identical(ctrl$c_method, "MOEDA")) {
    cli::cli_abort("{.code c_method = \"MOEDA\"} requires {.arg param_var_matrix}
                   to be supplied.")
  }

  # Add index to parameter name
  param <- param |> 
    dplyr::mutate(name_full = encode_param(group, name, index))
  
  # Check for parameters where value, min, max are equal.
  #
  # For the built-in engines these are written to the model config and then
  # dropped - the search has nowhere to record a held parameter. A PEST run
  # does: pest_param_table() emits them with `partrans = "fixed"`, so they
  # stay in the .pst, the parameter map, the pestpp-ies ensembles and the
  # sensitivity output. Keep them in `param` for that path.
  eq_pars <- param[param$value == param$min & param$value == param$max, ]
  if (nrow(eq_pars) > 0) {
    verb <- if (is_pest) {
      "held fixed (partrans = fixed) and reported in the calibration output"
    } else {
      "not be updated during calibration"
    }
    AEME::cli_safe(paste0("The following parameters have equal value, min and ",
                          "max and will ", verb, ": {.val ",
                          paste(eq_pars$name, collapse = ", "), "}"),
                   FUN = cli::cli_alert_warning)
    AEME::input_model_parameters(aeme = aeme, model = model, param = eq_pars,
                                 path = path)

    if (!is_pest) {
      param <- param |>
        dplyr::filter(!name_full %in% eq_pars$name_full)
    }
  }
  fixed_nf <- if (is_pest) eq_pars$name_full else character(0)
  
  # Accept the shorthand forms (a list keyed by variable, a logical matrix)
  # as well as the canonical dataframe; everything downstream sees the
  # dataframe.
  param_var_matrix <- as_param_var_matrix(param_var_matrix, param = param,
                                          vars_sim = vars_sim)

  if (!is.null(param_var_matrix)) {
    # Check all variables have parameters
    for (v in vars_sim) {
      sel_param <- param_var_matrix[["name_full"]][param_var_matrix[[v]]]
      if (length(sel_param) == 0) {
        cli::cli_abort("No parameters associated with variable {.val {v}} in param_var_matrix.")
      }
    }
    
    # Select logical columnms
    mat <- param_var_matrix |>
      dplyr::select(dplyr::all_of(vars_sim))
    # A fixed parameter is deliberately linked to nothing; keep it so
    # pest_param_table() can still emit it as `partrans = fixed`.
    rem_pars <- setdiff(param$name_full,
                        param_var_matrix$name_full[apply(mat, 1, any)])
    rem_pars <- setdiff(rem_pars, fixed_nf)
    if (length(rem_pars) > 0) {
      AEME::cli_safe(paste0("The following parameters are not associated with ",
                            "any of the response variables and will not be ",
                            "updated during calibration: ",
                            "{.val ", paste(rem_pars, collapse = ", "), "}"),
                     FUN = cli::cli_alert_warning)
      param <- param |>
        dplyr::filter(!name_full %in% rem_pars)
    }
  }

  include_wlev <- ifelse("LKE_lvlwtr" %in% vars_sim, TRUE, FALSE)

  lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)

  # Restrict each model's output to the calibration targets before any run
  # (also picked up by the PEST staging below). Default on; see
  # `?create_calib_control`.
  if (isTRUE(ctrl$trim_output)) {
    aeme <- apply_trim_output(aeme = aeme, model = model, vars_sim = vars_sim,
                              path = path)
  }

  names(model) <- model

  # PEST++ owns the search loop, the parallelism and the run history, so it
  # replaces the generation loop below rather than plugging into it.
  if (identical(ctrl$engine, "pest")) {
    # `param_var_matrix` and `create_pest_control(localizer = )` are the same
    # declaration reached two ways; a localizer set on the control wins,
    # because it was stated closer to the solver it configures.
    if (!is.null(param_var_matrix)) {
      if (is.null(ctrl$localizer)) {
        ctrl$localizer <- param_var_matrix
      } else {
        AEME::cli_safe(
          "Both {.arg param_var_matrix} and {.code ctrl$localizer} were
           supplied; using {.code ctrl$localizer}.",
          FUN = cli::cli_alert_warning)
      }
    }
    return(sapply(model, \(m) {
      calib_aeme_pest(aeme = aeme, param = param, m = m, path = path,
                      lake_dir = lake_dir, vars_sim = vars_sim,
                      FUN_list = FUN_list, weights = weights,
                      model_controls = model_controls, ctrl = ctrl,
                      include_wlev = include_wlev)
    }))
  }

  sapply(model, \(m) {
    var_indices <- list()
    t0 <- Sys.time() # Time check for calibration
    nsim <- 0 # Counter for number of simulations

    # One model run at the initial parameters, so a broken setup fails now
    # with an actionable message rather than after a whole calibration of
    # NA fits. Default on; see `?create_calib_control`.
    if (isTRUE(ctrl$preflight)) {
      calib_preflight(aeme = aeme, param = param, m = m, path = path,
                      vars_sim = vars_sim, FUN_list = FUN_list,
                      weights = weights, model_controls = model_controls,
                      ctrl = ctrl, include_wlev = include_wlev,
                      method = "calib")
    }

    if (!is.null(param_var_matrix)) {
      param_var_matrix <- param_var_matrix |> 
        dplyr::filter(model == m)
    }

    if (any(vars_sim != "LKE_lvlwtr")) {
      # Extract indices for modelled variables
      AEME::cli_inform_safe(c("i" = paste0("Extracting indices for {.val ", m,
                                           "} modelled variables [",
                                           format(Sys.time()), "]")))
      suppressMessages(
        var_indices <- run_and_fit(aeme = aeme, param = param,
                                   model = m, path = path,
                                   FUN_list = FUN_list,
                                   model_controls = model_controls,
                                   vars_sim = vars_sim, weights = weights,
                                   return_indices = TRUE,
                                   include_wlev = include_wlev, fit = FALSE)
      )
      AEME::cli_inform_safe(c("v" = paste0("Indices extracted for {.val ", m,
                                           "} modelled variables [",
                                           format(Sys.time()), "]")))
    }

    # Extract parameters for the model ----
    param <- param[param$model == m, ]
    # par_idx <- which(param$model %in% c(m))

    if (is.na(ctrl$NP)) {
      ctrl$NP <- 10 * nrow(param) # sum(par_idx)
    }
    ctrl$ngen <- round(ctrl$itermax / ctrl$NP)

    # Generate parameters for running calibration
    best_pars <- NULL
    if (is.null(param_df)) {
      if (ctrl$c_method == "LHC") {
        ctrl$NP <- ctrl$itermax
        ctrl$ngen <- 1
      }
      start_param <- FME::Latinhyper(param[, c("min", "max")], ctrl$NP)
      colnames(start_param) <- param$name_full
      start_param <- as.data.frame(start_param)
      
      gen_n <- 1
      tot_gen <- ctrl$ngen
    } else {
      # Add check for parameters to be the same
      p_chk <- param$name_full %in% names(param_df)
      if (any(!p_chk)) {
        AEME::cli_safe("Not all parameters are in supplied parameter dataframe",
                       FUN = cli::cli_alert_warning)
      }
      best_pars <- param_df[param_df$fit == min(param_df$fit), ]
      if (nrow(best_pars) > 1) {
        best_pars <- best_pars[which.max(best_pars$gen), ]
      }
      last_gen <- param_df[param_df$gen == max(param_df$gen), ]
      start_param <- next_gen_params(param_df = last_gen, param = param,
                                     ctrl = ctrl, best_pars = best_pars)
      gen_n <- max(param_df$gen) + 1
      tot_gen <- max(param_df$gen) + ctrl$ngen
    }

    # Starting cutoff/mutate values to anneal from, if ctrl$cutoff_final /
    # ctrl$mutate_final are set (see anneal_param()).
    cutoff_start <- ctrl$cutoff
    mutate_start <- ctrl$mutate

    if (is.null(ctrl$ncore)) {
      ctrl$ncore <- (parallel::detectCores() - 1)
    }
    # Bound ncore by the number of available cores, parameter sets and NP so
    # that the cluster size and every generation's split use the same value.
    ctrl$ncore <- min(ctrl$ncore, (parallel::detectCores() - 1),
                      nrow(start_param), ctrl$NP)

    # Correct N of splits if ncore is greater than number of parameter sets
    splts <- ctrl$ncore

    start_param <- adj_index_params(start_param, param = param)

    suppressWarnings({
      param_list <- split(start_param, 1:splts)
    })

    # Set up parallel cluster (or serial paths) ----
    if (ctrl$parallel) {
      temp_dirs <- make_temp_dir(m, lake_dir, n = ctrl$ncore)
      paths <- temp_dirs
      AEME::cli_inform_safe(c("i" = paste0("Using {.val ", ctrl$ncore,
                                           "} cores for parallel calibration ",
                                           "for {.val ", m, "}.")))
      unlink("parallel.log")
      cl <- aeme_make_cluster(ctrl$ncore)
      on.exit(parallel::stopCluster(cl))
      varlist <- list("param", "aeme", "paths", "m", "vars_sim", "FUN_list",
                      "model_controls", "var_indices", "ctrl", "weights",
                      "include_wlev")
      parallel::clusterExport(cl, varlist = varlist,
                              envir = environment())
    } else {
      AEME::cli_inform_safe(paste0("Using serial calibration for {.val ", m, "}."))
      paths <- rep(path, ctrl$ncore)
      cl <- NULL
    }

    # Evaluate every member of a generation's population, dispatching to the
    # cluster when running in parallel and in-process (lapply) otherwise ----
    evaluate_generation <- function(param_list) {
      worker_fun <- \(pars, i) {
        aemetools:::eval_param_chunk(pars_i = pars[[i]], path = paths[i],
                                     aeme = aeme, param = param, model = m,
                                     vars_sim = vars_sim, FUN_list = FUN_list,
                                     model_controls = model_controls,
                                     ctrl = ctrl, var_indices = var_indices,
                                     weights = weights,
                                     include_wlev = include_wlev,
                                     parallel = ctrl$parallel)
      }
      model_out <- if (is.null(cl)) {
        lapply(seq_along(param_list), worker_fun, pars = param_list)
      } else {
        parallel::parLapply(cl, seq_along(param_list), worker_fun,
                            pars = param_list)
      }
      dplyr::bind_rows(model_out)
    }

    # Generation 1 ----
    announce_generation(start_param, gen_n, tot_gen, ctrl)
    g1 <- evaluate_generation(param_list)

    best_pars_fmt <- signif(g1[which.min(g1$fit), param$name_full], 3)
    report_generation(g1, gen_n, tot_gen, m, ctrl, best_pars_fmt)

    g1$gen <- 1
    best_pars <- g1[which.min(g1$fit), ]
    out_df <- apply(g1, 2, signif, digits = 6)
    nsim <- nsim + nrow(out_df)
    ctrl$sim_id <- write_simulation_output(x = out_df, ctrl = ctrl,
                                           FUN_list = FUN_list,
                                           aeme = aeme, model = m,
                                           param = param,
                                           append_metadata = TRUE)

    if (ctrl$c_method == "LHC") {
      write_calib_metadata(ctrl = ctrl, nsim = nsim,  t0 = t0)
      AEME::cli_safe(paste0("LHC calibration complete for {.val ", m, "}. [",
                            format(Sys.time()), "]"), FUN = cli::cli_alert_success)
      return(ctrl$sim_id)
    }
    if (min(g1$fit) < ctrl$VTR) {
      write_calib_metadata(ctrl = ctrl, nsim = nsim,  t0 = t0)
      AEME::cli_safe(paste0("Model fitness is less than VTR. Stopping ",
                            "simulation for {.val ", m, "}. [",
                            format(Sys.time()), "]"), FUN = cli::cli_alert_success)
      return(ctrl$sim_id)
    }

    # Select survivors ----
    ctrl$cutoff <- anneal_param(cutoff_start, ctrl$cutoff_final, gen_n, tot_gen)
    ctrl$mutate <- anneal_param(mutate_start, ctrl$mutate_final, gen_n, tot_gen)
    g <- next_gen_params(param_df = g1, param = param, ctrl = ctrl,
                         best_pars = best_pars,
                         param_var_matrix = param_var_matrix,
                         weights = weights)

    # Generations 2..ngen ----
    for (gen in seq_len(ctrl$ngen)[-1]) {

      gen_n <- gen_n + 1
      announce_generation(g, gen_n, tot_gen, ctrl)

      suppressWarnings({
        param_list <- split(g, rep(1:ctrl$ncore, length.out = ctrl$NP))
      })
      g <- evaluate_generation(param_list)
      g$gen <- gen_n

      out_df <- apply(g, 2, signif, digits = 6)
      nsim <- nsim + nrow(out_df)
      write_simulation_output(x = out_df, ctrl = ctrl, aeme = aeme,
                              model = m, param = param,
                              FUN_list = FUN_list, sim_id = ctrl$sim_id,
                              append_metadata = FALSE)

      report_generation(g, gen_n, tot_gen, m, ctrl)

      if (min(g$fit) < best_pars$fit) {
        best_pars <- g[which.min(g$fit), ]
      }

      if (min(g$fit) < ctrl$VTR) {
        AEME::cli_safe(paste0("Model fitness is less than VTR. Stopping ",
                              "simulation for {.val ", m, "}. [",
                              format(Sys.time()), "]"), FUN = cli::cli_alert_success)
        return(ctrl$sim_id)
      }
      # `reltol` is a relative tolerance: population fitness spread as a
      # fraction of the mean fitness, not an absolute spread. The
      # denominator is floored to avoid dividing by (near) zero.
      rel_spread <- sd(g$fit) / max(abs(mean(g$fit)), sqrt(.Machine$double.eps))
      if (rel_spread < ctrl$reltol) {
        AEME::cli_safe(paste0("Model fitness has converged (sd < reltol). ",
                              "Stopping simulation for {.val ", m, "}. [",
                              format(Sys.time()), "]"), FUN = cli::cli_alert_success)
        return(ctrl$sim_id)
      }

      ctrl$cutoff <- anneal_param(cutoff_start, ctrl$cutoff_final, gen_n, tot_gen)
      ctrl$mutate <- anneal_param(mutate_start, ctrl$mutate_final, gen_n, tot_gen)
      g <- next_gen_params(param_df = g, param = param, ctrl = ctrl,
                           best_pars = best_pars,
                           param_var_matrix = param_var_matrix,
                           weights = weights)
    }
    write_calib_metadata(ctrl = ctrl, nsim = nsim,  t0 = t0)
    return(ctrl$sim_id)
  })
}
