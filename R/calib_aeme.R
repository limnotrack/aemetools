#' Calibrate AEME model parameters using observations
#'
#' @name calib_aeme
#' @description
#' `calib_model()` runs the model and compares it against observations provided.
#' It can run in parallel by using multiple cores availlable on your computer
#' to run quicker.
#'
#'
#' @inheritParams AEME::build_aeme
#' @param param dataframe; of parameters read in from a csv file. Requires the
#' columns c("model", "file", "name", "value", "min", "max", "log")
#' @param vars_sim vector; of variables names to be used in the calculation of
#' model fit.
#' @param FUN_list list of functions; named according to the variables in the
#'  `vars_sim`. Funtions are of the form `function(df)` which will be used
#'  to calculate model fit. If nor provided, uses mean absolute error (MAE).
#' @param ctrl list; of controls for sensitivity analysis function created using
#'  the \code{\link{create_control}} function. See \link{create_control} for
#'  more details.
#' @param weights vector; of weights for each variable in vars_sim. If not
#' provided, defaults to 1 for each variable.
#' @param param_var_matrix list of dataframes; with parameters as rows and 
#' response variables as columns. Created using 
#' \code{\link{create_param_var_matrix}}. This is used to specify which 
#' parameters are associated with which response variables, and therefore which 
#' parameters are updated in each generation of the calibration.
#' @param param_df dataframe; of parameters to be used in the calibration.
#' Requires the columns c("model", "file", "name", "value", "min", "max"). This
#' is used to restart from a previous calibration.
#'
#' @importFrom parallel stopCluster clusterExport parLapply makeCluster
#' detectCores
#' @importFrom utils write.csv write.table
#' @importFrom stats runif
#' @importFrom FME Latinhyper
#' @importFrom dplyr mutate bind_rows
#'
#' @return string of simulation id to be used to read the simulation output.
#'
#' @export

calib_aeme <- function(aeme, model, param, vars_sim = "HYD_temp", FUN_list, 
                       weights, path = ".", model_controls = NULL, ctrl = NULL,
                       param_var_matrix = NULL, param_df = NULL) {

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
    cli::cli_inform("No weights supplied. Defaulting to 1 for all variables.")
    weights <- set_weights(vars_sim = vars_sim)
  }
  # Check if vars_sim and weights are the same length
  if (length(vars_sim) != length(weights))
    stop("vars_sim and weights must be the same length")

  if (!all(vars_sim %in% names(FUN_list)))
    stop("FUN_list must have names that match vars_sim")

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
  
  # Add index to parameter name
  param <- param |> 
    dplyr::mutate(name_full = encode_param(group, name, index))
  
  # Check for parameters where value, min, max are equal
  eq_pars <- param[param$value == param$min & param$value == param$max, ]
  if (nrow(eq_pars) > 0) {
    cli::cli_alert_warning("The following parameters have the same value, min, 
                           and max and will not be updated during calibration: {.val {eq_pars$name}}")
    AEME::input_model_parameters(aeme = aeme, model = model, param = eq_pars, 
                                 path = path)
    
    param <- param |> 
      dplyr::filter(!name_full %in% eq_pars$name_full)
  }
  
  if (!is.null(param_var_matrix)) {
    # Select logical columnms
    mat <- param_var_matrix |> 
      dplyr::select(dplyr::all_of(vars_sim))
    rem_pars <- setdiff(param$name_full, param_var_matrix$name_full[apply(mat, 1, any)])
    if (length(rem_pars) > 0) {
      cli::cli_alert_warning("The following parameters are not associated with 
                             any of the response variables and will not be 
                             updated during calibration: {.val {rem_pars}}")
      param <- param |> 
        dplyr::filter(!name_full %in% rem_pars)
    }
  }

  include_wlev <- ifelse("LKE_lvlwtr" %in% vars_sim, TRUE, FALSE)

  lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)

  names(model) <- model
  sapply(model, \(m) {
    var_indices <- list()
    t0 <- Sys.time() # Time check for calibration
    nsim <- 0 # Counter for number of simulations
    if (!is.null(param_var_matrix)) {
      param_var_matrix <- param_var_matrix |> 
        dplyr::filter(model == m)
    }

    if (any(vars_sim != "LKE_lvlwtr")) {
      # Extract indices for modelled variables
      cli::cli_inform(c("i" = "Extracting indices for {.val {m}} modelled 
                        variables [{format(Sys.time())}]"))
      suppressMessages(
        var_indices <- run_and_fit(aeme = aeme, param = param,
                                   model = m, path = path,
                                   FUN_list = FUN_list, 
                                   model_controls = model_controls,
                                   vars_sim = vars_sim, weights = weights,
                                   return_indices = TRUE,
                                   include_wlev = include_wlev, fit = FALSE)
      )
      cli::cli_inform(c("v" = "Indices extracted for {.val {m}} modelled 
                        variables [{format(Sys.time())}]"))
    }

    # Extract parameters for the model ----
    param <- param[param$model == m, ]
    # par_idx <- which(param$model %in% c(m))
    obs <- AEME::observations(aeme)
    # Check if there are observations for the model or just calibrating wlev
    ctrl$use_obs <- ifelse(!is.null(obs$lake), TRUE, FALSE)

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
        cli::cli_alert_warning("Not all parameters are in supplied parameter dataframe")
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
    if (is.null(ctrl$ncore)) {
      ctrl$ncore <- (parallel::detectCores() - 1)
      if (ctrl$ncore > nrow(start_param)) ctrl$ncore <- nrow(start_param)
    }

    # Correct N of splits if ncore is greater than number of parameters
    splts <- min(ctrl$NP, ctrl$ncore)
    
    start_param <- adj_index_params(start_param, param = param)

    suppressWarnings({
      param_list <- split(start_param, 1:splts)
    })

    # Calibrate in parallel
    if (ctrl$parallel) {

      temp_dirs <- make_temp_dir(m, lake_dir, n = ctrl$ncore)
      # list.files(temp_dirs[1], recursive = TRUE)
      ncores <- min((parallel::detectCores() - 1), ctrl$ncore, ctrl$NP)
      cli::cli_inform(c("i" = "Using {.val {ncores}} cores for parallel 
                        calibration for {.val {m}}."))

      cl <- parallel::makeCluster(ncores, outfile = "parallel.log")
      on.exit(parallel::stopCluster(cl))
      varlist <- list("param", "aeme", "path", "m", "vars_sim", "FUN_list",
                      "model_controls", "var_indices", "temp_dirs","ctrl",
                      "weights", "var_indices", "include_wlev")
      parallel::clusterExport(cl, varlist = varlist,
                              envir = environment())
      cli::cli_inform(c(">" = "Starting generation {.val {gen_n}}/{.val 
      {tot_gen}}, {.val {ctrl$NP}} members. [{format(Sys.time())}]"))
      pr_df <- data.frame(rbind(signif(apply(start_param, 2, mean), 4),
                                signif(apply(start_param, 2, median), 4),
                                signif(apply(start_param, 2, sd), 4)),
                          row.names = c("mean", "median", "sd"))
      names(pr_df) <- gsub("\\[NA\\]", "", gsub("NA/", "", names(start_param)))
      cli::cli_inform("Parameter summary for generation {.val {gen_n}}:")
      print(pr_df)
      # model_out <- lapply(seq_along(param_list), \(pars, i) {
      model_out <- parallel::parLapply(cl, seq_along(param_list), \(pars, i) {

        path <- temp_dirs[i]
        pars[[i]][["fit"]] <- NA

        # Loop through each of the parameters
        for (p in seq_len(nrow(pars[[i]]))) {

          # Update the parameter value in the parameter table
          for(n in names(pars[[i]])) {
            param$value[param$name_full == n] <- pars[[i]][p, n]
          }
          # message(i, ", ", p)

          # Save the fit value
          res <- aemetools::run_and_fit(aeme = aeme,
                                        param = param,
                                        model = m,
                                        path = path,
                                        vars_sim = vars_sim,
                                        FUN_list = FUN_list,
                                        model_controls = model_controls,
                                        na_value = ctrl$na_value,
                                        var_indices = var_indices,
                                        return_indices = FALSE,
                                        include_wlev = include_wlev,
                                        fit = TRUE,
                                        weights = weights,
                                        timeout = ctrl$timeout)

          for (v in vars_sim) {
            pars[[i]][[v]][p] <- res[[v]]
          }

          if (ctrl$na_value %in% unlist(res)) {
            res1 <- ctrl$na_value
          } else {
            res1 <- sum(unlist(res))
            res1 <- ifelse(is.na(res1), ctrl$na_value, res1)
          }

          pars[[i]][["fit"]][p] <- res1
          print(pars[[i]][["fit"]][p])
        }
        return(pars[[i]])
      }, pars = param_list)

      g1 <- dplyr::bind_rows(model_out)
      best_pars <- signif(g1[which.min(g1$fit), 1:nrow(param)], 3)
      rep_vars <- g1 |> 
        dplyr::filter(fit != ctrl$na_value)
      cli::cli_alert_success("Completed generation {.val {gen_n}}/{.val {tot_gen}} 
                             for {.val {m}}. [{format(Sys.time())}]")
      cli::cli_inform("Best fit: {.val {signif(min(rep_vars$fit), 3)}} (sd: 
                      {.val {signif(sd(rep_vars$fit), 5)}})
            Parameters: [ {.val {best_pars}} ]")
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
        cli::cli_alert_success("LHC calibration complete for {.val {m}}. 
                               [{format(Sys.time())}]")
        return(ctrl$sim_id)
      }
      if (min(g1$fit) < ctrl$VTR) {
        write_calib_metadata(ctrl = ctrl, nsim = nsim,  t0 = t0)
        cli::cli_alert_success("Model fitness is less than VTR. Stopping simulation for 
                               {.val {m}}. [{format(Sys.time())}]")
        return(ctrl$sim_id)
      }


      # Select survivors ----
      g <- next_gen_params(param_df = g1, param = param, ctrl = ctrl,
                           best_pars = best_pars, 
                           param_var_matrix = param_var_matrix, 
                           weights = weights)

      for (gen in 2:ctrl$ngen) {

        gen_n <- gen_n + 1
        cli::cli_inform(c(">" = "Starting generation {.val {gen_n}}/{.val 
        {tot_gen}}, {.val {ctrl$NP}} members. [{format(Sys.time())}]"))
        pr_df <- data.frame(rbind(signif(apply(g, 2, mean), 4),
                                  signif(apply(g, 2, median), 4),
                                  signif(apply(g, 2, sd), 4)),
                            row.names = c("mean", "median", "sd"))
        names(pr_df) <- gsub("\\[NA\\]", "", gsub("NA/", "", names(g)))
        cli::cli_inform("Parameter summary for generation {.val {gen_n}}:")
        print(pr_df)
        suppressWarnings({
          param_list <- split(g, rep(1:ctrl$ncore, each = ctrl$ncore,
                                     length.out = ctrl$NP))
        })
        # model_out <- lapply(seq_along(param_list), \(pars, i) {
        model_out <- parallel::parLapply(cl, seq_along(param_list), \(pars, i) {

          path <- temp_dirs[i]
          pars[[i]][["fit"]] <- NA

          # Loop through each of the parameters
          for(p in seq_len(nrow(pars[[i]]))) {

            # Update the parameter value in the parameter table
            for(n in names(pars[[i]])) {
              param$value[param$name_full == n] <- pars[[i]][p, n]
            }
            # print(i); print(p)

            # Save the fit value
            res <- aemetools::run_and_fit(aeme = aeme,
                                          param = param,
                                          model = m,
                                          path = path,
                                          vars_sim = vars_sim,
                                          FUN_list = FUN_list,
                                          model_controls = model_controls,
                                          na_value = ctrl$na_value,
                                          var_indices = var_indices,
                                          return_indices = FALSE,
                                          include_wlev = include_wlev,
                                          fit = TRUE,
                                          weights = weights,
                                          timeout = ctrl$timeout)

            for (v in vars_sim) {
              pars[[i]][[v]][p] <- res[[v]]
            }

            if (ctrl$na_value %in% unlist(res)) {
              res1 <- ctrl$na_value
            } else {
              res1 <- sum(unlist(res))
              res1 <- ifelse(is.na(res1), ctrl$na_value, res1)
            }
            print(res1)

            pars[[i]][["fit"]][p] <- res1
          }
          return(pars[[i]])
        }, pars = param_list)

        g <- dplyr::bind_rows(model_out)
        g$gen <- gen_n

        out_df <- apply(g, 2, signif, digits = 6)
        nsim <- nsim + nrow(out_df)
        write_simulation_output(x = out_df, ctrl = ctrl, aeme = aeme,
                                model = m, param = param,
                                FUN_list = FUN_list, sim_id = ctrl$sim_id,
                                append_metadata = FALSE)

        rep_vars <- g |> 
          dplyr::filter(fit != ctrl$na_value)
        cli::cli_alert_success("Completed generation {.val {gen_n}}/{.val {tot_gen}} 
                             for {.val {m}}. [{format(Sys.time())}]")
        cli::cli_inform("Best fit: {.val {signif(min(rep_vars$fit), 3)}} (sd: 
                      {.val {signif(sd(rep_vars$fit), 5)}})")
        
        # cli::cli_alert_success("Completed generation {.val {gen_n}}/{.val {tot_gen}} 
        #                        for {.val {m}}. [{format(Sys.time())}]")
        # cli::cli_inform("Best fit: {.val {signif(min(g$fit), 5)}} (sd: 
        #                 {.val {signif(sd(g$fit), 5)}})")
        if(min(g$fit) < best_pars$fit) {
          best_pars <- g[which.min(g$fit), ]
        }

        if (min(g$fit) < ctrl$VTR) {
          cli::cli_alert_success("Model fitness is less than VTR. Stopping simulation for 
                                 {.val {m}}. [{format(Sys.time())}]")
          return(ctrl$sim_id)
        }
        if(sd(g$fit) < ctrl$reltol) {
          cli::cli_alert_success("Model fitness has converged (sd < reltol). 
                                 Stopping simulation for {.val {m}}. 
                                 [{format(Sys.time())}]")
          return(ctrl$sim_id)
        }

        g <- next_gen_params(param_df = g, param = param, ctrl = ctrl,
                             best_pars = best_pars, 
                             param_var_matrix = param_var_matrix, 
                             weights = weights)
      }
    } else {
      # Run in serial ----
      cli::cli_inform("Using serial calibration for {.val {m}}.")
      cli::cli_inform(c(">" = "Starting generation {.val {gen_n}}/{.val 
      {tot_gen}}, {.val {ctrl$NP}} members. [{format(Sys.time())}]"))
      pr_df <- data.frame(rbind(signif(apply(start_param, 2, mean), 4),
                                signif(apply(start_param, 2, median), 4),
                                signif(apply(start_param, 2, sd), 4)),
                          row.names = c("mean", "median", "sd"))
      names(pr_df) <- gsub("\\[NA\\]", "", gsub("NA/", "", names(start_param)))
      print(pr_df)
      model_out <- lapply(seq_along(param_list), \(pars, i) {

        pars[[i]][["fit"]] <- NA
        for (v in vars_sim) {
          pars[[i]][[v]] <- NA
        }

        # Loop through each of the parameters
        for (p in seq_len(nrow(pars[[i]]))) {

          # Update the parameter value in the parameter table
          for(n in names(pars[[i]])) {
            param$value[param$name_full == n] <- pars[[i]][p, n]
          }
          # message(i, ", ", p)

          # Save the fit value
          res <- run_and_fit(aeme = aeme,
                             param = param,
                             model = m,
                             path = path,
                             vars_sim = vars_sim,
                             FUN_list = FUN_list,
                             model_controls = model_controls,
                             na_value = ctrl$na_value,
                             var_indices = var_indices,
                             return_indices = FALSE,
                             include_wlev = include_wlev,
                             fit = TRUE,
                             weights = weights,
                             timeout = ctrl$timeout)

          for (v in vars_sim) {
            pars[[i]][[v]][p] <- res[[v]]
          }

          if (ctrl$na_value %in% unlist(res)) {
            res1 <- ctrl$na_value
          } else {
            res1 <- sum(unlist(res))
            res1 <- ifelse(is.na(res1), ctrl$na_value, res1)
          }

          pars[[i]][["fit"]][p] <- res1
          print(pars[[i]][["fit"]][p])
          # print(pars[[i]][p, ])
        }
        return(pars[[i]])
      }, pars = param_list)

      g1 <- dplyr::bind_rows(model_out)
      best_pars <- signif(g1[which.min(g1$fit), 1:nrow(param)], 3)
      rep_vars <- g1 |> 
        dplyr::filter(fit != ctrl$na_value)
      cli::cli_alert_success("Completed generation {.val {gen_n}}/{.val {tot_gen}} 
                             for {.val {m}}. [{format(Sys.time())}]")
      cli::cli_inform("Best fit: {.val {signif(min(rep_vars$fit), 3)}} (sd: 
                      {.val {signif(sd(rep_vars$fit), 5)}})
            Parameters: [ {.val {best_pars}} ]")
      
      
      
      # cli::cli_alert_success("Completed generation {.val {gen_n}}/{.val {tot_gen}} 
      #                        for {.val {m}}. [{format(Sys.time())}]")
      # cli::cli_inform("Best fit: {.val {signif(min(g1$fit), 3)}} (sd: 
      #                 {.val {signif(sd(g1$fit), 5)}})
      #       Parameters: [ {.val {best_pars}} ]")
      out_df <- apply(g1, 2, signif, digits = 6)
      nsim <- nsim + nrow(out_df)
      best_pars <- g1[which.min(g1$fit), ]

      ctrl$sim_id <- write_simulation_output(x = out_df, ctrl = ctrl,
                                             FUN_list = FUN_list,
                                             aeme = aeme, model = m,
                                             param = param,
                                             append_metadata = TRUE)
      
      if (ctrl$c_method == "LHC") {
        write_calib_metadata(ctrl = ctrl, nsim = nsim,  t0 = t0)
        cli::cli_alert_success("LHC calibration complete for {.val {m}}. 
                               [{format(Sys.time())}]")
        return(ctrl$sim_id)
      }

      if (min(g1$fit) < ctrl$VTR) {
        message("Model fitness is less than VTR. Stopping simulation.")
        return(ctrl$sim_id)
      }


      # Select survivors ----
      g <- next_gen_params(param_df = g1, param = param, ctrl = ctrl,
                           best_pars = best_pars, 
                           param_var_matrix = param_var_matrix, 
                           weights = weights)

      for (gen in 2:ctrl$ngen) {

        gen_n <- gen_n + 1
        cli::cli_inform(c(">" = "Starting generation {.val {gen_n}}/{.val 
        {tot_gen}}, {.val {ctrl$NP}} members. [{format(Sys.time())}]"))
        pr_df <- data.frame(rbind(signif(apply(g, 2, mean), 4),
                                  signif(apply(g, 2, median), 4),
                                  signif(apply(g, 2, sd), 4)),
                            row.names = c("mean", "median", "sd"))
        # names(pr_df) <- gsub("NA/", "", names(g))
        names(pr_df) <- gsub("\\[NA\\]", "", gsub("NA/", "", names(g)))
        print(pr_df)
        suppressWarnings({
          param_list <- split(g, rep(1:ctrl$ncore, each = ctrl$ncore,
                                     length.out = ctrl$NP))
        })
        model_out <- lapply(seq_along(param_list), \(pars, i) {

          pars[[i]][["fit"]] <- NA
          for (v in vars_sim) {
            pars[[i]][[v]] <- NA
          }

          # Loop through each of the parameters
          for(p in seq_len(nrow(pars[[i]]))) {

            # Update the parameter value in the parameter table
            for(n in names(pars[[i]])) {
              param$value[param$name_full == n] <- pars[[i]][p, n]
            }
            # print(i); print(p)

            # Save the fit value
            res <- run_and_fit(aeme = aeme,
                               param = param,
                               model = m,
                               path = path,
                               vars_sim = vars_sim,
                               FUN_list = FUN_list,
                               model_controls = model_controls,
                               na_value = ctrl$na_value,
                               var_indices = var_indices,
                               return_indices = FALSE,
                               include_wlev = include_wlev,
                               fit = TRUE,
                               weights = weights,
                               timeout = ctrl$timeout)

            for (v in vars_sim) {
              pars[[i]][[v]][p] <- res[[v]]
            }

            if (ctrl$na_value %in% unlist(res)) {
              res1 <- ctrl$na_value
            } else {
              res1 <- sum(unlist(res))
              res1 <- ifelse(is.na(res1), ctrl$na_value, res1)
            }

            pars[[i]][["fit"]][p] <- res1
            print(pars[[i]][["fit"]][p])
            # print(pars[[i]][p, ])
          }
          return(pars[[i]])
        }, pars = param_list)
        

        g <- dplyr::bind_rows(model_out)
        g$gen <- gen_n
        out_df <- apply(g, 2, signif, digits = 6)

        nsim <- nsim + nrow(out_df)
        write_simulation_output(x = out_df, ctrl = ctrl, aeme = aeme,
                                 model = m, param = param,
                                FUN_list = FUN_list, sim_id = ctrl$sim_id,
                                append_metadata = FALSE)

        
        rep_vars <- g |> 
          dplyr::filter(fit != ctrl$na_value)
        cli::cli_alert_success("Completed generation {.val {gen_n}}/{.val {tot_gen}} 
                             for {.val {m}}. [{format(Sys.time())}]")
        cli::cli_inform("Best fit: {.val {signif(min(rep_vars$fit), 3)}} (sd: 
                      {.val {signif(sd(rep_vars$fit), 5)}})")
        
        # cli::cli_alert_success("Completed generation {.val {gen_n}}/{.val {tot_gen}} 
        #                        for {.val {m}}. [{format(Sys.time())}]")
        # cli::cli_inform("Best fit: {.val {signif(min(g$fit), 5)}} (sd: 
        #                 {.val {signif(sd(g$fit), 5)}})")
        if(min(g$fit) < best_pars$fit) {
          best_pars <- g[which.min(g$fit), ]
        }

        if (min(g$fit) < ctrl$VTR) {
          cli::cli_alert_success("Model fitness is less than VTR. Stopping 
          simulation for {.val {m}}. [{format(Sys.time())}]")
          return(ctrl$sim_id)
        }
        if(sd(g$fit) < ctrl$reltol) {
          cli::cli_alert_success("Model fitness has converged (sd < reltol). 
                                 Stopping simulation for {.val {m}}. 
                                 [{format(Sys.time())}]")
          return(ctrl$sim_id)
        }

        g <- next_gen_params(param_df = g1, param = param, ctrl = ctrl,
                             best_pars = best_pars, 
                             param_var_matrix = param_var_matrix, 
                             weights = weights)
      }
    }
    write_calib_metadata(ctrl = ctrl, nsim = nsim,  t0 = t0)
    return(ctrl$sim_id)
  })
}
