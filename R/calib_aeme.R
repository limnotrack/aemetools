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
#' @param model string; for which model to calibrate. Only one model can be
#' passed. Options are c("dy_cd", "glm_aed" and "gotm_wet").
#' @param vars_sim vector; of variables names to be used in the calculation of
#' model fit. Currently only supports using one variable.
#' @param FUN_list list of functions; named according to the variables in the
#'  `vars_sim`. Funtions are of the form `function(df)` which will be used
#'  to calculate model fit. If NULL, uses mean absolute error (MAE).
#' @param ctrl list; of controls for sensitivity analysis function created using
#'  the \code{\link{create_control}} function. See \link{create_control} for
#'  more details.
#' @param weights vector; of weights for each variable in vars_sim. Default to
#' c(1).
#' @param param_df dataframe; of parameters to be used in the calibration.
#' Requires the columns c("model", "file", "name", "value", "min", "max"). This
#' is used to restart from a previous calibration.
#'
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom utils write.csv write.table
#' @importFrom stats runif
#' @importFrom FME Latinhyper
#' @importFrom dplyr mutate
#'
#' @return string of simulation id to be used to read the simulation output.
#'
#' @export

calib_aeme <- function(aeme, path = ".", param, model, model_controls = NULL,
                       vars_sim = "HYD_temp", FUN_list = NULL, ctrl = NULL,
                       weights = c(1), param_df = NULL) {

  # Check if vars_sim and weights are the same length
  if (length(vars_sim) != length(weights))
    stop("vars_sim and weights must be the same length")

  if (!all(vars_sim %in% names(FUN_list)))
    stop("FUN_list must have names that match vars_sim")

  if (is.null(model_controls)) {
    config <- AEME::configuration(aeme = aeme)
    model_controls <- config$model_controls
  }

  if (is.null(ctrl)) {
    ctrl <- create_control(method = "calib", NP = NA, itermax = 200)
  }
  if (is.null(ctrl$na_value)) {
    ctrl$na_value <- 999
  }
  nsim <- 0 # Counter for number of simulations
  t0 <- Sys.time() # Time check for calibration

  include_wlev <- ifelse("LKE_lvlwtr" %in% vars_sim, TRUE, FALSE)

  lke <- AEME::lake(aeme)
  lakename <- tolower(lke[["name"]])
  lake_dir <- file.path(path, paste0(lke$id, "_", lakename))

  names(model) <- model
  sapply(model, \(m) {
    var_indices <- list()
    if (any(vars_sim != "LKE_lvlwtr")) {
      # Extract indices for modelled variables
      message("Extracting indices for ", m, " modelled variables [",
              format(Sys.time()), "]")
      suppressMessages(
        var_indices <- run_and_fit(aeme = aeme, param = param,
                                   model = m, path = path,
                                   FUN_list = FUN_list, model_controls = model_controls,
                                   vars_sim = vars_sim, weights = weights,
                                   return_indices = TRUE,
                                   include_wlev = include_wlev, fit = FALSE)
      )
      message("Completed ", m, "! [", format(Sys.time()), "]")
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
      # start_param <- apply(param[, c("min", "max")], 1,
      #                      \(x) runif(ctrl$NP, x[1], x[2]))
      colnames(start_param) <- paste0(param$group, "/", param$name)
      # colnames(start_param) <- param$name
      start_param <- as.data.frame(start_param)
      gen_n <- 1
      tot_gen <- ctrl$ngen
    } else {
      # Add check for parameters to be the same
      p_chk <- param$name %in% names(param_df)
      if (any(!p_chk)) {
        message("Warning! Not all parameters are in supplied parameter dataframe")
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
      ctrl$ncore <- parallel::detectCores() - 1
      if (ctrl$ncore > nrow(start_param)) ctrl$ncore <- nrow(start_param)
    }

    # Correct N of splits if ncore is greater than number of parameters
    splts <- min(ctrl$NP, ctrl$ncore)

    suppressWarnings({
      param_list <- split(start_param, 1:splts)
    })

    # Calibrate in parallel
    if (ctrl$parallel) {

      temp_dirs <- make_temp_dir(m, lake_dir, n = ctrl$ncore)
      # list.files(temp_dirs[1], recursive = TRUE)
      ncores <- min((parallel::detectCores() - 1), ctrl$ncore, ctrl$NP)
      message("Calibrating in parallel for ", m, " using ", ncores, " cores...")

      cl <- parallel::makeCluster(ncores)
      on.exit(parallel::stopCluster(cl))
      varlist <- list("param", "aeme", "path", "m", "vars_sim", "FUN_list",
                      "model_controls", "var_indices", "temp_dirs","ctrl",
                      "weights", "var_indices", "include_wlev")
      parallel::clusterExport(cl, varlist = varlist,
                              envir = environment())
      message("Starting generation ", gen_n, "/", tot_gen,", ",
              ctrl$NP, " members. ",
              "[", format(Sys.time()), "]")
      pr_df <- data.frame(rbind(signif(apply(start_param, 2, mean), 4),
                                signif(apply(start_param, 2, median), 4),
                                signif(apply(start_param, 2, sd), 4)),
                          row.names = c("mean", "median", "sd"))
      names(pr_df) <- gsub("NA/", "", names(start_param))
      print(pr_df)
      # model_out <- lapply(seq_along(param_list), \(pars, i) {
      model_out <- parallel::parLapply(cl, seq_along(param_list), \(pars, i) {

        path <- temp_dirs[i]
        pars[[i]][["fit"]] <- NA

        # Loop through each of the parameters
        for (p in seq_len(nrow(pars[[i]]))) {

          # Update the parameter value in the parameter table
          for(n in names(pars[[i]])) {
            grp <- strsplit(n, "/")[[1]][1]
            nme <- paste0(strsplit(n, "/")[[1]][-1], collapse = "/")
            if (grp != "NA") {
              param$value[param$name == nme & param$group == grp] <- pars[[i]][p, n]
            } else {
              param$value[param$name == nme] <- pars[[i]][p, n]
            }
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
                                        weights = weights)

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

      g1 <- do.call(rbind, model_out)
      message("Best fit: ", signif(min(g1$fit), 3), " (sd: ",
              signif(sd(g1$fit), 5), ")
            Parameters: [", paste0(signif(g1[which.min(g1$fit),
                                             1:nrow(param)], 3),
                                   collapse = ", "), "]")
      g1$gen <- 1
      best_pars <- g1[which.min(g1$fit), ]
      out_df <- apply(g1, 2, signif, digits = 6)
      nsim <- nsim + nrow(out_df)
      ctrl$sim_id <- write_simulation_output(x = out_df, ctrl = ctrl,
                                             FUN_list = FUN_list,
                                             aeme = aeme, model = m,
                                             param = param, path = path,
                                             append_metadata = TRUE)

      if (ctrl$c_method == "LHC") {
        write_calib_metadata(ctrl = ctrl, nsim = nsim, path = path, t0 = t0)
        message("Completed LHC calibration. [", format(Sys.time()), "]")
        return(ctrl$sim_id)
      }
      if (min(g1$fit) < ctrl$VTR) {
        write_calib_metadata(ctrl = ctrl, nsim = nsim, path = path, t0 = t0)
        message("Model fitness is less than VTR. Stopping simulation.")
        return(ctrl$sim_id)
      }


      # Select survivors ----
      g <- next_gen_params(param_df = g1, param = param, ctrl = ctrl,
                           best_pars = best_pars)

      for (gen in 2:ctrl$ngen) {

        gen_n <- gen_n + 1
        message("Starting generation ", gen_n, "/", tot_gen,", ", ctrl$NP,
                " members. ", "[", format(Sys.time()), "]")
        pr_df <- data.frame(rbind(signif(apply(g, 2, mean), 4),
                                  signif(apply(g, 2, median), 4),
                                  signif(apply(g, 2, sd), 4)),
                            row.names = c("mean", "median", "sd"))
        names(pr_df) <- gsub("NA/", "", names(g))
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
              grp <- strsplit(n, "/")[[1]][1]
              nme <- paste0(strsplit(n, "/")[[1]][-1], collapse = "/")
              if (grp != "NA") {
                param$value[param$name == nme & param$group == grp] <- pars[[i]][p, n]
              } else {
                param$value[param$name == nme] <- pars[[i]][p, n]
              }
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
                                          weights = weights)

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

        g <- do.call(rbind, model_out)
        g$gen <- gen_n

        out_df <- apply(g, 2, signif, digits = 6)
        nsim <- nsim + nrow(out_df)
        write_simulation_output(x = out_df, ctrl = ctrl, aeme = aeme,
                                model = m, param = param, path = path,
                                FUN_list = FUN_list, sim_id = ctrl$sim_id,
                                append_metadata = FALSE)

        message("Best fit: ", signif(min(g$fit), 5), " (sd: ",
                signif(sd(g$fit), 5), ")")
        if(min(g$fit) < best_pars$fit) {
          best_pars <- g[which.min(g$fit), ]
        }

        if (min(g$fit) < ctrl$VTR) {
          message("Model fitness is less than VTR. Stopping simulation.")
          return(ctrl$sim_id)
        }
        if(sd(g$fit) < ctrl$reltol) {
          message("Model has converged. Stopping simulation.")
          return(ctrl$sim_id)
        }

        g <- next_gen_params(param_df = g, param = param, ctrl = ctrl,
                             best_pars = best_pars)
      }
    } else {
      # Run in serial ----
      message("Starting generation ", gen_n, "/", tot_gen,", ",
              ctrl$NP, " members. ",
              "[", format(Sys.time()), "]")
      pr_df <- data.frame(rbind(signif(apply(start_param, 2, mean), 4),
                                signif(apply(start_param, 2, median), 4),
                                signif(apply(start_param, 2, sd), 4)),
                          row.names = c("mean", "median", "sd"))
      names(pr_df) <- gsub("NA/", "", names(start_param))
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
            grp <- strsplit(n, "/")[[1]][1]
            nme <- paste0(strsplit(n, "/")[[1]][-1], collapse = "/")
            if (grp != "NA") {
              param$value[param$name == nme & param$group == grp] <- pars[[i]][p, n]
            } else {
              param$value[param$name == nme] <- pars[[i]][p, n]
            }
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
                             weights = weights)

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

      g1 <- do.call(rbind, model_out)
      message("Best fit: ", signif(min(g1$fit), 3), " (sd: ",
              signif(sd(g1$fit), 3), ")
            Parameters: [", paste0(signif(g1[which.min(g1$fit),
                                             1:nrow(param)], 3),
                                   collapse = ", "), "]")
      g1$gen <- gen_n
      out_df <- apply(g1, 2, signif, digits = 6)
      nsim <- nsim + nrow(out_df)
      best_pars <- g1[which.min(g1$fit), ]

      ctrl$sim_id <- write_simulation_output(x = out_df, ctrl = ctrl,
                                             FUN_list = FUN_list,
                                             aeme = aeme, model = m,
                                             param = param,
                                             path = path,
                                             append_metadata = TRUE)

      if (min(g1$fit) < ctrl$VTR) {
        message("Model fitness is less than VTR. Stopping simulation.")
        return(ctrl$sim_id)
      }


      # Select survivors ----
      g <- next_gen_params(param_df = g1, param = param, ctrl = ctrl,
                           best_pars = best_pars)

      for (gen in 2:ctrl$ngen) {

        gen_n <- gen_n + 1
        message("Starting generation ", gen_n, "/", tot_gen,", ", ctrl$NP,
                " members. ", "[", format(Sys.time()), "]")
        pr_df <- data.frame(rbind(signif(apply(g, 2, mean), 4),
                                  signif(apply(g, 2, median), 4),
                                  signif(apply(g, 2, sd), 4)),
                            row.names = c("mean", "median", "sd"))
        names(pr_df) <- gsub("NA/", "", names(g))
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
              grp <- strsplit(n, "/")[[1]][1]
              nme <- paste0(strsplit(n, "/")[[1]][-1], collapse = "/")
              if (grp != "NA") {
                param$value[param$name == nme & param$group == grp] <- pars[[i]][p, n]
              } else {
                param$value[param$name == nme] <- pars[[i]][p, n]
              }
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
                               weights = weights)

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
            # print(pars[[i]][["fit"]][p])
            print(pars[[i]][p, ])
          }
          return(pars[[i]])
        }, pars = param_list)

        g <- do.call(rbind, model_out)
        g$gen <- gen_n
        out_df <- apply(g, 2, signif, digits = 6)

        nsim <- nsim + nrow(out_df)
        write_simulation_output(x = out_df, ctrl = ctrl, aeme = aeme,
                                path = path, model = m, param = param,
                                FUN_list = FUN_list, sim_id = ctrl$sim_id,
                                append_metadata = FALSE)

        message("Best fit: ", signif(min(g$fit), 5), " (sd: ",
                signif(sd(g$fit), 5), ")")
        if(min(g$fit) < best_pars$fit) {
          best_pars <- g[which.min(g$fit), ]
        }

        if (min(g$fit) < ctrl$VTR) {
          message("Model fitness is less than VTR. Stopping simulation.")
          return(ctrl$sim_id)
        }
        if(sd(g$fit) < ctrl$reltol) {
          message("Model has converged. Stopping simulation.")
          return(ctrl$sim_id)
        }

        g <- next_gen_params(param_df = g1, param = param, ctrl = ctrl,
                             best_pars = best_pars)
      }
    }
    write_calib_metadata(ctrl = ctrl, nsim = nsim, path = path, t0 = t0)
    return(ctrl$sim_id)
  })
}
