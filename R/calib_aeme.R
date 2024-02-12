#' Calibrate AEME model parameters using observations
#'
#' @name calib_aeme
#' @description
#' `calib_model()` runs the model and compares it against observations provided.
#' It can run in parallel by using multiple cores availlable on your computer
#' to run quicker.
#'
#'
#' @param param dataframe; of parameters read in from a csv file. Requires the
#' columns c("model", "file", "name", "value", "min", "max", "log")
#' @param model string; for which model. Options are c("dy_cd", "glm_aed" and
#'  "gotm_wet")
#' @param vars_sim vector; of variables names to be used in the calculation of
#' model fit. Currently only supports using one variable.
#' @param FUN_list list of functions; named according to the variables in the
#'  `vars_sim`. Funtions are of the form `function(df)` which will be used
#'  to calculate model fit. If NULL, uses mean absolute error (MAE).
#' @param ctrl list; of controls for calibration function.
#' * `VTR` Value to be reached. The optimization process will stop if
#' either the maximum number of iterations itermax is reached or the best
#' parameter vector bestmem has found a value fn(bestmem) <= VTR. Default to
#'  -Inf.
#' * `NP` number of population members. Defaults to NA; if the user does not
#'  change the value of NP from NA it is reset as
#'   `10 * sum(param$model == model)`. For many  problems it is best to set NP
#'   to be at least 10 times the length of the parameter vector.
#' * `itermax` the maximum iteration (population generation) allowed.
#' Default is 200.
#' * `reltol` relative convergence tolerance. The algorithm stops if it is
#'  unable to reduce the value by a factor of `reltol * (abs(val) + reltol)`.
#'  Default = 0.07
#'  * `cutoff`: The quantile cutoff used to select the parents for the next
#'  generation. For example, if `cutoff = 0.25`, the best 25% of the population
#'  will be used as parents for the next generation.
#'  * `mutate` fraction of population to undergo mutation (0-1).
#'  * `parallel` boolean; run calibration in parallel. Default to TRUE
#'  * `out_file` filepath; to csv for calibration output to be written to.
#'  Defaults to "results.csv"
#'  * `na_value` value to replace NA values with in observations. Default to
#'   999.
#'  * `ncore`: The number of cores to use for the calibration. This is only used
#'  if `parallel = TRUE`. Default to `parallel::detectCores() - 1`.
#' @inheritParams AEME::build_ensemble
#' @param weights vector; of weights for each variable in vars_sim. Default to
#' c(1).
#' @param param_df dataframe; of parameters read in from a csv file. Requires
#' the columns c("model", "file", "name", "value", "min", "max").
#'
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom utils write.csv write.table
#' @importFrom stats runif
#' @importFrom FME Latinhyper
#' @importFrom dplyr mutate
#'
#' @return list; ctrl which was supplied with updated arguments if missing.
#'
#' @export

calib_aeme <- function(aeme_data, path = ".", param, model, mod_ctrls,
                       vars_sim = "HYD_temp", FUN_list = NULL, ctrl = NULL,
                       weights = c(1), param_df = NULL) {

  # Check if vars_sim and weights are the same length
  if (length(vars_sim) != length(weights))
    stop("vars_sim and weights must be the same length")

  if (!all(vars_sim %in% names(FUN_list)))
    stop("FUN_list must have names that match vars_sim")

  if (is.null(ctrl)) {
    ctrl <- list(VTR = -Inf, NP = NA, itermax = 200, reltol = 0.07,
                 cutoff = 0.25, mutate = 0.1, parallel = TRUE, out_file = "results.csv",
                 na_value = 999)
  }
  if (is.null(ctrl$na_value)) {
    ctrl$na_value <- 999
  }

  include_wlev <- ifelse("LKE_lvlwtr" %in% vars_sim, TRUE, FALSE)

  lke <- AEME::lake(aeme_data)
  lakename <- tolower(lke[["name"]])
  lake_dir <- file.path(path, paste0(lke$id, "_", lakename))

  var_indices <- list()
  if (any(vars_sim != "LKE_lvlwtr")) {
    # Extract indices for modelled variables
    message("Extracting indices for modelled variables [",
            format(Sys.time()), "]")
    suppressMessages(
      var_indices <- run_and_fit(aeme_data = aeme_data, param = param,
                                 model = model, path = path, FUN_list = FUN_list,
                                 mod_ctrls = mod_ctrls, vars_sim = vars_sim,
                                 weights = weights,
                                 return_indices = TRUE,
                                 include_wlev = include_wlev,
                                 fit = FALSE)
    )
    message("Complete! [", format(Sys.time()), "]")
  }

  # Extract parameters for the model ----
  param <- param[param$model == model, ]
  # par_idx <- which(param$model %in% c(model))
  obs <- AEME::observations(aeme_data)
  # Check if there are observations for the model or just calibrating wlev
  ctrl$use_obs <- ifelse(!is.null(obs$lake), TRUE, FALSE)

  if (is.na(ctrl$NP)) {
    ctrl$NP <- 10 * nrow(param) # sum(par_idx)
  }
  ctrl$ngen <- round(ctrl$itermax / ctrl$NP)

  # Generate parameters for running calibration
  best_pars <- NULL
  if (is.null(param_df)) {
    start_param <- FME::Latinhyper(param[, c("min", "max")],
                                   ctrl$NP)
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

  suppressWarnings({
    param_list <- split(start_param, rep(1:ctrl$ncore))
  })

  # Calibrate in parallel
  if (ctrl$parallel) {

    temp_dirs <- make_temp_dir(model, lake_dir, n = ctrl$ncore)
    # list.files(temp_dirs[1], recursive = TRUE)
    ncores <- min((parallel::detectCores() - 1), ctrl$ncore)
    message("Calibrating in parallel using ", ncores, " cores...")

    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl))
    varlist <- list("param", "aeme_data", "path", "model", "vars_sim", "FUN_list",
                    "mod_ctrls", "var_indices", "temp_dirs","ctrl",
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
        res <- aemetools::run_and_fit(aeme_data = aeme_data,
                                      param = param,
                                      model = model,
                                      path = path,
                                      vars_sim = vars_sim,
                                      FUN_list = FUN_list,
                                      mod_ctrls = mod_ctrls,
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
    sim_id <- write_simulation_output(x = out_df, ctrl = ctrl,
                                      FUN_list = FUN_list,
                                      aeme_data = aeme_data, model = model,
                                      param = param, method = "calib",
                                      append_metadata = TRUE)
    ctrl$sim_id <- sim_id

    if (min(g1$fit) < ctrl$VTR) {
      message("Model fitness is less than VTR. Stopping simulation.")
      return(ctrl)
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
          res <- aemetools::run_and_fit(aeme_data = aeme_data,
                                        param = param,
                                        model = model,
                                        path = path,
                                        vars_sim = vars_sim,
                                        FUN_list = FUN_list,
                                        mod_ctrls = mod_ctrls,
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
      write_simulation_output(x = out_df, ctrl = ctrl, aeme_data = aeme_data,
                              model = model, param = param, method = "calib",
                              FUN_list = FUN_list, sim_id = ctrl$sim_id,
                              append_metadata = FALSE)

      message("Best fit: ", signif(min(g$fit), 5), " (sd: ",
              signif(sd(g$fit), 5), ")")
      if(min(g$fit) < best_pars$fit) {
        best_pars <- g[which.min(g$fit), ]
      }

      if (min(g$fit) < ctrl$VTR) {
        message("Model fitness is less than VTR. Stopping simulation.")
        return(ctrl)
      }
      if(sd(g$fit) < ctrl$reltol) {
        message("Model has converged. Stopping simulation.")
        return(ctrl)
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
        res <- run_and_fit(aeme_data = aeme_data,
                           param = param,
                           model = model,
                           path = path,
                           vars_sim = vars_sim,
                           FUN_list = FUN_list,
                           mod_ctrls = mod_ctrls,
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
    best_pars <- g1[which.min(g1$fit), ]

    ctrl$sim_id <- write_simulation_output(x = out_df, ctrl = ctrl,
                                           FUN_list = FUN_list,
                                           aeme_data = aeme_data, model = model,
                                           param = param, method = "calib",
                                           append_metadata = TRUE)

    if (min(g1$fit) < ctrl$VTR) {
      message("Model fitness is less than VTR. Stopping simulation.")
      return(ctrl)
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
          res <- run_and_fit(aeme_data = aeme_data,
                             param = param,
                             model = model,
                             path = path,
                             vars_sim = vars_sim,
                             FUN_list = FUN_list,
                             mod_ctrls = mod_ctrls,
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

      write_simulation_output(x = out_df, ctrl = ctrl, aeme_data = aeme_data,
                              model = model, param = param, method = "calib",
                              FUN_list = FUN_list, sim_id = ctrl$sim_id,
                              append_metadata = FALSE)

      message("Best fit: ", signif(min(g$fit), 5), " (sd: ",
              signif(sd(g$fit), 5), ")")
      if(min(g$fit) < best_pars$fit) {
        best_pars <- g[which.min(g$fit), ]
      }

      if (min(g$fit) < ctrl$VTR) {
        message("Model fitness is less than VTR. Stopping simulation.")
        return(ctrl)
      }
      if(sd(g$fit) < ctrl$reltol) {
        message("Model has converged. Stopping simulation.")
        return(ctrl)
      }

      g <- next_gen_params(param_df = g1, param = param, ctrl = ctrl,
                           best_pars = best_pars)
    }
  }
  sim_id
}
