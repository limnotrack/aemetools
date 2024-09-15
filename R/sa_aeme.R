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
#' @importFrom AEME lake
#' @importFrom parallel stopCluster clusterExport parLapply detectCores
#' makeCluster
#' @importFrom utils write.csv write.table
#' @importFrom stats runif
#' @importFrom FME Latinhyper
#' @importFrom dplyr bind_rows
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
#'   utils::data("aeme_parameters", package = "AEME")
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

sa_aeme <- function(aeme, path = ".", param, model, model_controls = NULL,
                    FUN_list = NULL, ctrl = NULL, param_df = NULL) {

  # Check if vars_sim and weights are the same length
  if (is.null(ctrl)) {
    stop("ctrl must be supplied")
  }
  if (is.null(model_controls)) {
    config <- AEME::configuration(aeme = aeme)
    model_controls <- config$model_controls
  }
  vars_sim <- sapply(ctrl$vars_sim, \(v) v$var) |>
    unique()
  weights <- rep(1, length(vars_sim))
  names(weights) <- vars_sim
  # if (length(vars_sim) != length(weights))
  #   stop("vars_sim and weights must be the same length")

  if (!all(vars_sim %in% names(FUN_list)))
    stop("FUN_list must have names that match vars_sim")

  if (is.null(ctrl$na_value)) {
    ctrl$na_value <- 999
  }

  include_wlev <- ifelse("LKE_lvlwtr" %in% vars_sim, TRUE, FALSE)
  lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)

  names(model) <- model
  sapply(model, \(m) {
    var_indices <- NULL
    if (any(vars_sim != "LKE_lvlwtr")) {
      # Extract indices for modelled variables
      message("Extracting indices for ", m, " modelled variables [",
              format(Sys.time()), "]")
      suppressMessages(
        var_indices <- run_and_fit(aeme = aeme, param = param,
                                   model = m, path = path, FUN_list = FUN_list,
                                   model_controls = model_controls,
                                   vars_sim = vars_sim,
                                   weights = weights,
                                   return_indices = TRUE,
                                   include_wlev = include_wlev,
                                   method = "sa", sa_ctrl = ctrl,
                                   fit = FALSE)
      )
      message("Complete! [", format(Sys.time()), "]")
    }

    # Extract parameters for the model ----
    param <- param[param$model == m, ]
    # par_idx <- which(param$model %in% c(m))

    # Generate parameters for sensitivity analysis ----
    if (is.null(param_df)) {
      ## Create sample matrix to compute first and total-order indices:
      mat <- sensobol::sobol_matrices(N = ctrl$N, params = param$name)
      param_df <- mat
      for (i in 1:ncol(mat)) {
        param_df[, i] <- param$min[i] + (param$max[i] - param$min[i]) * mat[, i]
      }
      colnames(param_df) <- paste0(param$group, "/", param$name)
      param_df <- as.data.frame(param_df)
    }
    if (is.null(ctrl$ncore)) {
      ctrl$ncore <- (parallel::detectCores() - 1)
      if (ctrl$ncore > nrow(param_df)) ctrl$ncore <- nrow(param_df)
    }

    suppressWarnings({
      param_list <- split(param_df, rep(1:ctrl$ncore))
    })

    # Run in parallel
    if (ctrl$parallel) {

      temp_dirs <- make_temp_dir(m, lake_dir, n = ctrl$ncore)
      # list.files(temp_dirs[1], recursive = TRUE)
      ncores <- min(c((parallel::detectCores() - 1), ctrl$ncore))
      nmes <- names(ctrl$vars_sim)
      message("Running sensitivity analysis in parallel for ", m, " using ",
              ncores, " cores with ", nrow(param_df), " parameter sets [",
              format(Sys.time()), "]")

      cl <- parallel::makeCluster(ncores, outfile = "parallel.log")
      on.exit(parallel::stopCluster(cl))
      varlist <- list("param", "aeme", "path", "m", "vars_sim",
                      "FUN_list", "model_controls", "var_indices", "temp_dirs",
                      "ctrl", "weights", "var_indices", "include_wlev", "nmes")
      parallel::clusterExport(cl, varlist = varlist,
                              envir = environment())
      pr_df <- data.frame(rbind(signif(apply(param_df, 2, mean), 4),
                                signif(apply(param_df, 2, median), 4),
                                signif(apply(param_df, 2, sd), 4)),
                          row.names = c("mean", "median", "sd"))
      names(pr_df) <- gsub("NA/", "", names(param_df))
      print(pr_df)

      # model_out <- lapply(seq_along(param_list), \(pars, i) {
      model_out <- parallel::parLapply(cl, seq_along(param_list), \(pars, i) {

        path <- temp_dirs[i]
        # Add columns to pars
        # pars[[i]][["fit"]] <- NA
        for (n in nmes) {
          pars[[i]][[n]] <- NA
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
                                        method = "sa", sa_ctrl = ctrl,
                                        weights = weights)

          for (n in nmes) {
            pars[[i]][[n]][p] <- res[[n]]
          }

          # if (ctrl$na_value %in% unlist(res)) {
          #   res1 <- ctrl$na_value
          # } else {
          #   res1 <- sum(unlist(res))
          #   res1 <- ifelse(is.na(res1), ctrl$na_value, res1)
          # }
          #
          # pars[[i]][["fit"]][p] <- res1
          # print(pars[[i]][["fit"]][p])
        }
        return(pars[[i]])
      }, pars = param_list)

      message("Completed ", m, "! [", format(Sys.time()), "]")

      g1 <- dplyr::bind_rows(model_out)
      out_df <- apply(g1, 2, signif, digits = 6)
      ctrl$sim_id <- write_simulation_output(x = out_df, ctrl = ctrl,
                                             FUN_list = FUN_list,
                                             aeme = aeme, model = m,
                                             param = param,
                                             append_metadata = TRUE)
    } else {
      # Run in serial ----
      message("Running sensitivity analysis in serial for ", m, " with ",
              nrow(param_df), " parameter sets [", format(Sys.time()), "]")
      pr_df <- data.frame(rbind(signif(apply(param_df, 2, mean), 4),
                                signif(apply(param_df, 2, median), 4),
                                signif(apply(param_df, 2, sd), 4)),
                          row.names = c("mean", "median", "sd"))
      names(pr_df) <- gsub("NA/", "", names(param_df))
      print(pr_df)
      nmes <- names(ctrl$vars_sim)
      model_out <- lapply(seq_along(param_list), \(pars, i) {

        # Add columns to pars
        # pars[[i]][["fit"]] <- NA
        for (n in nmes) {
          pars[[i]][[n]] <- NA
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
                             fit = TRUE, method = "sa", sa_ctrl = ctrl,
                             weights = weights)

          for (n in nmes) {
            pars[[i]][[n]][p] <- res[[n]]
          }

          # if (ctrl$na_value %in% unlist(res)) {
          #   res1 <- ctrl$na_value
          # } else {
          #   res1 <- sum(unlist(res))
          #   res1 <- ifelse(is.na(res1), ctrl$na_value, res1)
          # }
          #
          # pars[[i]][["fit"]][p] <- res1
        }

        return(pars[[i]])
      }, pars = param_list)

      g1 <- dplyr::bind_rows(model_out)
      out_df <- apply(g1, 2, signif, digits = 6)
      ctrl$sim_id <- write_simulation_output(x = out_df, ctrl = ctrl,
                                             FUN_list = FUN_list,
                                             aeme = aeme, model = m,
                                             param = param,
                                             append_metadata = TRUE)

      message("Completed ", m, "! [", format(Sys.time()), "]")
    }
    ctrl$sim_id
  })
}
