#' Run sensitivity analysis on AEME model parameters
#'
#' @name sa_aeme
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
#' @param FUN_list list of functions; named according to the variables in the
#'  `vars_sim`. Funtions are of the form `function(df)` which will be used
#'  to calculate model fit. If NULL, uses mean absolute error (MAE).
#' @param ctrl list; of controls for calibration function. The control
#' parameters are as follows:
#' -   `N`: The initial sample size of the base sample matrix.
#' -   `ncore`: The number of cores to use for the calibration. This is only
#' used if parallel = `TRUE`. Default to `parallel::detectCores() - 1.`
#' -   `na_value`: value to replace NA value when returned.
#' -   `parallel`: Logical value. If `TRUE`, the sensitivity analysis will be
#'  run in parallel. If `FALSE`, it will be run in series.
#' -   `out_file`: A character string naming a file for writing. Currently it
#'   can write to a .csv file or a database (.db; using the duckDB package)
#'   file. If writing to a database, it creates a table named "sa_output".
#' -   `vars_sim`: A named list of output variables for sensitivity analysis.
#'   The name is user defined but each list must contain:
#'     - `var`: The variable name to use for the sensitivity analysis.
#'     - `month`: A vector of months to use for the sensitivity analysis.
#'     - `depth_range`: A vector of length 2 with the minimum and maximum depth
#'   range to use for the sensitivity analysis.
#' @inheritParams AEME::build_ensemble
#' @param param_df dataframe; of parameters read in from a csv file. Requires
#' the columns c("model", "file", "name", "value", "min", "max").
#'
#' @importFrom AEME lake
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom utils write.csv write.table
#' @importFrom stats runif
#' @importFrom FME Latinhyper
#' @importFrom dplyr mutate
#' @importFrom sensobol sobol_matrices
#'
#' @return list; ctrl which was supplied with updated arguments if missing.
#'
#' @examples
#' \dontrun{
#'   # Run sensitivity analysis
#'   tmpdir <- tempdir()
#'   aeme_dir <- system.file("extdata/lake/", package = "AEME")
#'   # Copy files from package into tempdir
#'   file.copy(aeme_dir, tmpdir, recursive = TRUE)
#'   path <- file.path(tmpdir, "lake")
#'   aeme_data <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
#'   mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
#'   inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
#'   outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
#'   model <- c("glm_aed")
#'   aeme_data <- AEME::build_ensemble(path = path, aeme_data = aeme_data,
#'                                     model = model, mod_ctrls = mod_ctrls,
#'                                     inf_factor = inf_factor, ext_elev = 5,
#'                                     use_bgc = FALSE)
#'
#'   # Load parameters
#'   utils::data("aeme_parameters", package = "aemetools")
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
#'   ctrl <- list(N = 2^3, ncore = 2L, na_value = 999, parallel = TRUE,
#'                out_file = "results.db",
#'                vars_sim = list(
#'                  surf_temp = list(var = "HYD_temp",
#'                                   month = c(10:12, 1:3),
#'                                   depth_range = c(0, 2)
#'                  ),
#'                  bot_temp = list(var = "HYD_temp",
#'                                  month = c(10:12, 1:3),
#'                                  depth_range = c(10, 13)
#'                  )
#'                )
#'   )
#'
#'   # Run sensitivity analysis AEME model
#'   ctrl <- sa_aeme(aeme_data = aeme_data, path = path, param = param,
#'                   model = model, ctrl = ctrl, mod_ctrls = mod_ctrls,
#'                   FUN_list = FUN_list)
#' }
#'
#' @export

sa_aeme <- function(aeme_data, path = ".", param, model, mod_ctrls,
                    FUN_list = NULL, ctrl = NULL, param_df = NULL) {

  # Check if vars_sim and weights are the same length
  if (is.null(ctrl)) {
    stop("ctrl must be supplied")
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
                                 method = "sa", sa_ctrl = ctrl,
                                 fit = FALSE)
    )
    message("Complete! [", format(Sys.time()), "]")
  }

  # Extract parameters for the model ----
  param <- param[param$model == model, ]
  # par_idx <- which(param$model %in% c(model))

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
    ctrl$ncore <- parallel::detectCores() - 1
    if (ctrl$ncore > nrow(param_df)) ctrl$ncore <- nrow(param_df)
  }

  suppressWarnings({
    param_list <- split(param_df, rep(1:ctrl$ncore))
  })

  # Run in parallel
  if (ctrl$parallel) {

    temp_dirs <- make_temp_dir(model, lake_dir, n = ctrl$ncore)
    # list.files(temp_dirs[1], recursive = TRUE)
    ncores <- min((parallel::detectCores() - 1), ctrl$ncore)
    nmes <- names(ctrl$vars_sim)
    message("Running sensitivity analysis in parallel using ", ncores,
            " cores with ", nrow(param_df), " parameter sets [",
            format(Sys.time()), "]")

    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl))
    varlist <- list("param", "aeme_data", "path", "model", "vars_sim",
                    "FUN_list", "mod_ctrls", "var_indices", "temp_dirs",
                    "ctrl", "weights", "var_indices", "include_wlev", "nmes")
    parallel::clusterExport(cl, varlist = varlist,
                            envir = environment())
    # message("Starting generation ", gen_n, "/", tot_gen,", ",
    #         ctrl$NP, " members. ",
    #         "[", format(Sys.time()), "]")
    print(data.frame(rbind(signif(apply(param_df, 2, mean), 4),
                           signif(apply(param_df, 2, median), 4),
                           signif(apply(param_df, 2, sd), 4)),
                     row.names = c("mean", "median", "sd")))
    # model_out <- lapply(seq_along(param_list), \(pars, i) {
    model_out <- parallel::parLapply(cl, seq_along(param_list), \(pars, i) {

      path <- temp_dirs[i]
      # Add columns to pars
      pars[[i]][["fit"]] <- NA
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
                                      method = "sa", sa_ctrl = ctrl,
                                      weights = weights)

        for (n in nmes) {
          pars[[i]][[n]][p] <- res[[n]]
        }

        if (ctrl$na_value %in% unlist(res)) {
          res1 <- ctrl$na_value
        } else {
          res1 <- sum(unlist(res))
          res1 <- ifelse(is.na(res1), ctrl$na_value, res1)
        }

        pars[[i]][["fit"]][p] <- res1
        # print(pars[[i]][["fit"]][p])
      }
      return(pars[[i]])
    }, pars = param_list)

    message("Complete! [", format(Sys.time()), "]")

    g1 <- do.call(rbind, model_out)
    out_df <- apply(g1, 2, signif, digits = 6)
    write_calib_output(x = out_df, file = file.path(path, ctrl$out_file),
                       name = "sa_output")

  } else {
    # Run in serial ----
    message("Running sensitivity analysis in serial with ", nrow(param_df),
            " parameter sets [", format(Sys.time()), "]")

    print(data.frame(rbind(signif(apply(param_df, 2, mean), 4),
                           signif(apply(param_df, 2, median), 4),
                           signif(apply(param_df, 2, sd), 4)),
                     row.names = c("mean", "median", "sd")))
    nmes <- names(ctrl$vars_sim)
    model_out <- lapply(seq_along(param_list), \(pars, i) {

      # Add columns to pars
      pars[[i]][["fit"]] <- NA
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
                           fit = TRUE, method = "sa", sa_ctrl = ctrl,
                           weights = weights)

        for (n in nmes) {
          pars[[i]][[n]][p] <- res[[n]]
        }

        if (ctrl$na_value %in% unlist(res)) {
          res1 <- ctrl$na_value
        } else {
          res1 <- sum(unlist(res))
          res1 <- ifelse(is.na(res1), ctrl$na_value, res1)
        }

        pars[[i]][["fit"]][p] <- res1
        # print(pars[[i]][["fit"]][p])
        # print(pars[[i]][p, ])
      }
      # out_df <- apply(pars[[i]], 2, signif, digits = 6)
      # write_calib_output(x = out_df, file = file.path(path, ctrl$out_file),
      #                    name = "sa_output")

      return(pars[[i]])
    }, pars = param_list)

    g1 <- do.call(rbind, model_out)
    out_df <- apply(g1, 2, signif, digits = 6)
    write_calib_output(x = out_df, file = file.path(path, ctrl$out_file),
                       name = "sa_output")

    message("Complete! [", format(Sys.time()), "]")
  }
  invisible(ctrl)
}
