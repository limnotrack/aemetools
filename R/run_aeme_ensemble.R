#' Run AEME ensemble
#'
#' Run a parameter ensemble
#'
#' @inheritParams calib_aeme
#'
#' @inherit AEME::run_aeme return
#' @export
#'

run_aeme_ensemble <- function(aeme, sim_id, param, calib = NULL,
                              fit_col = "fit", ctrl) {

  # Check inputs
  if (!is(aeme, "aeme")) stop("aeme must be an aeme object")
  if (!is.character(sim_id)) stop("sim_id must be a character")
  if (!is.data.frame(param)) stop("param must be a data frame")

  if (is.null(calib)) {
    calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  }

  model <- names(sim_id)
  cfg <- AEME::configuration(aeme)
  model_controls <- cfg$model_controls
  outp <- AEME::output(aeme)
  lke <- AEME::lake(aeme)
  lakename <- tolower(lke[["name"]])
  lake_dir <- file.path(path, paste0(lke$id, "_", lakename))

  mod_list <- lapply(model, \(m) {
  # Catch if calib is NULL

    model_pars <- param |>
      dplyr::filter(model == m)

    # Identify the fixed and variable columns
    fixed_cols <- c("gen", "run", "fit_value")

    # Select parameters below the cutoff
    sid <- sim_id[m]
    sel_pars <- calib$simulation_data |>
      dplyr::filter(sim_id == sid & fit_type == fit_col) |>
      tidyr::pivot_wider(id_cols = dplyr::all_of(fixed_cols),
                         names_from = parameter_name,
                         values_from = parameter_value)
    variable_cols <- setdiff(names(sel_pars), fixed_cols)

    # Filter out duplicates based on the variable columns
    filtered_sel_pars <- sel_pars |>
      dplyr::distinct(dplyr::across(dplyr::all_of(variable_cols)),
                      .keep_all = TRUE)

    print(filtered_sel_pars)

    cutoff_value <- quantile(filtered_sel_pars$fit_value, ctrl$cutoff)
    # filt_pars <- filtered_sel_pars |>
    #   dplyr::filter(fit_value <= cutoff_value) |>
    #   dplyr::rename(fit = fit_value)
    # ctrl <- list(na_value = 999,)
    param_df <- filtered_sel_pars |>
      dplyr::select(-gen, - run) |>
      dplyr::rename(fit = fit_value)

    new_params <- next_gen_params(param_df = param_df, param = model_pars,
                                  ctrl = ctrl, add_mutation = FALSE,
                                  keep_best_pars = TRUE)

    if (is.null(ctrl$ncore)) {
      ctrl$ncore <- parallel::detectCores() - 1
      if (ctrl$ncore > nrow(new_params)) ctrl$ncore <- nrow(new_params)
    }

    # Correct N of splits if ncore is greater than number of parameters
    splts <- min(ctrl$NP, ctrl$ncore)

    suppressWarnings({
      param_list <- split(new_params, 1:splts)
    })

    # Loop through each of the parameters
    if (ctrl$parallel) {

      temp_dirs <- make_temp_dir(m, lake_dir, n = ctrl$ncore)
      # list.files(temp_dirs[1], recursive = TRUE)
      ncores <- min((parallel::detectCores() - 1), ctrl$ncore, ctrl$NP)
      cl <- parallel::makeCluster(ncores)
      on.exit(parallel::stopCluster(cl))
      varlist <- list("param_list", "aeme", "path", "m", "model_pars",
                      "temp_dirs","ctrl")
      parallel::clusterExport(cl, varlist = varlist,
                              envir = environment())
      message("Running an ensemble of ", m, " with ", ctrl$NP,
              " members using ", ctrl$ncore, " cores. ", "[",
              format(Sys.time()), "]")

      # Run the ensemble in parallel
      out_list <- parallel::parLapply(cl = cl, seq_along(param_list), \(pars, i) {

        path <- temp_dirs[i]
        # Update the parameter value in the parameter table
        pars_out <- lapply(seq_len(nrow(pars[[i]])), \(p) {
          for(n in names(pars[[i]])) {
            grp <- strsplit(n, "/")[[1]][1]
            nme <- paste0(strsplit(n, "/")[[1]][-1], collapse = "/")
            if (grp != "NA") {
              model_pars$value[model_pars$name == nme &
                                 model_pars$group == grp] <- pars[[i]][p, n]
            } else {
              model_pars$value[model_pars$name == nme] <- pars[[i]][p, n]
            }
          }
          # message(i, ", ", p)

          a2 <- aemetools::run_aeme_param(aeme = aeme, param = model_pars, model = m,
                                          path = path, na_value = ctrl$na_value,
                                          return_aeme = TRUE)
          outp2 <- AEME::output(a2)
          outp2[["ens_001"]][[m]]
        })
      }, pars = param_list)
      out_list <- unlist(out_list, recursive = FALSE)
    } else {
      out_list <- lapply(seq_len(nrow(new_params)), \(p) {

        # Update the parameter value in the parameter table
        for(n in names(new_params)) {
          grp <- strsplit(n, "/")[[1]][1]
          nme <- paste0(strsplit(n, "/")[[1]][-1], collapse = "/")
          if (grp != "NA") {
            model_pars$value[model_pars$name == nme &
                               model_pars$group == grp] <- new_params[p, n]
          } else {
            model_pars$value[model_pars$name == nme] <- new_params[p, n]
          }
        }
        # message(i, ", ", p)

        a2 <- aemetools::run_aeme_param(aeme = aeme, param = model_pars, model = m,
                                        path = path, na_value = ctrl$na_value,
                                        return_aeme = TRUE)
        outp2 <- AEME::output(a2)
        outp2[["ens_001"]][[m]]
      })
    }
    out_list
  })
  names(mod_list) <- model

  # Add output to the AEME object
  ens_n <- outp$n_members
  outp <- list()
  for (i in 1:ctrl$NP) {
    ens_n <- i # ens_n + 1
    ens_lab <- paste0("ens_", sprintf("%03d", ens_n))
    outp[[ens_lab]] <- list(dy_cd = mod_list[["dy_cd"]][[i]],
                            glm_aed = mod_list[["glm_aed"]][[i]],
                            gotm_wet = mod_list[["gotm_wet"]][[i]])
    outp$n_members <- ens_n
  }

  AEME::output(aeme) <- outp
  return(aeme)
}
