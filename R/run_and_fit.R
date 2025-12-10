#' Run a model and calculate model fit.
#'
#' @inheritParams AEME::run_aeme
#' @inheritParams run_aeme_param
#' @inheritParams calib_aeme
#' @param param dataframe; of parameters read in from a csv file. Requires the
#' columns c("model", "file", "name", "value", "min", "max", "log")
#' @param model string; for which model. Options are c("dy_cd", "glm_aed" and
#'  "gotm_wet")
#' @param vars_sim vector; of variables names to be used in the calculation of
#' model fit. Currently only supports using one variable.
#' @param FUN_list function; of the form `function(O, P)` which will be used in
#'  to calculate model fit. If NULL, uses mean absolute error (MAE).
#' @param var_indices list; generated from running `run_and_fit()` with
#' `return indices = TRUE` on the first simulation.
#' @param return_indices boolean; return the indices (depths, time and dates)
#' of each variable. Used when running calibration and the time period does not
#'  change between simulations.
#' @param return_df boolean; return dataframe of modelled and observed.
#' @param weights vector; of weights to be used in the calculation of model fit.
#' @param na_value numeric; value to be returned if model fails to run.
#' @param include_wlev boolean; include water level in the calculation of model
#' fit.
#' @param method string; of the method of the model run. Options are c("sa",
#'  "calib").
#' @param fit boolean; fit model or not. If FALSE, only return netCDF file
#' connection.
#' @param sa_ctrl list; of control parameters for the sensitivity analysis.
#' Only required if `method = "sa"`.
#' @param timeout numeric; time in seconds to run each simulation. Default is
#' Inf.
#'
#' @return A single value of model fit, calculated by `FUN_list`.
#'
#' @importFrom dplyr case_when filter left_join mutate select bind_rows
#' @importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
#' @importFrom lubridate as_date
#' @importFrom AEME lake input observations get_var_indices read_model_outputs
#' @importFrom AEME get_deriv_inputs add_deriv_output is_model_error
#' @importFrom cli cli_alert_warning cli_alert_info cli_div
#' @importFrom reshape2 melt
#' @importFrom stats approx
#' @importFrom utils data
#' @importFrom graphics points
#' @importFrom rLakeAnalyzer thermo.depth center.buoyancy meta.depths
#' @importFrom rLakeAnalyzer schmidt.stability
#'
#' @export

run_and_fit <- function(aeme, param, model, vars_sim, path,
                        model_controls = NULL,
                        FUN_list = NULL, weights, na_value = 999,
                        var_indices = NULL, return_indices = FALSE,
                        include_wlev = FALSE, return_df = FALSE,
                        method = "calib", sa_ctrl = NULL,
                        fit = TRUE, timeout = Inf) {
  
  return_nc <- ifelse(fit | return_indices, TRUE, FALSE)
  if (is.null(model_controls)) {
    config <- AEME::configuration(aeme = aeme)
    model_controls <- config$model_controls
  }
  if (missing(weights)) {
    cli::cli_inform("No weights supplied. Defaulting to 1 for all variables.")
    weights <- set_weights(vars_sim = vars_sim)
  }
  if (include_wlev & !"LKE_lvlwtr" %in% names(weights)) {
    weights["LKE_lvlwtr"] <- 1
    cli::cli_alert_info("Including water level in model fit with weight of 1.")
  }
  if (include_wlev & !"LKE_lvlwtr" %in% names(FUN_list)) {
    FUN_list[["LKE_lvlwtr"]] <- FUN_list[[1]]
    cli::cli_alert_info("Including water level in model fit using first 
                        function in FUN_list.")
  }
  
  # Create a list for the return values
  return_list <- list()
  if (method == "calib") {
    for (v in vars_sim) {
      return_list[[v]] <- na_value
    }
  } else if (method == "sa") {
    for (n in names(sa_ctrl$vars_sim)) {
      return_list[[n]] <- na_value
    }
  }
  
  
  # Load data from AEME package ----
  data("key_naming", package = "AEME", envir = environment())
  inp <- AEME::input(aeme)
  hyps <- inp$hypsograph
  
  nc <- run_aeme_param(aeme = aeme, param = param, model = model,
                       path = path, model_controls = model_controls,
                       na_value = na_value, return_nc = return_nc,
                       timeout = timeout)
  # if nc is not ncdf4 object, return return_list
  if (!is.list(nc) & !inherits(nc, "ncdf4")) {
    cli::cli_alert_warning("Error opening netCDF file. Returning na_value.")
    return(return_list)
  }
  
  on.exit({
    ncdf4::nc_close(nc)
  })
  

  
  if (!is.list(nc)) {
    cli::cli_alert_warning("Error opening netCDF file. Returning na_value.")
    return(return_list)
  }
  if (nc$error) {
    cli::cli_alert_warning("Error opening netCDF file. Returning na_value.")
    return(return_list)
  }
  # If error in running model, return na_value
  if (is.null(nc)) {
    cli::cli_alert_warning("Error opening netCDF file. Returning na_value.")
    return(return_list)
  }
  
  if (fit | return_indices) {
    # Load AEME data
    lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)
    inp <- AEME::input(aeme)
    obs <- AEME::observations(aeme)
    wbal <- AEME::water_balance(aeme)
    aeme_time <- AEME::time(aeme)
    if (!is.null(obs$lake)) {
      obs$lake$depth <- (obs$lake$depth_to + obs$lake$depth_from) / 2
    }
    
    
    # Default function ----
    if (is.null(FUN_list)) {
      FUN_list <- function(df) {
        mean(abs(df$model - df$obs), na.rm = TRUE)
      }
    }
    
    if (method == "calib") {
      if (include_wlev) {
        wlev_weight <- weights[["LKE_lvlwtr"]]
        vars_sim <- vars_sim[vars_sim != "LKE_lvlwtr"]
        weights <- weights[names(weights) != "LKE_lvlwtr"]
      } else {
        vars_sim <- vars_sim[vars_sim != "LKE_lvlwtr"]
        weights <- weights[names(weights) != "LKE_lvlwtr"]
      }
    }
    
    if (return_indices) {
      var_indices <- NULL
    }

    # Get variable date & depth indices ----
    if (is.null(var_indices)) {
      if (method == "calib") {
        all_vars <- AEME::get_vars_sim(vars_sim = vars_sim)
        var_indices <- AEME::get_var_indices(nc = nc, model = model,
                                             aeme = aeme, path = path,
                                             vars_sim = all_vars)
      } else if (method == "sa") {
        nmes <- names(sa_ctrl$var)
        names(nmes) <- nmes
        var_indices <- lapply(nmes, \(n) {
          AEME::get_var_indices(nc = nc, model = model, aeme = aeme,
                                path = path,
                                vars_sim = sa_ctrl$vars_sim[[n]]$var,
                                month = sa_ctrl$vars_sim[[n]]$month,
                                depth_range = sa_ctrl$vars_sim[[n]]$depth_range)[[1]]
        })
      }
      if (return_indices) {
        return(var_indices)
      }
    }
    
    # Extract model variables ----
    if (length(vars_sim) > 0) {
      if (method == "calib") {
        vars_out <- lapply(vars_sim, \(v) {
          
          deriv_chk <- key_naming |> 
            dplyr::filter(name == v) |>
            dplyr::pull(derived)
          if (deriv_chk) {
            extract_var <- AEME::get_deriv_inputs(v)
            deriv_var <- v
          } else {
            extract_var <- v
          }
          # Pull out the dimensions
          depths <- var_indices[[extract_var[1]]][["depths"]]
          date_index <- var_indices[[extract_var[1]]][["date_index"]]
          dates <- var_indices[[extract_var[1]]][["dates"]]
          
          if (length(depths) == 0 | length(date_index) == 0) {
            return(return_list)
          }
          
          out <- AEME::read_model_outputs(nc = nc, lake_dir = lake_dir,
                                          model = model, vars_sim = extract_var, 
                                          depths = depths, 
                                          date_index = date_index, 
                                          incl_fluxes = FALSE)
          
          if (AEME::is_model_error(out)) {
            cli::cli_div(theme = list(span.emph = list(color = "red")))
            cli::cli_alert_warning("Error reading model outputs for variable
                                   {v}: {.emph {out$reason}}. Returning
                                   na_value.")
            return(return_list)
          }
          
          if (deriv_chk) {
            out <- AEME::add_deriv_output(out_list = out, hyps = hyps, 
                                          vars_sim = deriv_var)
          }
          out <- out[[v]]
          if (is.null(nrow(out))) {
            if (length(out) < length(dates)) {
              cli::cli_alert_warning("Mismatch in number of dates and model output
                                     for variable {v}. Returning na_value.")
              dates <- dates[1:length(out)]
            }
            depths <- NA_real_
            each <- 1
          } else {
            if (ncol(out) != length(dates)) {
              cli::cli_alert_warning("Mismatch in number of dates and model output
                                     for variable {v}. Returning na_value.")
              dates <- dates[1:ncol(out)]
            }
            each <- length(depths)
          }
          
          
          conv_fact <- ifelse(model == "glm_aed",
                              key_naming[key_naming$name == v, "conversion_aed"],
                              1)
          
          if (!is.matrix(out)) {
            out <- matrix(out, nrow = 1, ncol = length(out))
          }
          out <- out * conv_fact
          
          # Build long dataframe for 2D variable
          out2 <- data.frame(
            Date     = rep(dates, each = each),
            depth    = as.vector(depths),
            model    = as.vector(out),
            var_aeme = v
          )
          return(out2)
        })
      } else if (method == "sa") {
        nmes <- names(sa_ctrl$var)
        names(nmes) <- nmes
        vars_out <- lapply(nmes, \(n) {
          v <- sa_ctrl$vars_sim[[n]]$var
          # Pull out the dimensions
          depths <- var_indices[[n]][["depths"]]
          date_index <- var_indices[[n]][["date_index"]]
          dates <- var_indices[[n]][["dates"]]
          
          
          if (v == "LKE_lvlwtr") {
            depth <- AEME::read_model_outputs(nc = nc, lake_dir = lake_dir,
                                              model = model, 
                                              vars_sim = "HYD_temp", 
                                              date_index = date_index, 
                                              incl_fluxes = FALSE)
            
            if (AEME::is_model_error(depth)) {
              cli::cli_div(theme = list(span.emph = list(color = "red")))
              cli::cli_alert_warning("Error reading model outputs for variable
                                   {v}: {.emph {out$reason}}. Returning
                                   na_value.")
              return(return_list)
            }
            
            df <- data.frame(Date = var_indices[[n]][["dates"]],
                             depth = NA, 
                             model = depth[["LKE_lvlwtr"]],
                             var_aeme = "LKE_lvlwtr",
                             name = n)
            return(df)
          }
          
          deriv_chk <- key_naming |> 
            dplyr::filter(name == v) |>
            dplyr::pull(derived)
          if (deriv_chk) {
            extract_var <- AEME::get_deriv_inputs(v)
            deriv_var <- v
          } else {
            extract_var <- v
          }
          
          if (length(depths) == 0 | length(date_index) == 0) {
            return(return_list)
          }
          
          out <- AEME::read_model_outputs(nc = nc, lake_dir = lake_dir,
                                          model = model, vars_sim = extract_var, 
                                          depths = depths, 
                                          date_index = date_index, 
                                          incl_fluxes = FALSE)
          if (AEME::is_model_error(out)) {
            cli::cli_div(theme = list(span.emph = list(color = "red")))
            cli::cli_alert_warning("Error reading model outputs for variable
                                   {v}: {.emph {out$reason}}. Returning
                                   na_value.")
            return(return_list)
          }
          
          if (deriv_chk) {
            out <- AEME::add_deriv_output(out_list = out, hyps = hyps, 
                                          vars_sim = deriv_var)
          }
          out <- out[[v]]
          if (is.null(nrow(out))) {
            depths <- NA_real_
            each <- 1
          } else {
            each <- length(depths)
          }
          
          
          conv_fact <- ifelse(model == "glm_aed",
                              key_naming[key_naming$name == v, "conversion_aed"],
                              1)
          
          if (!is.matrix(out)) {
            out <- matrix(out, nrow = 1, ncol = length(out))
          }
          out <- out * conv_fact
          if ("logical" %in% class(out)) {
            out <- matrix(out, nrow = 1, ncol = length(out))
          }
          if ("numeric" %in% class(out)) {
            out <- matrix(out, nrow = 1, ncol = length(out))
          }
          if ("list" %in% class(out)) {
            out <- do.call(cbind, out)
          }
          
          # Build long dataframe for 2D variable
          out2 <- data.frame(
            Date     = rep(dates, each = each),
            depth    = as.vector(depths),
            model    = as.vector(out),
            var_aeme = v,
            name = n
          )
          return(out2)
        })
      }
      
      mod_out <- dplyr::bind_rows(vars_out)
      if (ncol(mod_out) == 1 & nrow(obs$lake) > 0) {
        return(return_list)
      }
    }
    
    
    if (include_wlev & method == "calib") {
      #### PROBABLY NEED CATCHES HERE FOR NO WATER LEVEL OUTPUT #####
      balance <- aemetools::get_wlevel(lake_dir = lake_dir, model = model,
                                       nlev = 10, return_df = TRUE)
      if (is.null(ncol(balance))) {
        return(return_list)
      } else if (any(balance[["lvl"]] <= 0) | any(is.na(balance[["lvl"]]))) {
        return(return_list)
      }
      tme <- AEME::time(aeme)
      time_check <- any(obs$level$Date > tme$start & obs$level$Date < tme$stop)
      if (!is.null(obs$level) & time_check) {
        lvl_adj <- obs$level |>
          dplyr::mutate(value = (value - min(inp$hypsograph$elev)))
      } else {
        if (!is.null(wbal$data$wbal)) {
          lvl_adj <- wbal$data$wbal |>
            dplyr::select(Date, value) |>
            dplyr::mutate(value = abs(min(inp$hypsograph$depth)),
                          var_aeme = "LKE_lvlwtr")
        } else {
          lvl_adj <- balance |>
            dplyr::select(Date) |>
            dplyr::mutate(value = (inp$init_depth),
                          var_aeme = "LKE_lvlwtr")
        }
      }
      
      df_lvl <- dplyr::left_join(balance, lvl_adj, by = "Date")
      
      df_lvl <- df_lvl |>
        dplyr::rename(model = lvl) |>
        dplyr::mutate(model = dplyr::case_when(
          is.na(model) ~ 0,
          .default = model
        ),
        LID = NA, var_aeme = "DEPTH", depth = NA,
        depth_from = NA, diff = model - value) |>
        dplyr::filter(!is.na(diff)) |>
        dplyr::select(LID, Date, value, var_aeme, depth, depth_from, model,
                      diff) |>
        dplyr::rename(obs = value)
    }
    
    if (!is.null(obs$lake) & length(vars_sim) > 0) {
      
      if (method == "calib") {
        obs_sub <- obs$lake |>
          dplyr::select(Date, depth, var_aeme, value) |>
          dplyr::filter(Date %in% mod_out$Date) |>
          dplyr::filter(var_aeme %in% vars_sim) |>
          dplyr::rename(obs = value)
        
        if (nrow(obs_sub) < 1) {
          cli::cli_alert_warning("No observational data present.")
          return(return_list)
        }
        comp_df <- obs_sub |> 
          dplyr::left_join(mod_out,
                           by = c("Date", "depth", "var_aeme")) |>
          dplyr::mutate(diff = model - obs)
      } else {
        comp_df <- mod_out
      }
      
      if (nrow(comp_df) == 0) {
        return(return_list)
      }
      
      if (return_df) {
        return(comp_df)
      } else {
        
        if (method == "calib") {
          vars_present <- unique(comp_df$var_aeme)
          names(vars_present) <- vars_present
          res <- lapply(vars_present, \(v) {
            sub <- comp_df |>
              dplyr::filter(var_aeme == v)
            FUN_list[[v]](sub)
          })
          for (v in names(res)) {
            return_list[[v]] <- res[[v]]  * weights[[v]]
          }
        } else if (method == "sa") {
          nmes_present <- unique(comp_df$name)
          names(nmes_present) <- nmes_present
          res <- lapply(nmes_present, \(n) {
            sub <- comp_df |>
              dplyr::filter(name == n)
            FUN_list[[sa_ctrl$vars_sim[[n]]$var]](sub)
          })
          for (n in names(res)) {
            return_list[[n]] <- res[[n]]
          }
        }
        
        if (include_wlev & method == "calib") {
          # Mutiply residuals by the mean difference in water level
          return_list[["LKE_lvlwtr"]] <- FUN_list$LKE_lvlwtr(df_lvl) *
            wlev_weight
        }
        return(return_list)
      }
      # }
    } else {
      # plot(df_lvl$Date, df_lvl$model, type = "l")
      # graphics::points(df_lvl$Date, df_lvl$obs, col = "red")
      res1 <- FUN_list$LKE_lvlwtr(df_lvl) * wlev_weight
      return_list[["LKE_lvlwtr"]] <- ifelse(is.nan(res1), na_value, res1)
      return(return_list)
    }
  }
}
