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
#'
#' @return A single value of model fit, calculated by `FUN_list`.
#'
#' @importFrom dplyr case_when
#' @importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
#' @importFrom lubridate as_date
#' @importFrom AEME lake input observations get_var_indices
#' @importFrom reshape2 melt
#' @importFrom stats approx
#' @importFrom utils data
#' @importFrom graphics points
#'
#' @export

run_and_fit <- function(aeme, param, model, vars_sim, path,
                        model_controls = NULL,
                        FUN_list = NULL, weights, na_value = 999,
                        var_indices = NULL, return_indices = FALSE,
                        include_wlev = FALSE, return_df = FALSE,
                        method = "calib", sa_ctrl = NULL,
                        fit = TRUE) {

  return_nc <- ifelse(fit | return_indices, TRUE, FALSE)
  if (is.null(model_controls)) {
    config <- AEME::configuration(aeme = aeme)
    model_controls <- config$model_controls
  }

  # Load data from AEME package ----
  utils::data("key_naming", package = "AEME", envir = environment())

  if (any(vars_sim %in% key_naming$name[key_naming$derived])) {
    lke <- AEME::lake(aeme)
    if (lke$depth < 10) {
      z_step <- 0.2
    } else {
      z_step <- 0.5
    }
    inp <- AEME::input(aeme)
    bathy <- inp$hypsograph |>
      dplyr::filter(depth <= 0)
    bathy$depth <- max(bathy$elev) - bathy$elev
    bthD <- (bathy$depth)
    max_dep <- max(bthD)
    bthA <- (bathy$area)
    laz_fun_list <- list(HYD_thmcln = rLakeAnalyzer::thermo.depth,
                         HYD_ctrbuy = rLakeAnalyzer::center.buoyancy,
                         HYD_epidep = rLakeAnalyzer::meta.depths,
                         HYD_hypdep = rLakeAnalyzer::meta.depths)
  }

  nc <- run_aeme_param(aeme = aeme, param = param, model = model,
                       path = path, model_controls = model_controls,
                       na_value = na_value, return_nc = return_nc)

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

  if (!is.list(nc)) {
    message("Error opening netCDF file. Returning na_value.")
    return(return_list)
  }
  if (nc$error) {
    message("Error opening netCDF file. Returning na_value.")
    return(return_list)
  }
  # If error in running model, return na_value
  if (is.null(nc)) {
    message("Error opening netCDF file. Returning na_value.")
    return(return_list)
  }
  on.exit({
    ncdf4::nc_close(nc)
  })

  if (fit | return_indices) {
    # Load AEME data
    lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)
    inp <- AEME::input(aeme)
    obs <- AEME::observations(aeme)
    wbal <- AEME::water_balance(aeme)
    aeme_time <- AEME::time(aeme)
    if (!is.null(obs$lake))
      obs$lake$depth_mid <- (obs$lake$depth_to + obs$lake$depth_from) / 2


    # Default function ----
    if (is.null(FUN_list)) {
      FUN_list <- function(df) {
        mean(abs(df$model - df$obs))
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

    # Dimensions which will vary
    if (model == "dy_cd") {
      lyrs <- tryCatch({
        ncdf4::ncvar_get(nc, "dyresmLAYER_HTS_Var")
      }, error = \(e) return(NULL))
      if (is.null(lyrs)) {
        return(return_list)
      }
      lyrs <- lyrs[nrow(lyrs):1, ]
      NS <- ncdf4::ncvar_get(nc, "dyresmLayerCount")
    } else if(model == "glm_aed") {
      NS <- ncdf4::ncvar_get(nc, "NS")
      lyrs <- ncdf4::ncvar_get(nc, "z")
      lyrs[lyrs > 1000000] <- NA
    } else if (model == 'gotm_pclake' | model == "gotm_wet") {
      z <- ncdf4::ncvar_get(nc, "z")
      zi <- ncdf4::ncvar_get(nc, "zi")
    }

    # Get variable date & depth indices ----
    if (is.null(var_indices)) {
      if (method == "calib") {
        var_indices <- AEME::get_var_indices(nc = nc, model = model,
                                             aeme = aeme, path = path,
                                             vars_sim = vars_sim)
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

          deriv_var <- key_naming$derived[key_naming$name %in% v]
          if (deriv_var) {
            v0 <- key_naming$derived_from[key_naming$name %in% v]
            v1 <- key_naming[key_naming$name %in% v0, model]
          } else {
            v1 <- key_naming[key_naming$name %in% v, model]
          }
          v1 <- ifelse(model == "dy_cd", paste0("dyresm", v1, "_Var"), v1)
          v1 <- ifelse(model == "dy_cd",
                       paste0("dyresm", key_naming[key_naming$name %in% v, model],
                              "_Var"),
                       v1)

          this.var <- ncdf4::ncvar_get(nc, v1)
          if (model == "dy_cd") {
            this.var <- this.var[nrow(this.var):1, ]
          }

          if(length(var_indices[[v]][["depths"]]) == 0 |
             length(var_indices[[v]][["time"]]) == 0 |
             is.null(ncol(this.var))) {
            return(return_list)
          }

          conv.fact <- ifelse(model == "glm_aed",
                              key_naming[key_naming$name == v, "conversion_aed"],
                              1)

          na_idx <- which(apply(this.var, 2, \(x) all(is.na(x))))

          out <- sapply(var_indices[[v]][["time"]], FUN = \(i) {
            if (i > ncol(this.var)) {
              return(rep(na_value, length(var_indices[[v]][["depths"]])))
            }
            if (all(is.na(this.var[, i])) | sum(!is.na(this.var[, i])) == 1) {
              return(rep(na_value, length(var_indices[[v]][["depths"]])))
            }

            if (deriv_var) {
              out_deps <- bthD
            } else {
              out_deps <- var_indices[[v]][["depths"]]
            }

            if (model %in% c("glm_aed", "dy_cd")) {
              z <- c(0, lyrs[1:NS[i], i])
              z <- max(z) - z
              elevs_mid <- stats::approx(z, n = length(z)*2 - 1)$y # extract mid layer depth
              elevs_mid <- elevs_mid[-which(elevs_mid %in% z)]
              vals <- stats::approx(x = elevs_mid, y = this.var[1:NS[i], i],
                                    xout = out_deps, rule = 2)$y
            } else if(model == "gotm_wet") {
              vals <- stats::approx(x = z[, i], y = this.var[, i],
                                    xout = out_deps, rule = 2)$y
            }
            if (!deriv_var) {
              return(vals)
            } else {
              if (v == "HYD_schstb") {
                idx2 <- which(!is.na(vals))
                # if (length(idx2) <= 1) return(NA)
                v_out <- rLakeAnalyzer::schmidt.stability(wtr = vals[idx2],
                                                      depths = out_deps[idx2],
                                                      bthA = bthA, bthD = bthD)
                v_out[is.nan(v_out)] <- NA
                v_out
              } else if (v %in% names(laz_fun_list)) {
                idx <- ifelse(v == "HYD_hypdep", 2, 1)
                idx2 <- which(!is.na(vals))
                if (length(idx2) <= 1) return(NA)
                v_out <- laz_fun_list[[v]](wtr = vals[idx2], depths = out_deps[idx2])
                v_out[is.nan(v_out)] <- NA
                v_out <- ifelse(is.na(v_out) & v == "HYD_thmcln", max_dep,
                                v_out)
                v_out[idx]
              } else if (v == "CHM_oxynal") {
                idx2 <- which(!is.na(vals))
                if (length(idx2) <= 1) return(NA)

                oxy_layers <- approx(y = vals[idx2], x = out_deps[idx2],
                                     xout = seq(0, max(out_deps[idx2]),
                                                by = z_step), rule = 2)$y
                sum(oxy_layers < 1)
              }
            }
          })
          if ("numeric" %in% class(out)) {
            out <- matrix(out, nrow = 1, ncol = length(out))
          }
          out <- out * conv.fact
          rownames(out) <- var_indices[[v]][["depths"]]
          colnames(out) <- as.character(var_indices[[v]][["dates"]])
          out2 <- reshape2::melt(out, value.name = "model",
                                 varnames = c("depth_mid", "Date"))
          out2$Date <- as.Date(out2$Date)
          out2$var_aeme <- v
          return(out2)
        })
      } else if (method == "sa") {
        nmes <- names(sa_ctrl$var)
        names(nmes) <- nmes
        vars_out <- lapply(nmes, \(n) {
          deriv_var <- key_naming$derived[key_naming$name %in%
                                            sa_ctrl$vars_sim[[n]]$var]
          if (deriv_var) {
            v0 <- key_naming$derived_from[key_naming$name %in%
                                            sa_ctrl$vars_sim[[n]]$var]
            v1 <- key_naming[key_naming$name %in% v0, model]
          } else {
            v1 <- key_naming[key_naming$name %in% sa_ctrl$vars_sim[[n]]$var,
                        model]
          }
          v1 <- ifelse(model == "dy_cd", paste0("dyresm", v1, "_Var"), v1)

          if (sa_ctrl$vars_sim[[n]]$var == "LKE_lvlwtr") {
            if (model == "dy_cd") {
              mod_layers <- ncdf4::ncvar_get(nc, "dyresmLAYER_HTS_Var")
              depth <- apply(mod_layers, 2, \(x) max(x, na.rm = TRUE))
            } else if (model == "glm_aed") {
              depth <- ncdf4::ncvar_get(nc, "lake_level")
            } else if (model == "gotm_wet") {
              zi <- ncdf4::ncvar_get(nc, "zi")
              depth <- zi[nrow(zi), ] - zi[1, ]
              depth[depth <= 0] <- 0
            }
            # depth <- depth[var_indices[[n]][["time"]]]
            df <- data.frame(depth_mid = NA,
                             Date = var_indices[[n]][["dates"]],
                             model = depth[var_indices[[n]][["time"]]],
                             var_aeme = "LKE_lvlwtr",
                             name = n)
            return(df)
          }
          this.var <- ncdf4::ncvar_get(nc, v1)
          if (model == "dy_cd") {
            this.var <- this.var[nrow(this.var):1, ]
          }

          if(length(var_indices[[n]][["depths"]]) == 0 |
             length(var_indices[[n]][["time"]]) == 0 |
             is.null(ncol(this.var))) {
            return(return_list)
          }

          conv.fact <- ifelse(model == "glm_aed",
                              key_naming[key_naming$name == sa_ctrl$vars_sim[[n]]$var, "conversion_aed"],
                              1)

          na_idx <- which(apply(this.var, 2, \(x) all(is.na(x))))

          out <- sapply(var_indices[[n]][["time"]], FUN = \(i) {
            # print(i)
            if (i > ncol(this.var)) {
              return(rep(na_value, length(var_indices[[n]][["depths"]])))
            }
            if (all(is.na(this.var[, i])) | sum(!is.na(this.var[, i])) == 1) {
              return(rep(na_value, length(var_indices[[n]][["depths"]])))
            }

            if (model %in% c("glm_aed", "dy_cd")) {
              z <- c(0, lyrs[1:NS[i], i])
              z <- max(z) - z
              elevs_mid <- stats::approx(z, n = length(z)*2 - 1)$y # extract mid layer depth
              deps <- elevs_mid[-which(elevs_mid %in% z)]
              vals <- stats::approx(x = deps, y = this.var[1:NS[i], i],
                                    xout = var_indices[[n]][["depths"]],
                                    rule = 2)$y
            } else if(model == "gotm_wet") {
              deps <- max(zi[, i]) - z[, i]
              # idx <- which(var_indices[[n]][["depths"]] < max(deps))

              vals <- stats::approx(x = deps, y = this.var[, i],
                                    xout = var_indices[[n]][["depths"]],
                                    rule = 2)$y
            }
            if (!deriv_var) {
              return(vals)
            } else {
              if (sa_ctrl$vars_sim[[n]]$var == "HYD_schstb") {
                idx2 <- which(!is.na(vals))
                # if (length(idx2) <= 1) return(NA)
                v <- rLakeAnalyzer::schmidt.stability(wtr = vals[idx2],
                                                      depths = var_indices[[n]][["depths"]][idx2],
                                                      bthA = bthA, bthD = bthD)
                v[is.nan(v)] <- NA
                v
              } else if (sa_ctrl$vars_sim[[n]]$var %in% names(laz_fun_list)) {
                idx <- ifelse(sa_ctrl$vars_sim[[n]]$var == "HYD_hypdep", 2, 1)
                idx2 <- which(!is.na(vals))
                if (length(idx2) <= 1) return(NA)
                v <- laz_fun_list[[sa_ctrl$vars_sim[[n]]$var]](wtr = vals[idx2],
                                                               depths = var_indices[[n]][["depths"]][idx2])
                v[is.nan(v)] <- NA
                v[idx]
              } else if (sa_ctrl$vars_sim[[n]]$var == "CHM_oxynal") {
                idx2 <- which(!is.na(vals))
                if (length(idx2) <= 1) return(NA)

                oxy_layers <- approx(y = vals[idx2], x = var_indices[[n]][["depths"]][idx2],
                                     xout = seq(0, max(var_indices[[n]][["depths"]][idx2]), by = z_step), rule = 2)$y
                sum(oxy_layers < 1)
              }
            }
          })
          if ("numeric" %in% class(out)) {
            out <- matrix(out, nrow = 1, ncol = length(out))
          }
          if ("list" %in% class(out)) {
            out <- do.call(cbind, out)
          }
          if (deriv_var) {
            out2 <- data.frame(Date = var_indices[[n]][["dates"]],
                               depth_mid = NA,
                               var_aeme = sa_ctrl$vars_sim[[n]]$var,
                               name = n,
                               model = as.vector(unlist(out)))
            return(out2)
          }
          out <- out * conv.fact
          rownames(out) <- var_indices[[n]][["depths"]]
          colnames(out) <- as.character(var_indices[[n]][["dates"]])
          out2 <- reshape2::melt(out, value.name = "model",
                                 varnames = c("depth_mid", "Date"))
          out2$Date <- as.Date(out2$Date)
          out2$var_aeme <- sa_ctrl$vars_sim[[n]]$var
          out2$name <- n
          return(out2)
        })
      }

      mod_out <- do.call(rbind, vars_out)
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
      if (!is.null(obs$level)) {
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
        LID = NA, var_aeme = "DEPTH", depth_mid = NA,
        depth_from = NA, diff = model - value) |>
        dplyr::filter(!is.na(diff)) |>
        dplyr::select(LID, Date, value, var_aeme, depth_mid, depth_from, model,
                      diff) |>
        dplyr::rename(obs = value)
    }

    if (!is.null(obs$lake) & length(vars_sim) > 0) {

      if (method == "calib") {
        obs_sub <- obs$lake |>
          dplyr::filter(Date %in% mod_out$Date) |>
          dplyr::rename(obs = value)

        if (nrow(obs_sub) < 1) {
          message("No observational data present.")
          return(return_list)
        } #else {
        tst <- dplyr::left_join(obs_sub, mod_out,
                                by = c("Date", "depth_mid", "var_aeme")) |>
          # dplyr::filter(!is.na(model)) |>
          dplyr::filter(var_aeme %in% vars_sim) |>
          # dplyr::mutate(
          #   obs = dplyr::case_when(
          #     is.na(obs) ~ na_value,
          #     .default = obs
          #   ),
          #   model = dplyr::case_when(
          #     is.na(model) ~ na_value,
          #     .default = model
          #   )) |>
          dplyr::mutate(diff = model - obs)
      } else {
          tst <- mod_out
      }

      if (nrow(tst) == 0) {
        return(return_list)
      }

      if (return_df) {
        return(tst)
      } else {

        if (method == "calib") {
          vars_present <- unique(tst$var_aeme)
          names(vars_present) <- vars_present
          res <- lapply(vars_present, \(v) {
            sub <- tst |>
              dplyr::filter(var_aeme == v)
            FUN_list[[v]](sub)
          })
          for (v in names(res)) {
            return_list[[v]] <- res[[v]]  * weights[[v]]
          }
        } else if (method == "sa") {
          nmes_present <- unique(tst$name)
          names(nmes_present) <- nmes_present
          res <- lapply(nmes_present, \(n) {
            sub <- tst |>
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
