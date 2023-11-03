#' Run a model and calculate model fit.
#'
#' @inheritParams AEME::run_aeme
#' @inheritParams run_and_fit
#' @inheritParams run_aeme_param
#' @param param dataframe; of parameters read in from a csv file. Requires the
#' columns c("model", "file", "name", "value", "min", "max", "log")
#' @param model string; for which model. Options are c("dy_cd", "glm_aed" and
#'  "gotm_wet")
#' @param vars_sim vector; of variables names to be used in the calculation of
#' model fit. Currently only supports using one variable.
#' @param FUN function; of the form `function(O, P)` which will be used in
#'  to calculate model fit. If NULL, uses mean absolute error (MAE).
#' @param var_indices list; generated from running `run_and_fit()` with
#' `return indices = TRUE` on the first simulation.
#' @param return_indices boolean; return the indices (depths, time and dates)
#' of each variable. Used when running calibration and the time period does not
#'  change between simulations.
#' @param return_df boolean; return dataframe of modelled and observed.
#'
#' @return A single value of model fit, calculated by `FUN`.
#'
#' @importFrom dplyr case_when
#' @importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
#' @importFrom lubridate as_date
#' @importFrom AEME lake input observations
#' @importFrom reshape2 melt
#'
#' @export
#'

run_and_fit <- function(aeme_data, param, model, vars_sim, path, mod_ctrls,
                        FUN = NULL, weights, na_value = 999, var_indices = NULL,
                        return_indices = FALSE, include_wlev = FALSE,
                        return_df = FALSE, fit = TRUE) {

  return_nc <- ifelse(fit | return_indices, TRUE, FALSE)

  nc <- run_aeme_param(aeme_data = aeme_data, param = param, model = model,
                       path = path, mod_ctrls = mod_ctrls,
                       na_value = na_value, return_nc = return_nc)
  on.exit({
    ncdf4::nc_close(nc)
  })
  if (nc$error) {
    message("Error opening netCDF file. Returning na_value.")
    return(na_value)
  }
  # If error in running model, return na_value
  if (is.null(nc)) {
    return(na_value)
  }

  if (fit | return_indices) {

    # Load data from AEME package ----
    utils::data("key_naming", package = "AEME", envir = environment())

    # Load AEME data
    lke <- AEME::lake(aeme_data)
    lakename <- tolower(lke[["name"]])
    lake_dir <- file.path(path, paste0(lke$id, "_", lakename))
    inp <- AEME::input(aeme_data)
    obs <- AEME::observations(aeme_data)
    if (!is.null(obs$lake))
      obs$lake$depth_mid <- (obs$lake$depth_to - obs$lake$depth_from) / 2


    # Default function ----
    if (is.null(FUN)) {
      FUN <- function(O, P) {
        mean(abs(P - O))
      }
    }

    if (include_wlev) {
      wlev_weight <- weights[["HYD_wlev"]]
      vars_sim <- vars_sim[vars_sim != "HYD_wlev"]
      weights <- weights[names(weights) != "HYD_wlev"]
    } else {
      vars_sim <- vars_sim[vars_sim != "HYD_wlev"]
      weights <- weights[names(weights) != "HYD_wlev"]
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
        return(na_value)
      }
      lyrs <- lyrs[nrow(lyrs):1, ]
      NS <- ncdf4::ncvar_get(nc, "dyresmLayerCount")
    } else if(model == "glm_aed") {
      NS <- ncdf4::ncvar_get(nc, "NS")
      lyrs <- ncdf4::ncvar_get(nc, "z")
      lyrs[lyrs > 1000000] <- NA
    } else if (model == 'gotm_pclake' | model == "gotm_wet") {
      z <- ncdf4::ncvar_get(nc, "z")
    }

    # Time index ----
    if (is.null(var_indices)) {
      if (model == "dy_cd") {
        dates <- as.POSIXct((ncdf4::ncvar_get(nc, 'dyresmTime') - 2415018.5) *
                              86400,
                            origin = "1899-12-30", tz = "UTC") |>
          as.Date()
      } else if (model == "glm_aed") {
        hours.since  <- ncdf4::ncvar_get(nc, "time")
        date.start <- as.POSIXct(gsub("hours since ", "",
                                      ncdf4::ncatt_get(nc, "time", "units")$value),
                                 tz = "UTC")
        dates <- as.Date(hours.since * 3600 + date.start)
      } else if (model == 'gotm_pclake' | model == "gotm_wet") {
        out.steps <- ncdf4::ncvar_get(nc, "time")
        date.start <- ncdf4::ncatt_get(nc,'time','units')$value |>
          gsub("seconds since ", "", x = _) |>
          as.POSIXct() |>
          as.Date()
        dates <- seq.Date(date.start, by = 1, length.out = length(out.steps))
      }

      names(vars_sim) <- vars_sim
      var_indices <- lapply(vars_sim, \(v) {
        obs_v <- obs$lake |>
          dplyr::filter(var == v & Date %in% dates)
        deps <- unique(obs_v$depth_mid)
        deps <- deps[order(deps)]

        date_idx <- which(dates %in% obs_v$Date)
        list(time = date_idx, depths = deps, dates = dates[date_idx])
      })

      if (return_indices) {
        return(var_indices)
      }
    }

    # Extract model variables ----
    if (length(vars_sim) > 0) {
      vars_out <- lapply(vars_sim, \(v) {

        v1 <- ifelse(model == "dy_cd",
                     paste0("dyresm", key_naming[key_naming$name %in% v, model],
                            "_Var"),
                     key_naming[key_naming$name %in% v, model])

        this.var <- ncdf4::ncvar_get(nc, v1)
        if(model == "dy_cd") {
          this.var <- this.var[nrow(this.var):1, ]
        }

        if(length(var_indices[[v]][["depths"]]) == 0 |
           length(var_indices[[v]][["time"]]) == 0 |
           is.null(ncol(this.var))) {
          return(na_value)
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

          if (model %in% c("glm_aed", "dy_cd")) {
            z <- c(0, lyrs[1:NS[i], i])
            z <- max(z) - z
            elevs_mid <- approx(z, n = length(z)*2 - 1)$y # extract mid layer depth
            elevs_mid <- elevs_mid[-which(elevs_mid %in% z)]
            approx(x = elevs_mid, y = this.var[1:NS[i], i],
                   xout = var_indices[[v]][["depths"]], rule = 2)$y
          } else if(model == "gotm_wet") {
            approx(x = z[, i], y = this.var[, i],
                   xout = var_indices[[v]][["depths"]], rule = 2)$y
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
        out2$var <- v
        return(out2)
      })
      # Filter(Negate(is.null), vars_out)

      mod_out <- do.call(rbind, vars_out)
      if (ncol(mod_out) == 1 & nrow(obs$lake) > 0) {
        return(na_value)
      }
    }


    if (include_wlev) {
      #### PROBABLY NEED CATCHES HERE FOR NO WATER LEVEL OUTPUT #####
      balance <- aemetools::get_wlevel(lake_dir = lake_dir, model = model,
                                       nlev = 10, return_df = TRUE)
      if (is.null(ncol(balance))) {
        return(na_value)
      } else if (any(balance[["lvl"]] <= 0) | any(is.na(balance[["lvl"]]))) {
        return(na_value)
      }
      lvl_adj <- obs$level |>
        dplyr::mutate(lvlwtr = (lvlwtr - min(inp$hypsograph$elev)))

      df_lvl <- dplyr::left_join(balance, lvl_adj, by = "Date")

      df_lvl <- df_lvl |>
        dplyr::rename(value = lvlwtr, model = lvl) |>
        dplyr::mutate(model = dplyr::case_when(
          is.na(model) ~ 0,
          .default = model
        ),
        LID = NA, var = "DEPTH", depth_mid = NA,
        depth_from = NA, diff = model - value) |>
        dplyr::select(LID, Date, value, var, depth_mid, depth_from, model, diff)
    }

    if (!is.null(obs$lake) & length(vars_sim) > 0) {
      obs_sub <- obs$lake |>
        dplyr::filter(Date %in% mod_out$Date)

      if (nrow(obs_sub) < 1) {
        message("No observational data present.")
        return(na_value)
      } else {
        tst <- dplyr::left_join(obs_sub, mod_out,
                                by = c("Date", "depth_mid", "var")) |>
          dplyr::filter(!is.na(model)) |>
          dplyr::mutate(diff = model - value)

        if (nrow(tst) == 0) {
          return(na_value)
        }

        if (return_df) {
          return(tst)
        } else {

          res <- sapply(unique(tst$var), \(v) {
            sub <- tst |>
              dplyr::filter(var == v)
            FUN(O = sub$value, P = sub$model)
          })
          res <- sum(res)
          # res <- FUN(O = tst$value, P = tst$model)
          if (include_wlev) {
            # Mutiply residuals by the mean difference in water level
            # res1 <- mean(abs(df_lvl$diff))
            res1 <- FUN(O = df_lvl$value, P = df_lvl$model) * wlev_weight
            res <- res + res1
          }
          res <- ifelse(is.na(res), na_value, res)
          return(res)
        }
      }
    } else {
      res1 <- sum(abs(df_lvl$diff))
      res1 <- ifelse(is.nan(res1), na_value, res1)
      return(res1)
    }
  }
}
