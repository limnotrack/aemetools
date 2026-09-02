#' Run a model and calculate model fit.
#'
#' @inheritParams AEME::run_aeme
#' @inheritParams run_aeme_param
#' @inheritParams calib_aeme
#' @param param dataframe; of parameters read in from a csv file. Requires the
#' columns c("model", "file", "name", "value", "min", "max", "log")
#' @param model string; for which model. Options are c("dy_cd", "glm_aed",
#'  "gotm_wet", "simstrat_aed", "simstrat_aed2").
#' @param vars_sim vector; of variable names used in the calculation of model
#' fit.
#' @param FUN_list function; of the form `function(df)` using `df$model` and
#'  `df$obs`, used to calculate model fit. If NULL, uses mean absolute error
#'  (MAE).
#' @param var_indices list; generated from running `run_and_fit()` with
#' `return_indices = TRUE` on the first simulation.
#' @param return_indices boolean; return the indices (depths, time and dates)
#' of each variable. Used when running calibration and the time period does not
#'  change between simulations.
#' @param return_df boolean; return dataframe of modelled and observed.
#' @param weights vector; of weights to be used in the calculation of model fit.
#' @param na_value numeric; value to be returned if the model fails to run.
#' @param include_wlev boolean; include water level in the calculation of model
#' fit.
#' @param method string; the method of the model run. One of c("sa", "calib").
#' @param fit boolean; calculate fit. When `FALSE` it is only ever paired with
#' `return_indices = TRUE`, i.e. "run the model and hand back the indices".
#' @param sa_ctrl list; control parameters for the sensitivity analysis. Only
#' required if `method = "sa"`.
#' @param timeout numeric; time in seconds to run each simulation. Default is
#' Inf.
#'
#' @return For `method = "calib"`, a named list with one weighted fit value per
#' entry of `vars_sim` (plus `LKE_lvlwtr` when `include_wlev = TRUE`). For
#' `method = "sa"`, one value per `names(sa_ctrl$vars_sim)` sub-region, plus a
#' `failed` flag. `return_df = TRUE` instead returns the modelled/observed
#' comparison dataframe - one row per gridded observation and, when
#' `include_wlev = TRUE`, one row per water-level observation with
#' `var_aeme = "LKE_lvlwtr"` and `depth = NA`; `return_indices = TRUE`
#' returns the date/depth indices.
#'
#' @importFrom dplyr bind_rows case_when filter left_join mutate rename select
#' @importFrom ncdf4 nc_close
#'
#' @export
run_and_fit <- function(aeme, param, model, vars_sim, path,
                        model_controls = NULL,
                        FUN_list = NULL, weights, na_value = 999,
                        var_indices = NULL, return_indices = FALSE,
                        include_wlev = FALSE, return_df = FALSE,
                        method = "calib", sa_ctrl = NULL,
                        fit = TRUE, timeout = Inf) {

  return_nc <- fit || return_indices

  if (is.null(model_controls)) {
    model_controls <- AEME::configuration(aeme = aeme)$model_controls
  }
  if (missing(weights)) {
    AEME::cli_inform_safe("No weights supplied. Defaulting to 1 for all variables.")
    weights <- set_weights(vars_sim = vars_sim)
  }
  if (include_wlev && !"LKE_lvlwtr" %in% names(weights)) {
    weights["LKE_lvlwtr"] <- 1
    AEME::cli_safe("Including water level in model fit with weight of 1.",
                   FUN = cli::cli_alert_info)
  }
  if (include_wlev && !"LKE_lvlwtr" %in% names(FUN_list)) {
    FUN_list[["LKE_lvlwtr"]] <- FUN_list[[1]]
    AEME::cli_safe("Including water level in model fit using first function in
                    FUN_list.", FUN = cli::cli_alert_info)
  }

  # Return-value skeleton: one na_value slot per thing that gets scored, so
  # every early exit can hand back a correctly shaped result.
  score_names <- if (method == "sa") names(sa_ctrl$vars_sim) else vars_sim
  return_list <- stats::setNames(vector("list", length(score_names)),
                                 score_names)
  return_list[] <- na_value
  if (method == "sa") return_list$failed <- FALSE

  key_naming <- AEME::key_naming
  # Built once here rather than a dplyr::filter() per variable in the loops
  # below (this function runs once per model evaluation, hundreds of times
  # per calibration).
  deriv_lookup <- stats::setNames(key_naming$derived, key_naming$var_aeme)
  conv_lookup  <- stats::setNames(key_naming$conversion_aed, key_naming$var_aeme)

  # --- Run the model ---------------------------------------------------------
  nc <- run_aeme_param(aeme = aeme, param = param, model = model, path = path,
                       model_controls = model_controls, na_value = na_value,
                       return_nc = return_nc, timeout = timeout)

  # run_aeme_param() returns an ncdf4 handle / an open_nc_safe() wrapper list
  # on success, and na_value (numeric) or NULL on failure - so one guard
  # covers every failure mode (and the fit = FALSE, return_indices = FALSE
  # case, which never occurs but would land here as NULL).
  if (!((inherits(nc, "ncdf4") || is.list(nc)) && !isTRUE(nc$error))) {
    AEME::cli_safe(
      paste0("Error opening netCDF file. Returning {.val ", na_value, "}."),
      FUN = cli::cli_alert_warning)
    return(mark_sa_failure(return_list, method))
  }
  on.exit(try(ncdf4::nc_close(nc), silent = TRUE), add = TRUE)

  # --- Load the pieces needed to score -------------------------------------
  lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)
  inp <- AEME::input(aeme)
  hyps <- inp$hypsograph
  obs <- AEME::observations(aeme)
  if (!is.null(obs$lake)) {
    obs$lake <- normalise_lake_obs(obs$lake)
  }

  if (is.null(FUN_list)) {
    FUN_list <- function(df) mean(abs(df$model - df$obs), na.rm = TRUE)
  }

  # LKE_lvlwtr is never read as an ordinary gridded variable in a calibration
  # - it is handled by the water-level block below - so drop it from the
  # variable/weight vectors (its weight is kept aside for that block).
  wlev_weight <- NULL
  if (method == "calib") {
    if (include_wlev) wlev_weight <- weights[["LKE_lvlwtr"]]
    vars_sim <- setdiff(vars_sim, "LKE_lvlwtr")
    weights  <- weights[names(weights) != "LKE_lvlwtr"]
  }

  # --- Date & depth indices ------------------------------------------------
  if (return_indices) var_indices <- NULL
  if (is.null(var_indices)) {
    var_indices <- if (method == "sa") {
      regions <- names(sa_ctrl$vars_sim)
      stats::setNames(lapply(regions, function(n) {
        r <- sa_ctrl$vars_sim[[n]]
        AEME::get_var_indices(nc = nc, model = model, aeme = aeme, path = path,
                              vars_sim = r$var, month = r$month,
                              depth_range = r$depth_range)[[1]]
      }), regions)
    } else if (length(vars_sim) == 0) {
      # Water level is the only target. It is not a gridded variable - it was
      # stripped from `vars_sim` above and is handled by `.raf_wlev()` - so
      # there is nothing to index, and `get_var_indices()` cannot be asked
      # for an empty set (it fails building its frame from zero variables).
      list()
    } else {
      AEME::get_var_indices(nc = nc, model = model, aeme = aeme, path = path,
                            vars_sim = AEME::get_vars_sim(vars_sim = vars_sim))
    }
    if (return_indices) return(var_indices)
  }

  # --- Extract modelled values as one long dataframe -----------------------
  mod_out <- NULL
  if (length(vars_sim) > 0) {

    pieces <- if (method == "sa") {
      regions <- names(sa_ctrl$vars_sim)
      lapply(stats::setNames(regions, regions), function(n) {
        v <- sa_ctrl$vars_sim[[n]]$var
        idx <- var_indices[[n]]
        if (identical(v, "LKE_lvlwtr")) {
          out <- AEME::read_model_outputs(nc = nc, lake_dir = lake_dir,
                                          model = model, vars_sim = "HYD_temp",
                                          date_index = idx[["date_index"]],
                                          incl_fluxes = FALSE)
          if (AEME::is_model_error(out)) {
            .raf_warn_read(v, out$reason)
            return(NULL)
          }
          return(data.frame(Date = idx[["dates"]], depth = NA_real_,
                            model = out[["LKE_lvlwtr"]], var_aeme = "LKE_lvlwtr",
                            name = n, stringsAsFactors = FALSE))
        }
        .raf_extract_var(v = v, idx = idx, nc = nc, lake_dir = lake_dir,
                         model = model, deriv_lookup = deriv_lookup,
                         conv_lookup = conv_lookup, hyps = hyps, name = n)
      })
    } else {
      vs <- stats::setNames(vars_sim, vars_sim)
      lapply(vs, function(v) {
        key <- if (isTRUE(deriv_lookup[[v]])) AEME::get_deriv_inputs(v)[1] else v
        .raf_extract_var(v = v, idx = var_indices[[key]], nc = nc,
                         lake_dir = lake_dir, model = model,
                         deriv_lookup = deriv_lookup, conv_lookup = conv_lookup,
                         hyps = hyps)
      })
    }

    # A NULL piece is a variable whose output could not be extracted; drop
    # it and score the rest (it keeps its na_value slot in `return_list`,
    # which is what the previous code did too, via a malformed bind). If
    # nothing extracted, the whole run failed.
    pieces <- pieces[!vapply(pieces, is.null, logical(1))]
    if (length(pieces) == 0) return(mark_sa_failure(return_list, method))
    mod_out <- dplyr::bind_rows(pieces)
  }

  # --- Water level -------------------------------------------------------------
  # LKE_lvlwtr is not a gridded variable; `.raf_wlev()` compares the modelled
  # surface against obs$level on its own daily grid. `lvl_comp` reshapes that
  # frame to the (Date, depth, var_aeme, obs, model, diff) schema of the
  # gridded comparison so that `return_df` (residual mode) can hand PEST one
  # row per water-level observation, keyed like every other observation but
  # with depth = NA.
  df_lvl <- NULL
  lvl_comp <- NULL
  if (include_wlev && method == "calib") {
    df_lvl <- .raf_wlev(aeme = aeme, nc = nc, model = model, obs = obs,
                        inp = inp)
    if (is.null(df_lvl)) return(return_list)
    if (nrow(df_lvl) > 0) {
      lvl_comp <- data.frame(
        Date = as.Date(df_lvl$Date), depth = NA_real_,
        var_aeme = "LKE_lvlwtr", obs = df_lvl$obs, model = df_lvl$model,
        diff = df_lvl$diff, stringsAsFactors = FALSE
      )
    }
  }

  # --- Score -----------------------------------------------------------------
  if (!is.null(obs$lake) && length(vars_sim) > 0) {

    if (method == "calib") {
      obs_sub <- obs$lake |>
        dplyr::select(Date, depth, var_aeme, value) |>
        dplyr::filter(Date %in% mod_out$Date, var_aeme %in% vars_sim) |>
        dplyr::rename(obs = value)
      if (nrow(obs_sub) < 1) {
        AEME::cli_safe("No observational data present.",
                       FUN = cli::cli_alert_warning)
        # A residual-mode run that also fits water level can still proceed
        # on the water-level rows alone.
        if (return_df && !is.null(lvl_comp)) return(lvl_comp)
        return(return_list)
      }
      comp_df <- obs_sub |>
        dplyr::left_join(mod_out, by = c("Date", "depth", "var_aeme")) |>
        dplyr::mutate(diff = model - obs)
    } else {
      comp_df <- mod_out
    }

    if (nrow(comp_df) == 0) return(mark_sa_failure(return_list, method))
    # Residual mode: append the water-level rows so the forward run sees a
    # value for every observation, gridded and level alike.
    if (return_df) return(dplyr::bind_rows(comp_df, lvl_comp))

    if (method == "calib") {
      for (v in unique(comp_df$var_aeme)) {
        sub <- comp_df[comp_df$var_aeme == v, ]
        return_list[[v]] <- FUN_list[[v]](sub) * weights[[v]]
      }
    } else {
      for (n in unique(comp_df$name)) {
        sub <- comp_df[comp_df$name == n, ]
        return_list[[n]] <- FUN_list[[sa_ctrl$vars_sim[[n]]$var]](sub)
      }
    }

    if (include_wlev && method == "calib") {
      return_list[["LKE_lvlwtr"]] <- FUN_list$LKE_lvlwtr(df_lvl) * wlev_weight
    }
    return(return_list)
  }

  # No lake observations (or no gridded vars): water level only.
  if (include_wlev && method == "calib" && !is.null(df_lvl) &&
      nrow(df_lvl) > 0) {
    if (return_df) return(lvl_comp)
    res1 <- FUN_list$LKE_lvlwtr(df_lvl) * wlev_weight
    return_list[["LKE_lvlwtr"]] <- ifelse(is.nan(res1), na_value, res1)
  }
  return_list
}

#' Warn that a variable's model output could not be read.
#' @noRd
.raf_warn_read <- function(v, reason) {
  AEME::cli_safe(paste0("Error reading model outputs for variable ", v,
                        ": {.emph ", reason %||% "no output", "}. Returning
                        na_value."),
                 FUN = cli::cli_alert_warning)
}

#' Extract one variable's modelled values over its date/depth window as a
#' long dataframe (`Date`, `depth`, `model`, `var_aeme`, `name`), or `NULL`
#' on any failure. Shared by the calibration and sensitivity paths, which
#' differ only in where the window comes from and whether a `name` column is
#' attached.
#' @noRd
.raf_extract_var <- function(v, idx, nc, lake_dir, model, deriv_lookup,
                             conv_lookup, hyps, name = NULL) {

  depths     <- idx[["depths"]]
  date_index <- idx[["date_index"]]
  dates      <- idx[["dates"]]
  if (length(depths) == 0 || length(date_index) == 0) return(NULL)

  is_deriv <- isTRUE(deriv_lookup[[v]])
  extract_var <- if (is_deriv) AEME::get_deriv_inputs(v) else v

  out <- AEME::read_model_outputs(nc = nc, lake_dir = lake_dir, model = model,
                                  vars_sim = extract_var, depths = depths,
                                  date_index = date_index, incl_fluxes = FALSE)
  if (AEME::is_model_error(out)) {
    .raf_warn_read(v, out$reason)
    return(NULL)
  }
  if (is_deriv) {
    out <- AEME::add_deriv_output(out_list = out, hyps = hyps, vars_sim = v)
  }
  out <- out[[v]]

  # A plain vector back is a single-depth (surface) variable; a matrix is
  # depth x time. Either way, trim `dates` if the run stopped short.
  if (is.null(nrow(out))) {
    if (length(out) < length(dates)) {
      AEME::cli_safe(paste0("Fewer timesteps than requested for variable ", v,
                            "; trimming dates."), FUN = cli::cli_alert_warning)
      dates <- dates[seq_along(out)]
    }
    depths <- NA_real_
    each <- 1L
  } else {
    if (ncol(out) != length(dates)) {
      AEME::cli_safe(paste0("Fewer timesteps than requested for variable ", v,
                            "; trimming dates."), FUN = cli::cli_alert_warning)
      dates <- dates[seq_len(ncol(out))]
    }
    each <- length(depths)
  }

  # AED variables are stored in different units in the netCDF; convert to the
  # AEME unit. A missing entry means "no conversion", not NA.
  conv_fact <- 1
  if (identical(model, "glm_aed")) {
    cf <- conv_lookup[[v]]
    if (!is.null(cf) && !is.na(cf)) conv_fact <- cf
  }
  if (!is.matrix(out)) out <- matrix(out, nrow = 1L, ncol = length(out))
  out <- out * conv_fact

  df <- data.frame(Date = rep(dates, each = each),
                   depth = as.vector(depths),
                   model = as.vector(out),
                   var_aeme = v,
                   stringsAsFactors = FALSE)
  if (!is.null(name)) df$name <- name
  df
}

#' Put observed lake level on the same vertical datum as the modelled level.
#'
#' `AEME::read_model_wlev()` reports the modelled surface as height above the
#' lowest point of the hypsograph; `observations(aeme)$level$value` shares the
#' hypsograph's own vertical reference, so subtracting the deepest bed
#' elevation converts an observation into the modelled quantity.
#'
#' Shared by `.raf_wlev()` (the scalar fit path) and
#' \code{\link{pest_obs_table}} (residual mode) so the two cannot drift apart
#' on the datum - a mismatch there would surface as a constant bias in every
#' water-level residual.
#' @noRd
.wlev_obs_to_model_datum <- function(level_value, hypsograph) {
  level_value - min(hypsograph$elev)
}

#' Build the modelled-vs-observed water-level comparison frame, or `NULL`
#' when the run produced no usable water level (the caller then returns its
#' na_value list).
#' @noRd
.raf_wlev <- function(aeme, nc, model, obs, inp) {

  balance <- AEME::read_model_wlev(nc = nc, model = model)
  if (is.null(ncol(balance))) return(NULL)
  if (any(balance[["LKE_lvlwtr"]] <= 0) || anyNA(balance[["LKE_lvlwtr"]])) {
    return(NULL)
  }

  wbal <- AEME::water_balance(aeme)
  tme <- AEME::time(aeme)
  time_check <- !is.null(obs$level) &&
    any(obs$level$Date > tme$start & obs$level$Date < tme$stop)

  if (!is.null(obs$level) && time_check) {
    lvl_adj <- obs$level |>
      dplyr::mutate(value = .wlev_obs_to_model_datum(value, inp$hypsograph))
  } else if (!is.null(wbal$data$wbal)) {
    lvl_adj <- wbal$data$wbal |>
      dplyr::select(Date, value) |>
      dplyr::mutate(value = abs(min(inp$hypsograph$depth)),
                    var_aeme = "LKE_lvlwtr")
  } else {
    lvl_adj <- balance |>
      dplyr::select(Date) |>
      dplyr::mutate(value = inp$init_depth, var_aeme = "LKE_lvlwtr")
  }

  balance |>
    dplyr::left_join(lvl_adj, by = "Date") |>
    dplyr::rename(model = LKE_lvlwtr) |>
    dplyr::mutate(
      model = dplyr::case_when(is.na(model) ~ 0, .default = model),
      LID = NA, var_aeme = "DEPTH", depth = NA,
      diff = model - value) |>
    dplyr::filter(!is.na(diff), Date >= tme$start, Date <= tme$stop) |>
    dplyr::select(LID, Date, value, var_aeme, depth, model, diff) |>
    dplyr::rename(obs = value)
}
