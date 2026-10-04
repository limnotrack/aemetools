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
#' @param regions named list; calibration sub-regions, exactly as
#' \code{\link{calib_aeme}} accepts as its named-list `vars_sim` form. Only
#' meaningful when `method = "calib"`. Each named entry's variable is scored
#' separately over its own `month`/`depth_range` window and weighted by
#' `weights[[var]] * region$weight`, instead of being pooled with the rest
#' of that variable's observations.
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
#' This is the calibration objective itself - `calib_aeme()` (via
#' `eval_param_chunk()`) sums this function's per-`vars_sim` components into
#' the single `fit` value it records - so it is **not** expected to
#' numerically match `AEME::assess_aeme()` / `AEME::assess_model()` on the
#' same parameter set, even for the exact best-fit run. The two answer
#' different questions: `assess_aeme()` reports a fixed set of per-variable,
#' unweighted, conventionally-oriented statistics (`nse`, `kge`, `mae`, ...)
#' read via `AEME::get_var()`, for general model diagnostics; `run_and_fit()`
#' reports whatever `FUN_list` returns, weighted and (for `method = "calib"`)
#' later summed across variables, read via `AEME::read_model_outputs()`, as
#' the quantity calibration actually minimises.
#'
#' To reproduce a recorded `fit_value` for verification, call this function
#' again with the same `param`, `FUN_list`, `vars_sim` and `weights` used for
#' the original `calib_aeme()` call, then reduce it the same way
#' `eval_param_chunk()` does - summed, with `na_value` substituted whenever
#' any component is `NA` (a component can come back as the `na_value`
#' sentinel rather than `NA` on a failed run, so guard on that rather than
#' summing directly):
#' \preformatted{
#' res <- run_and_fit(aeme, param, model, vars_sim, path, FUN_list, weights,
#'                    include_wlev = TRUE, method = "calib", fit = TRUE)
#' fit <- if (any(is.na(unlist(res)))) na_value else sum(unlist(res))
#' }
#' Use `assess_aeme()`/`assess_model()` separately for interpretable,
#' per-variable fit statistics - it is not a substitute for the above.
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
                        method = "calib", sa_ctrl = NULL, regions = NULL,
                        fit = TRUE, timeout = Inf) {

  # The variables covered by a calibration sub-region are scored separately
  # (their own depth/month window, their own weight) rather than pooled with
  # the rest of that variable's observations - see the "Score" block below.
  region_vars <- if (method == "calib" && !is.null(regions)) {
    unique(vapply(regions, function(v) v$var, character(1)))
  } else {
    character(0)
  }

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
  score_names <- if (method == "sa") {
    names(sa_ctrl$vars_sim)
  } else if (length(region_vars) > 0) {
    c(names(regions), setdiff(vars_sim, region_vars))
  } else {
    vars_sim
  }
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

  # --- Early dry-lake guard -------------------------------------------------
  #
  # Checked unconditionally, before ANY per-variable extraction - regardless
  # of which water-balance variable (if any) this calibration actually
  # targets. Previously this check only existed inside .raf_wlev(), gated on
  # include_wlev/LKE_lvlwtr specifically (see that function, below): a
  # calibration scoring water balance via LKE_vol instead (this project's
  # current stage 2, switched from LKE_lvlwtr to avoid elevation-datum
  # issues) never triggered it at all. A lake that has gone dry, or produced
  # a non-finite water level at some point in the run, is a structurally
  # failed run - left uncaught here, every other depth-resolved variable's
  # extraction (interp_static_grid(), in AEME::read_model_outputs()) then
  # fails silently and separately on each affected day (too few valid model
  # layers to interpolate from), surfacing as scattered NA in the scored
  # output instead of one clean whole-run failure. Checking the model's own
  # water level FIRST, independent of vars_sim/regions, catches this in one
  # place and fails the whole run the same way a crashed model run does.
  wlev_check <- tryCatch(AEME::read_model_wlev(nc = nc, model = model),
                        error = function(e) NULL)
  if (!is.null(wlev_check) && !is.null(ncol(wlev_check)) &&
      "LKE_lvlwtr" %in% names(wlev_check) &&
      (any(wlev_check[["LKE_lvlwtr"]] <= 0) || anyNA(wlev_check[["LKE_lvlwtr"]]))) {
    AEME::cli_safe(
      paste0("Water level reached zero or was non-finite at some point in ",
             "the run. Returning {.val ", na_value, "} for every scored ",
             "variable rather than letting depth-resolved extraction fail ",
             "silently, day by day."),
      FUN = cli::cli_alert_warning)
    return(mark_sa_failure(return_list, method))
  }

  # --- Load the pieces needed to score -------------------------------------
  lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)
  inp <- AEME::input(aeme)
  hyps <- inp$hypsograph
  obs <- AEME::observations(aeme)
  if (!is.null(obs$lake)) {
    obs$lake <- normalise_lake_obs(obs$lake)
  }
  # AEME observations may now carry a POSIXct timestamp while daily model
  # output reads back as `Date`. Built-in calibration keys the model
  # comparison on the calendar day, so collapse obs$lake and obs$level to a
  # UTC `Date` here, once - so they agree with the (also day-collapsed) model
  # output both in the scoring block below and inside `.raf_wlev()`.
  obs$lake  <- obs_calendar_date(obs$lake)
  obs$level <- obs_calendar_date(obs$level)

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
    # Region-covered variables are indexed/extracted/scored via `regions`
    # below instead of being pooled into the flat vars_sim path.
    vars_sim <- setdiff(vars_sim, region_vars)
  }

  # --- Date & depth indices ------------------------------------------------
  if (return_indices) var_indices <- NULL
  if (is.null(var_indices)) {
    var_indices <- if (method == "sa") {
      sa_regions <- names(sa_ctrl$vars_sim)
      stats::setNames(lapply(sa_regions, function(n) {
        .region_var_indices(nc = nc, model = model, aeme = aeme, path = path,
                            region = sa_ctrl$vars_sim[[n]])
      }), sa_regions)
    } else if (length(region_vars) > 0) {
      # Calibration with sub-regions: index each region over its own
      # depth/month window, and any remaining flat vars_sim (e.g.
      # LKE_lvlwtr is handled separately, but another gridded variable not
      # split into regions) as before.
      region_idx <- stats::setNames(lapply(names(regions), function(n) {
        .region_var_indices(nc = nc, model = model, aeme = aeme, path = path,
                            region = regions[[n]])
      }), names(regions))
      flat_idx <- if (length(vars_sim) == 0) {
        list()
      } else {
        AEME::get_var_indices(nc = nc, model = model, aeme = aeme, path = path,
                              vars_sim = AEME::get_vars_sim(vars_sim = vars_sim))
      }
      c(region_idx, flat_idx)
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
  if (length(vars_sim) > 0 || length(region_vars) > 0) {

    flat_pieces_fun <- function() {
      vs <- stats::setNames(vars_sim, vars_sim)
      lapply(vs, function(v) {
        key <- if (isTRUE(deriv_lookup[[v]])) AEME::get_deriv_inputs(v)[1] else v
        .raf_extract_var(v = v, idx = var_indices[[key]], nc = nc,
                         lake_dir = lake_dir, model = model,
                         deriv_lookup = deriv_lookup, conv_lookup = conv_lookup,
                         hyps = hyps)
      })
    }

    pieces <- if (method == "sa") {
      sa_regions <- names(sa_ctrl$vars_sim)
      lapply(stats::setNames(sa_regions, sa_regions), function(n) {
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
    } else if (length(region_vars) > 0) {
      region_pieces <- lapply(stats::setNames(names(regions), names(regions)),
                              function(n) {
        v <- regions[[n]]$var
        .raf_extract_var(v = v, idx = var_indices[[n]], nc = nc,
                         lake_dir = lake_dir, model = model,
                         deriv_lookup = deriv_lookup, conv_lookup = conv_lookup,
                         hyps = hyps, name = n)
      })
      c(region_pieces, if (length(vars_sim) > 0) flat_pieces_fun() else list())
    } else {
      flat_pieces_fun()
    }

    # A NULL piece is a variable whose output could not be extracted; drop
    # it and score the rest (it keeps its na_value slot in `return_list`,
    # which is what the previous code did too, via a malformed bind). If
    # nothing extracted, the whole run failed.
    pieces <- pieces[!vapply(pieces, is.null, logical(1))]
    if (length(pieces) == 0) return(mark_sa_failure(return_list, method))
    mod_out <- dplyr::bind_rows(pieces)
    # Model output reads back as `Date` (daily run) or UTC POSIXct (sub-daily).
    # The calibration obs were collapsed to a UTC calendar `Date` above, so
    # collapse the model side the same way and the scoring join keys cleanly.
    # No-op for a daily run (Date -> Date).
    if (method == "calib" && !is.null(mod_out)) {
      mod_out$Date <- as.Date(mod_out$Date, tz = "UTC")
    }
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
  all_grid_vars <- c(vars_sim, region_vars)
  if (!is.null(obs$lake) && length(all_grid_vars) > 0) {

    if (method == "calib") {
      obs_sub <- obs$lake |>
        dplyr::select(Date, depth, var_aeme, value) |>
        dplyr::filter(Date %in% mod_out$Date, var_aeme %in% all_grid_vars) |>
        dplyr::rename(obs = value)
      if (nrow(obs_sub) < 1) {
        # Distinguish "this lake has no observations for these variables" from
        # "it has observations, but none share a timestamp with a model output
        # step". The second is what a daily-vs-sub-daily cadence mismatch (or
        # observations that carry a time-of-day the output grid never lands on)
        # looks like, and it needs a different fix - aligning the observations
        # to the output grid - so name it rather than let it read as an empty
        # lake.
        n_var_obs <- sum(obs$lake$var_aeme %in% all_grid_vars, na.rm = TRUE)
        if (n_var_obs > 0) {
          AEME::cli_safe(
            paste0("None of the {.val ", n_var_obs, "} observation",
                   if (n_var_obs == 1) "" else "s", " for {.val {all_grid_vars}} ",
                   "share a timestamp with a model output step - check the ",
                   "observation cadence against the model output timestep."),
            FUN = cli::cli_alert_warning)
          attr(return_list, "diag") <- "obs_unaligned"
        } else {
          AEME::cli_safe("No observational data present.",
                         FUN = cli::cli_alert_warning)
          attr(return_list, "diag") <- "no_obs"
        }
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
      # `name` is only populated (by mod_out, via the left_join above) for
      # rows extracted through a sub-region; every other row scores through
      # the flat, pooled-by-variable path exactly as before. A region-covered
      # variable's observation that falls outside every region's window has
      # no matching mod_out row (left_join leaves it with model = NA, name =
      # NA) - it belongs to no target at all, so it must not fall back into
      # the flat path, which would score it against no simulated value.
      is_region_row <- if ("name" %in% names(comp_df)) {
        !is.na(comp_df$name)
      } else {
        rep(FALSE, nrow(comp_df))
      }
      flat_row <- !is_region_row & comp_df$var_aeme %in% vars_sim
      for (v in unique(comp_df$var_aeme[flat_row])) {
        sub <- comp_df[flat_row & comp_df$var_aeme == v, ]
        return_list[[v]] <- FUN_list[[v]](sub) * weights[[v]]
      }
      for (n in unique(comp_df$name[is_region_row])) {
        sub <- comp_df[is_region_row & comp_df$name == n, ]
        r <- regions[[n]]
        return_list[[n]] <- FUN_list[[r$var]](sub) * weights[[r$var]] *
          (r$weight %||% 1)
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

#' Build one calibration/SA sub-region's model date/depth index.
#'
#' Always routes through `AEME::get_var_indices()`'s month/depth_range
#' branch - substituting a placeholder for whichever of `region$month` /
#' `region$depth_range` is `NULL` - rather than ever taking its
#' observation-driven default (`month` and `depth_range` both absent).
#' That default has two problems that make it unusable for a region:
#'
#' * it derives dates/depths from `observations(aeme)$lake` rows for
#'   *this exact variable* - but a region is just as often built around a
#'   variable that has no observations of its own at all: a Morris
#'   screening target like `HYD_schstb` (Schmidt stability, a value derived
#'   from the full `HYD_temp` profile, not something ever logged as an
#'   observation) or a whole-lake diagnostic like `LKE_vol`/`LKE_nrgtot`.
#'   With zero matching rows it returns a zero-length index, and for a
#'   *derived* variable that reaches `AEME::add_deriv_output()` with a
#'   zero-column input matrix, which crashes several frames down in
#'   `rLakeAnalyzer`'s `safe_apply()` (`seq_len(NULL)`/`seq_len(integer(0))`:
#'   "argument must be coercible to non-negative integer") rather than
#'   failing cleanly.
#' * even when the variable *is* observed, gating the index on its
#'   observation dates silently changes what a `method = "sa"` region
#'   aggregates over - Morris screening has never joined to observations
#'   (see the "Score" section of `run_and_fit()`), so every existing region
#'   convention (always supplying both `month` and `depth_range`) already
#'   aggregates over the *whole* simulated window, observed or not.
#'
#' The month/depth_range branch instead derives dates from the model's own
#' time axis regardless of observations - correct for `LKE_lvlwtr` (which
#' has no rows in `observations(aeme)$lake` at all, only in `$level`) and
#' every other variable alike - and:
#'
#' * `depth_range = NULL` (the only sensible spec for a variable with no
#'   depth axis) is substituted with a degenerate placeholder so the
#'   branch's unconditional `seq(min(depth_range), max(depth_range), by =
#'   0.5)` does not error on non-finite bounds. The placeholder depths are
#'   harmless regardless of value: `.raf_extract_var()` overwrites them
#'   with `NA` once it sees the model output come back as a plain vector
#'   rather than a depth x time matrix.
#' * `month = NULL` with a depth_range given is substituted with every
#'   month, because the branch's own date filter (`dates %in% month`) is
#'   silently always `FALSE` against a `NULL` month - "no month filter",
#'   not "match nothing".
#' @noRd
.region_var_indices <- function(nc, model, aeme, path, region) {
  # A `derived` variable (AEME::key_naming$derived) is computed from another
  # variable's full depth profile (AEME::get_deriv_inputs()) - e.g.
  # HYD_schstb and LKE_nrgtot both derive from HYD_temp's whole water
  # column - so `depth_range = NULL` is wrong for it even though its own
  # output is a single value per timestep. Left unchecked, requesting a
  # degenerate one-depth placeholder starves the derived calculation of a
  # profile and it fails several frames down inside rLakeAnalyzer
  # (`seq_len(NULL)`/`seq_len(integer(0))`) with no clue what caused it.
  # `depth_range = NULL` is only for a variable with no depth axis at all,
  # e.g. LKE_vol or LKE_lvlwtr.
  kn <- AEME::key_naming
  is_deriv <- isTRUE(kn$derived[match(region$var, kn$var_aeme)])
  if (is.null(region$depth_range) && is_deriv) {
    cli::cli_abort(c(
      "{.arg depth_range} is {.val NULL} for {.val {region$var}}, a derived
       variable computed from {.val {AEME::get_deriv_inputs(region$var)}}'s
       full depth profile - not a true whole-lake scalar.",
      "i" = "Supply a {.field depth_range} spanning the water column (e.g.
             {.code c(0, z)}), as for any other depth-resolved variable.
             {.val NULL} is only for a variable with no depth axis at all,
             e.g. {.val LKE_vol} or {.val LKE_lvlwtr}."
    ))
  }
  AEME::get_var_indices(nc = nc, model = model, aeme = aeme, path = path,
                        vars_sim = region$var,
                        month = region$month %||% 1:12,
                        depth_range = region$depth_range %||% c(0, 0))[[1]]
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

  # A plain vector back can mean two different things, and they must not be
  # scored the same way:
  #   1. A genuinely depth-less (whole-lake scalar) variable, e.g. LKE_vol/
  #      LKE_lvlwtr - it has no depth axis in the netCDF at all, so `depth
  #      = NA` is correct.
  #   2. A genuinely depth-resolved variable that was simply asked for at
  #      ONE depth - e.g. get_var_indices() narrows a region's requested
  #      depths to real observation depths within depth_range (see that
  #      function's "Prefer real observation depths..." comment), and when
  #      every one of a variable's own observations sits at the same depth
  #      (confirmed: PHY_cyano - every Rototoa observation is recorded at
  #      depth 0), that narrowing legitimately produces a single, KNOWN
  #      depth, not a placeholder. R's default array-dim-dropping then
  #      collapses the depth x time matrix to a plain vector exactly as it
  #      would for case 1, even though the variable IS depth-resolved
  #      (confirmed directly against the raw netCDF: PHY_cyano is written
  #      [lon, lat, z=300, time], identical in shape to PHY_tchla, which
  #      already extracts correctly because its own observations are not
  #      all at one depth). Discarding depths to NA here for case 2 broke
  #      run_and_fit()'s "calib" mode: it joins model output onto
  #      observations on an EXACT (Date, depth, var_aeme) key, and NA can
  #      never match a real observation depth like 0 - every row silently
  #      failed to join, "calib" mode reported the variable as never
  #      produced at all, and "sa" mode (which never joins to observations)
  #      never surfaced the problem. Distinguish the two cases from the
  #      netCDF variable's own dimensions, the one source of truth that
  #      does not depend on how many depths happened to be requested.
  # Either way, trim `dates` if the run stopped short.
  if (is.null(nrow(out))) {
    if (length(out) < length(dates)) {
      AEME::cli_safe(paste0("Fewer timesteps than requested for variable ", v,
                            "; trimming dates."), FUN = cli::cli_alert_warning)
      dates <- dates[seq_along(out)]
    }
    has_depth_axis <- !is.null(nc$var[[extract_var]]) &&
      "z" %in% vapply(nc$var[[extract_var]]$dim, `[[`, character(1), "name")
    single_known_depth <- length(depths) == 1 && is.finite(depths)
    depths <- if (has_depth_axis && single_known_depth) depths else NA_real_
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
  # AEME (>= 0.4.0) time bounds are UTC POSIXct and a sub-daily run's Date
  # column is POSIXct; obs$level and the water-balance frame are a daily Date
  # axis. Work on the UTC calendar date so the join and window filter below
  # stay Date-to-Date.
  balance$Date <- as.Date(balance$Date, tz = "UTC")
  if (any(balance[["LKE_lvlwtr"]] <= 0) || anyNA(balance[["LKE_lvlwtr"]])) {
    return(NULL)
  }

  wbal <- AEME::water_balance(aeme)
  tme <- AEME::time(aeme)
  time_check <- !is.null(obs$level) &&
    any(obs$level$Date > as.Date(tme$start, tz = "UTC") &
          obs$level$Date < as.Date(tme$stop, tz = "UTC"))

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

  # `lvl_adj` is drawn from obs$level, the water balance, or `balance` itself;
  # keep its key a UTC `Date` so the join to `balance` (also a UTC `Date`)
  # cannot silently miss on a class mismatch.
  lvl_adj$Date <- as.Date(lvl_adj$Date, tz = "UTC")

  balance |>
    dplyr::left_join(lvl_adj, by = "Date") |>
    dplyr::rename(model = LKE_lvlwtr) |>
    dplyr::mutate(
      model = dplyr::case_when(is.na(model) ~ 0, .default = model),
      LID = NA, var_aeme = "DEPTH", depth = NA,
      diff = model - value) |>
    dplyr::filter(!is.na(diff), Date >= as.Date(tme$start, tz = "UTC"),
                  Date <= as.Date(tme$stop, tz = "UTC")) |>
    dplyr::select(LID, Date, value, var_aeme, depth, model, diff) |>
    dplyr::rename(obs = value)
}
