#' Estimate starting values for sediment zone parameters from observations
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Sets the starting `value` of sediment zone parameters in `param` from
#' observed data, so a calibration starts near the observed state rather than
#' at the shipped defaults. Currently supports the GLM sediment temperature
#' parameters `sediment/sed_temp_mean`, `sediment/sed_temp_amplitude` and
#' `sediment/sed_temp_peak_doy`.
#'
#' Each sediment zone is turned into a depth band using the lake depth and
#' GLM's `zone_heights` (the height above the bed of each zone's upper edge,
#' zone 1 being the deepest). Observed `HYD_temp` in a band is used as a proxy
#' for the temperature of the sediment beneath it. When the observations cover
#' enough of the year (`min_months`), an annual harmonic
#' \eqn{T = m + A\cos(2\pi(doy - p)/365.25)} is fitted to the band, giving the
#' annual mean `m`, amplitude `A` and peak day of year `p` without the bias of
#' sampling only some seasons. Otherwise only the plain mean is used and the
#' amplitude and peak day are left alone.
#'
#' How the estimates are applied depends on how the parameter is set up in
#' `param`:
#' * independent per-zone rows (`index` 1..n) take the estimate for their
#'   zone, and a row with no `index` takes the mean over zones;
#' * an anchor + offset set from [zone_offset_param()] takes the zone 1
#'   estimate for the anchor and the difference between neighbouring zones for
#'   the offsets (clipped to the offset bounds);
#' * an anchor + ratio set from [zone_ratio_param()] takes the zone 1
#'   estimate and the ratio between neighbouring zones.
#'
#' Zones with too little data are left at their current value. Estimated values
#' are clipped into the existing `min`/`max` of each row (with a warning),
#' unless `width` is supplied to re-centre the bounds on the estimate.
#'
#' @param aeme An `aeme` object, used for the observations, lake depth and
#' zone heights. Can be `NULL` if `obs`, `max_depth` and `zone_heights` are all
#' supplied.
#' @param param data.frame of parameters, as passed to [calib_aeme()].
#' @param n_zones integer; number of GLM sediment zones. Taken from
#' `zone_heights` when `NULL`.
#' @param zone_heights numeric; GLM `zone_heights`. When `NULL`, read from the
#' built configuration of `aeme` (so `aeme` must have been built).
#' @param max_depth numeric; maximum lake depth in metres. When `NULL`, taken
#' from `AEME::lake(aeme)$depth`.
#' @param obs data.frame of observations with columns `var_aeme`, `depth`,
#' `value` and `Date`. When `NULL`, `AEME::observations(aeme)$lake` is used.
#' @param min_obs integer; minimum observations in a zone's depth band.
#' @param min_months integer; minimum number of distinct months with data
#' before the annual harmonic is fitted.
#' @param width named numeric; optional half-width for re-centring the bounds
#' on the estimate, named by parameter (e.g. `c(sed_temp_mean = 4)`). Applies
#' to the independent and anchor rows only.
#'
#' @return `param` with updated `value` (and `min`/`max` if `width` is given).
#' The per-zone estimates are attached as the `"zone_estimates"` attribute.
#' @seealso [zone_offset_param()], [zone_ratio_param()]
#' @export
#'
#' @examples
#' set.seed(1)
#' doy <- rep(seq(15, 350, by = 30), 2)
#' obs <- data.frame(
#'   var_aeme = "HYD_temp",
#'   depth = rep(c(2, 12), each = length(doy) / 2),
#'   Date = as.Date("2020-01-01") + doy - 1,
#'   value = c(16 + 5 * cos(2 * pi * (doy[1:12] - 40) / 365),
#'             11 + 2 * cos(2 * pi * (doy[1:12] - 90) / 365))
#' )
#' p <- data.frame(model = "glm_aed", file = "glm4.nml",
#'                 name = "sediment/sed_temp_mean", value = 12, min = 5,
#'                 max = 25, group = NA_character_, index = 1L)
#' p <- zone_offset_param(p, n_zones = 2)
#' estimate_zone_start(aeme = NULL, p, obs = obs, zone_heights = c(6, 14),
#'                     max_depth = 13)
estimate_zone_start <- function(aeme, param, n_zones = NULL,
                                zone_heights = NULL, max_depth = NULL,
                                obs = NULL, min_obs = 10, min_months = 4,
                                width = NULL) {

  if (!is.data.frame(param) || !"name" %in% names(param))
    cli::cli_abort("{.arg param} must be a data frame with a {.field name}
                   column.")

  if (!"index" %in% names(param)) param$index <- NA_integer_
  for (col in c("value", "min", "max")) {
    if (!col %in% names(param)) param[[col]] <- NA_real_
  }

  # Zone geometry ----
  if (is.null(zone_heights)) {
    if (is.null(aeme))
      cli::cli_abort("Supply {.arg zone_heights} or a built {.arg aeme}.")
    cfg <- AEME::configuration(aeme = aeme)
    zone_heights <- cfg$glm_aed$hydrodynamic$sediment$zone_heights
    if (is.null(zone_heights))
      cli::cli_abort("No {.field zone_heights} in the configuration of
                     {.arg aeme}. Build it first with {.fn AEME::build_aeme}
                     or supply {.arg zone_heights}.")
  }
  zone_heights <- as.numeric(zone_heights)
  if (is.null(n_zones)) n_zones <- length(zone_heights)
  if (length(zone_heights) < n_zones)
    cli::cli_abort("{.arg zone_heights} has {length(zone_heights)} value{?s}
                   but {.arg n_zones} is {n_zones}.")
  zone_heights <- zone_heights[seq_len(n_zones)]
  if (is.null(max_depth)) {
    if (is.null(aeme))
      cli::cli_abort("Supply {.arg max_depth} or {.arg aeme}.")
    max_depth <- AEME::lake(aeme)$depth
  }

  # Observations ----
  if (is.null(obs)) {
    if (is.null(aeme))
      cli::cli_abort("Supply {.arg obs} or {.arg aeme}.")
    obs <- AEME::observations(aeme)$lake
  }
  if (is.null(obs) || !all(c("var_aeme", "depth", "value") %in% names(obs)))
    cli::cli_abort("{.arg obs} must have columns {.field var_aeme},
                   {.field depth}, {.field value} and {.field Date}.")
  if (!"Date" %in% names(obs)) obs$Date <- as.Date(obs$datetime)
  temp <- obs[obs$var_aeme %in% "HYD_temp" & !is.na(obs$value), , drop = FALSE]

  est <- estimate_zone_temp(temp, zone_heights = zone_heights,
                            max_depth = max_depth, min_obs = min_obs,
                            min_months = min_months)

  # Apply to param ----
  targets <- c(sed_temp_mean = "mean", sed_temp_amplitude = "amplitude",
               sed_temp_peak_doy = "peak_doy")
  base <- zone_base_name(param$name)
  key <- sub("^.*/", "", base)
  clipped <- character()

  for (tn in names(targets)) {
    e <- est[[targets[[tn]]]]
    if (all(is.na(e))) next
    rows <- which(key == tn)
    if (length(rows) == 0) next

    is_off <- grepl("_zoffset$", param$name)
    is_rat <- grepl("_zratio$", param$name)
    is_step <- is_off | is_rat
    anchor <- rows[!is_step[rows] & param$index[rows] %in% 1L]
    steps <- rows[is_step[rows]]

    set_val <- function(i, v) {
      if (is.na(v)) return(invisible())
      lo <- param$min[i]; hi <- param$max[i]
      if (!is.null(width) && !is.na(width[tn]) && !is_step[i]) {
        param$min[i] <<- lo <- v - width[[tn]]
        param$max[i] <<- hi <- v + width[[tn]]
      }
      if (!is.na(lo) && !is.na(hi) && (v < lo || v > hi)) {
        clipped <<- c(clipped, paste0(param$name[i], "[", param$index[i], "]"))
        v <- min(max(v, lo), hi)
      }
      param$value[i] <<- v
    }

    if (length(steps) > 0) {
      # anchor + offset/ratio rows
      if (length(anchor) == 1) set_val(anchor, e[1])
      for (i in steps) {
        z <- param$index[i]
        if (is.na(z) || z < 2 || z > length(e)) next
        if (is.na(e[z]) || is.na(e[z - 1])) next
        set_val(i, if (is_off[i]) e[z] - e[z - 1] else e[z] / e[z - 1])
      }
    }
    for (i in setdiff(rows, c(steps))) {
      if (is.na(param$index[i])) {
        set_val(i, if (tn == "sed_temp_peak_doy") circ_mean_doy(e) else
          mean(e, na.rm = TRUE))
      } else if (param$index[i] >= 1 && param$index[i] <= length(e)) {
        set_val(i, e[param$index[i]])
      }
    }
  }

  if (length(clipped) > 0) {
    cli::cli_warn(c("Estimates outside the parameter bounds were clipped:",
                    stats::setNames(clipped, rep("*", length(clipped)))))
  }
  attr(param, "zone_estimates") <- est
  param
}

#' Estimate sediment temperature parameters per zone from water temperature
#'
#' @param temp data.frame of `HYD_temp` observations (`depth`, `value`,
#' `Date`).
#' @param zone_heights numeric; upper edge of each zone as height above bed.
#' @param max_depth numeric; maximum depth in metres.
#' @return data.frame with one row per zone: depth band, `n_obs`, `n_months`,
#' `method` (`"harmonic"`, `"mean"` or `"none"`), `mean`, `amplitude`,
#' `peak_doy`.
#' @noRd
estimate_zone_temp <- function(temp, zone_heights, max_depth, min_obs = 10,
                               min_months = 4) {
  n <- length(zone_heights)
  lower_h <- c(0, zone_heights[-n])
  # depth band of each zone (depth increases downwards)
  d_top <- pmax(max_depth - zone_heights, 0)
  d_bot <- pmax(max_depth - lower_h, 0)

  out <- data.frame(zone = seq_len(n), depth_top = d_top, depth_bottom = d_bot,
                    n_obs = 0L, n_months = 0L, method = "none",
                    mean = NA_real_, amplitude = NA_real_,
                    peak_doy = NA_real_, stringsAsFactors = FALSE)

  for (i in seq_len(n)) {
    last <- i == 1   # deepest zone also takes anything at/below the bed
    sel <- temp$depth >= d_top[i] &
      (temp$depth < d_bot[i] | (last & temp$depth <= d_bot[i]))
    x <- temp[sel, , drop = FALSE]
    out$n_obs[i] <- nrow(x)
    if (nrow(x) < min_obs) next
    dt <- as.Date(x$Date)
    mon <- length(unique(format(dt, "%m")))
    out$n_months[i] <- mon
    doy <- as.numeric(format(dt, "%j"))
    if (mon >= min_months) {
      w <- 2 * pi / 365.25
      fit <- stats::lm(x$value ~ cos(w * doy) + sin(w * doy))
      b <- stats::coef(fit)
      if (all(is.finite(b))) {
        out$method[i] <- "harmonic"
        out$mean[i] <- unname(b[1])
        out$amplitude[i] <- sqrt(b[2]^2 + b[3]^2)
        out$peak_doy[i] <- (atan2(b[3], b[2]) / w) %% 365.25
        next
      }
    }
    out$method[i] <- "mean"
    out$mean[i] <- mean(x$value)
  }
  out
}

#' Circular mean of days of year
#' @noRd
circ_mean_doy <- function(doy) {
  doy <- doy[!is.na(doy)]
  if (length(doy) == 0) return(NA_real_)
  w <- 2 * pi / 365.25
  (atan2(mean(sin(w * doy)), mean(cos(w * doy))) / w) %% 365.25
}
