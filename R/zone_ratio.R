# Suffixes identifying zone-step parameters: a ratio (value = deeper * ratio)
# or an offset (value = deeper + offset) relative to the next deeper zone.
zone_ratio_suffix <- "_zratio"
zone_offset_suffix <- "_zoffset"
zone_suffix_regex <- "_z(ratio|offset)$"

#' Flag zone-ratio and zone-offset rows in a parameter dataframe
#' @param param data.frame with a `name` column.
#' @return logical vector.
#' @noRd
is_zone_ratio <- function(param) {
  grepl(zone_suffix_regex, as.character(param$name))
}

#' Name of the anchor parameter a zone-step row belongs to
#' @param x character vector of parameter names.
#' @noRd
zone_base_name <- function(x) {
  sub(zone_suffix_regex, "", x, ignore.case = TRUE)
}

#' Parameterise zoned parameters as an anchor value plus ratios
#'
#' Sediment zone parameters (e.g. `aed_sed_const2d/fsed_oxy`,
#' `sediment/sed_temp_mean`) are normally calibrated as independent values per
#' zone, which allows combinations that break the expected change with depth
#' (e.g. zone 1 oxygen flux greater than zone 2). `zone_ratio_param()`
#' replaces them with the value for zone 1 (the deepest zone), which is
#' calibrated as usual, and one ratio per shallower zone. Each zone is a
#' fraction of the next deeper zone, so `zone2 = ratio2 * zone1` and
#' `zone3 = ratio3 * zone2`, which guarantees that a shallower zone never
#' exceeds a deeper one (in magnitude) when the ratios are within 0 and 1.
#'
#' The ratio rows are ordinary calibration parameters (named
#' `<name>_zratio`) that can be linked to variables in `param_var_matrix` like
#' any other. They are converted back to real per-zone values by
#' [expand_zone_ratios()] immediately before the model files are written, so no
#' change to the model configuration is needed.
#'
#' For quantities that do not scale sensibly as a ratio, such as sediment
#' temperature, use [zone_offset_param()].
#'
#' @param param data.frame of parameters for a single zoned parameter. Either
#' one row (the zone 1 value) or one row per zone. The row with `index == 1`
#' (or the first row if `index` is missing) is the anchor.
#' @param n_zones integer; number of zones in the model for this parameter.
#' @param lower,upper numeric; bounds for the ratios.
#' @param start numeric; starting value for the ratios when `param` has a
#' single row. If `param` has one row per zone, starting ratios are derived
#' from the zone values and clipped to the bounds.
#'
#' @return data.frame with the anchor row (`index = 1`) followed by
#' `n_zones - 1` ratio rows.
#' @seealso [zone_offset_param()], [expand_zone_ratios()]
#' @examples
#' p <- data.frame(model = "glm_aed", file = "aed.nml",
#'                 name = "aed_sed_const2d/fsed_oxy", value = -30, min = -60,
#'                 max = -5, group = NA_character_, index = 1)
#' zone_ratio_param(p, n_zones = 3)
#' @export
zone_ratio_param <- function(param, n_zones, lower = 0.1, upper = 1,
                             start = 0.5) {
  if (!is.numeric(lower) || !is.numeric(upper) || lower <= 0 || upper > 1 ||
      lower >= upper)
    cli::cli_abort("Ratio bounds must satisfy 0 < {.arg lower} < {.arg upper}
                   <= 1.")
  zone_step_param(param, n_zones, lower, upper, start,
                  suffix = zone_ratio_suffix, type = "ratio")
}

#' Parameterise zoned parameters as an anchor value plus offsets
#'
#' The additive counterpart of [zone_ratio_param()], for quantities such as
#' sediment temperature where a ratio is not meaningful (a ratio of degrees
#' Celsius is arbitrary, and shallower zones are normally *warmer* than deeper
#' ones). Zone 1 (the deepest) is calibrated directly and each shallower zone is
#' the next deeper zone plus an offset: `zone2 = zone1 + offset2`,
#' `zone3 = zone2 + offset3`. With `lower >= 0` the values can only increase
#' towards the surface; use a negative `lower` (or `upper <= 0`) to allow or
#' force the opposite.
#'
#' Offset rows are named `<name>_zoffset` and are converted back to per-zone
#' values by [expand_zone_ratios()].
#'
#' @inheritParams zone_ratio_param
#' @param lower,upper numeric; bounds for the offsets, in the units of the
#' parameter.
#' @param start numeric; starting value for the offsets when `param` has a
#' single row. If `param` has one row per zone, starting offsets are derived
#' from the zone values and clipped to the bounds.
#'
#' @return data.frame with the anchor row (`index = 1`) followed by
#' `n_zones - 1` offset rows.
#' @seealso [zone_ratio_param()], [expand_zone_ratios()]
#' @examples
#' p <- data.frame(model = "glm_aed", file = "glm4.nml",
#'                 name = "sediment/sed_temp_mean", value = 12, min = 5,
#'                 max = 25, group = NA_character_, index = 1)
#' zone_offset_param(p, n_zones = 3, lower = 0, upper = 5, start = 1)
#' @export
zone_offset_param <- function(param, n_zones, lower = 0, upper = 5,
                              start = 1) {
  if (!is.numeric(lower) || !is.numeric(upper) || lower >= upper)
    cli::cli_abort("Offset bounds must satisfy {.arg lower} < {.arg upper}.")
  zone_step_param(param, n_zones, lower, upper, start,
                  suffix = zone_offset_suffix, type = "offset")
}

#' Shared builder for zone-ratio and zone-offset parameters
#' @noRd
zone_step_param <- function(param, n_zones, lower, upper, start, suffix, type) {
  if (!is.data.frame(param) || nrow(param) < 1)
    cli::cli_abort("{.arg param} must be a data.frame with at least one row.")
  if (!all(c("name", "value") %in% names(param)))
    cli::cli_abort("{.arg param} must have columns {.field name} and
                   {.field value}.")
  if (length(unique(param$name)) != 1)
    cli::cli_abort("{.arg param} must contain a single parameter name.")
  if (!is.numeric(n_zones) || length(n_zones) != 1 || n_zones < 2)
    cli::cli_abort("{.arg n_zones} must be a single number >= 2.")

  if (!"index" %in% names(param)) param$index <- seq_len(nrow(param))
  param <- param[order(param$index), , drop = FALSE]
  anchor <- param[1, , drop = FALSE]
  anchor$index <- 1L

  # Starting steps
  r <- rep(start, n_zones - 1)
  if (nrow(param) > 1) {
    vals <- param$value
    if (length(vals) < n_zones)
      vals <- c(vals, rep(NA_real_, n_zones - length(vals)))
    vals <- vals[seq_len(n_zones)]
    r_i <- if (type == "ratio") vals[-1] / vals[-n_zones] else
      vals[-1] - vals[-n_zones]
    r[is.finite(r_i)] <- r_i[is.finite(r_i)]
  }
  r <- pmin(pmax(r, lower), upper)

  step <- anchor[rep(1, n_zones - 1), , drop = FALSE]
  step$name <- paste0(anchor$name, suffix)
  step$index <- seq(2L, n_zones)
  step$value <- r
  if ("min" %in% names(step)) step$min <- lower
  if ("max" %in% names(step)) step$max <- upper
  if ("log" %in% names(step)) step$log <- FALSE
  rownames(step) <- NULL
  rownames(anchor) <- NULL

  rbind(anchor, step)
}

#' Convert zone-ratio and zone-offset parameters to per-zone values
#'
#' Expands the anchor + ratio parameterisation created by [zone_ratio_param()]
#' (or the anchor + offset one from [zone_offset_param()]) into ordinary
#' per-zone parameter rows that can be written to the model configuration with
#' `AEME::input_model_parameters()`. The ratio or offset rows are removed and
#' replaced with one row per shallower zone, with
#' `value[i] = value[i - 1] * ratio[i]` or
#' `value[i] = value[i - 1] + offset[i]`.
#'
#' A `param` without any zone-ratio or zone-offset rows is returned unchanged.
#'
#' @param param data.frame of parameters, as used by [run_aeme_param()].
#'
#' @return data.frame of parameters with the same columns as `param` and no
#' zone-ratio or zone-offset rows.
#' @examples
#' p <- data.frame(model = "glm_aed", file = "aed.nml",
#'                 name = "aed_sed_const2d/fsed_oxy", value = -30, min = -60,
#'                 max = -5, group = NA_character_, index = 1)
#' p <- zone_ratio_param(p, n_zones = 3)
#' expand_zone_ratios(p)
#' @export
expand_zone_ratios <- function(param) {
  if (!is.data.frame(param) || !"name" %in% names(param)) return(param)
  is_ratio <- is_zone_ratio(param)
  if (!any(is_ratio)) return(param)

  if (!"group" %in% names(param)) param$group <- NA_character_
  if (!"model" %in% names(param)) param$model <- NA_character_
  if (!"file" %in% names(param)) param$file <- NA_character_

  ratios <- param[is_ratio, , drop = FALSE]
  base <- param[!is_ratio, , drop = FALSE]
  ratios$base_name <- zone_base_name(ratios$name)
  ratios$type <- ifelse(grepl("_zoffset$", ratios$name), "offset", "ratio")

  key <- function(model, file, name, group) {
    paste(model, file, name, group, sep = "|")
  }
  r_key <- key(ratios$model, ratios$file, ratios$base_name, ratios$group)
  b_key <- key(base$model, base$file, base$name, base$group)

  derived <- list()
  drop_base <- rep(FALSE, nrow(base))
  for (k in unique(r_key)) {
    rr <- ratios[r_key == k, , drop = FALSE]
    rr <- rr[order(rr$index), , drop = FALSE]
    if (length(unique(rr$type)) != 1) {
      cli::cli_abort("{.val {rr$base_name[1]}} mixes zone ratios and zone
                     offsets; use one or the other.")
    }
    anchor <- base[b_key == k & base$index %in% 1L, , drop = FALSE]
    if (nrow(anchor) != 1) {
      cli::cli_abort("Zone-ratio parameter {.val {rr$name[1]}} needs exactly
                     one anchor row with {.code index = 1} for
                     {.val {rr$base_name[1]}}.")
    }
    if (!identical(as.integer(rr$index), seq(2L, length.out = nrow(rr)))) {
      cli::cli_abort("Zone-ratio indices for {.val {rr$base_name[1]}} must be
                     consecutive starting at 2.")
    }
    # Ratios replace any independent value given for the same zones
    drop_base <- drop_base | (b_key == k & base$index %in% rr$index)

    vals <- if (rr$type[1] == "ratio") {
      anchor$value * cumprod(rr$value)
    } else {
      anchor$value + cumsum(rr$value)
    }
    d <- anchor[rep(1, nrow(rr)), , drop = FALSE]
    d$index <- rr$index
    d$value <- vals
    derived[[k]] <- d
  }

  out <- rbind(base[!drop_base, , drop = FALSE], do.call(rbind, derived))
  out <- out[order(match(b_key_order(out), unique(b_key_order(out))),
                   out$index), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Grouping key keeping a parameter's zones adjacent
#' @noRd
b_key_order <- function(df) {
  paste(df$model, df$file, df$name, df$group, sep = "|")
}
