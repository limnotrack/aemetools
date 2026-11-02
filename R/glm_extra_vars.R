#' Is a variable one of GLM-AED's extra (sediment / diagnostic) outputs?
#'
#' TRUE for the AEME names in `AEME::glm_extra_vars` (e.g. `"SEDL_Fsed_oxy"`,
#' `"SED_Fsed_oxy"`), including the lake-wide `_lake` summary of a sediment-zone
#' variable (e.g. `"SED_Fsed_oxy_lake"`). These variables have no
#' `AEME::key_naming` entry, so they are read with `extra_vars` rather than
#' as ordinary `vars_sim`.
#'
#' @param v character; AEME variable name(s).
#' @return logical vector, the length of `v`.
#' @noRd
is_glm_extra_var <- function(v) {
  if (!length(v)) return(logical(0))
  reg <- .glm_extra_registry()
  if (is.null(reg)) return(rep(FALSE, length(v)))
  base <- sub("_lake$", "", v)
  base %in% reg$var_aeme & !v %in% AEME::key_naming$var_aeme
}

#' The `glm_extra_vars` registry, or `NULL` if AEME predates it
#' @noRd
.glm_extra_registry <- function() {
  tryCatch(AEME::glm_extra_vars, error = function(e) NULL)
}

#' Describe one extra variable: what dimensions it has and what a region
#' must therefore specify.
#'
#' @param v character; AEME name, optionally with a `_lake` suffix.
#' @return list with `var`, `lake` (is a lake-wide summary), `needs_zone`,
#'   `needs_layer` and `is_series` (is a time series at all, so can be a
#'   sensitivity response).
#' @noRd
glm_extra_var_info <- function(v) {
  reg <- .glm_extra_registry()
  lake <- grepl("_lake$", v)
  base <- sub("_lake$", "", v)
  dims <- strsplit(reg$dims[match(base, reg$var_aeme)], ",")[[1]]
  list(var = v, lake = lake, dims = dims,
       is_series = "time" %in% dims,
       needs_zone = !lake && "nzones" %in% dims,
       needs_layer = !lake && "sed_layers" %in% dims,
       lake_ok = identical(dims, c("nzones", "time")))
}

#' Validate the GLM-AED extra variables in a sensitivity analysis' regions
#'
#' Checked up front by [sa_aeme()] so a mis-specified region fails with a
#' message rather than as a whole analysis of `na_value` responses.
#'
#' * extra variables are GLM-AED only;
#' * a per-zone variable (`SED_*`) needs `zone` (a single integer), or use its
#'   lake-wide `_lake` series;
#' * a variable with a `sed_layers` dimension (`SED_temp`) takes an optional
#'   `sed_layer` (a single integer, default 1);
#' * `zone` / `sed_layer` are meaningless for other variables;
#' * they have no depth axis, so `depth_range` is ignored.
#'
#' @param vars_sim named list of regions, as in [create_sa_control()].
#' @param model character; model(s) being analysed.
#' @return `NULL` invisibly; aborts on a problem.
#' @noRd
check_sa_extra_vars <- function(vars_sim, model = NULL) {
  vars <- vapply(vars_sim, function(r) r$var %||% NA_character_, character(1))
  extra <- is_glm_extra_var(vars)

  # `zone` / `sed_layer` on an ordinary variable is always a mistake
  for (n in names(vars_sim)[!extra]) {
    r <- vars_sim[[n]]
    if (!is.null(r$zone) || !is.null(r$sed_layer)) {
      cli::cli_abort(c(
        "Region {.val {n}}: {.field zone}/{.field sed_layer} only apply to
         GLM-AED sediment variables, not {.val {r$var}}.",
        "i" = "See {.code AEME::glm_extra_vars} for the sediment variables."))
    }
  }
  if (!any(extra)) return(invisible(NULL))

  if (is.null(.glm_extra_registry())) {
    cli::cli_abort(c(
      "GLM-AED sediment variables need a newer AEME.",
      "i" = "Install AEME (>= 0.4.1) for {.code AEME::glm_extra_vars}."))
  }
  if (!is.null(model) && !all(model == "glm_aed")) {
    extra_vars <- unique(vars[extra])
    other_models <- setdiff(model, "glm_aed")
    cli::cli_abort(c(
      "GLM-AED sediment variables ({.val {extra_vars}}) cannot be analysed
       with {.val {other_models}}.",
      "i" = "Run these responses with {.code model = \"glm_aed\"} only."))
  }

  for (n in names(vars_sim)[extra]) {
    r <- vars_sim[[n]]
    info <- glm_extra_var_info(r$var)
    if (!info$is_series) {
      cli::cli_abort("Region {.val {n}}: {.val {r$var}} has no time axis and
                     cannot be a sensitivity response.")
    }
    if (info$lake && !info$lake_ok) {
      cli::cli_abort("Region {.val {n}}: {.val {r$var}} has no lake-wide
                     summary; only per-zone variables do.")
    }
    for (f in c("zone", "sed_layer")) {
      val <- r[[f]]
      if (!is.null(val) && (length(val) != 1 || !is.numeric(val) || is.na(val) ||
                            val < 1 || val != round(val))) {
        cli::cli_abort("Region {.val {n}}: {.field {f}} must be a single
                       positive integer.")
      }
    }
    if (info$needs_zone && is.null(r$zone)) {
      cli::cli_abort(c(
        "Region {.val {n}}: {.val {r$var}} is per sediment zone.",
        "i" = "Set {.code zone = <integer>} or use the lake-wide
               {.val {paste0(r$var, '_lake')}} series."))
    }
    if (!info$needs_zone && !is.null(r$zone)) {
      cli::cli_abort("Region {.val {n}}: {.val {r$var}} is not per zone, so
                     {.field zone} does not apply.")
    }
    if (!info$needs_layer && !is.null(r$sed_layer)) {
      cli::cli_abort("Region {.val {n}}: {.val {r$var}} has no sediment layer
                     dimension, so {.field sed_layer} does not apply.")
    }
    if (!is.null(r$depth_range)) {
      cli::cli_warn("Region {.val {n}}: {.field depth_range} is ignored for
                    {.val {r$var}} (no depth axis).")
    }
  }
  invisible(NULL)
}

#' Read one GLM-AED sediment / diagnostic variable over a region's dates
#'
#' Counterpart of `.raf_extract_var()` for the variables in
#' `AEME::glm_extra_vars`. They are requested from
#' `AEME::read_model_outputs()` via `extra_vars` (never trimmed from
#' `output.nc`: GLM writes the netCDF in full, so a variable is present
#' whenever the model has the module that produces it) and reduced to a single
#' time series: a per-zone variable by its `zone` (and `sed_layer`), a `_lake`
#' variable by AEME's area-weighted summary.
#'
#' @param v character; AEME name, e.g. `"SEDL_Fsed_oxy"`, `"SED_Fsed_oxy"` or
#'   `"SED_Fsed_oxy_lake"`.
#' @param idx list; the region's index from `.region_var_indices()` (only the
#'   time axis, `date_index` and `dates`, is used).
#' @param zone,sed_layer integer or `NULL`; see `check_sa_extra_vars()`.
#' @inheritParams .raf_extract_var
#' @return long dataframe like `.raf_extract_var()` (`depth` is `NA`), or
#'   `NULL` on any failure.
#' @noRd
.raf_extract_extra_var <- function(v, idx, nc, lake_dir, model, name = NULL,
                                   zone = NULL, sed_layer = NULL) {

  if (!identical(model, "glm_aed")) {
    .raf_warn_read(v, paste0("sediment variables are GLM-AED only, not ",
                             model))
    return(NULL)
  }
  date_index <- idx[["date_index"]]
  dates      <- idx[["dates"]]
  if (length(date_index) == 0) return(NULL)

  info <- glm_extra_var_info(v)
  reg  <- .glm_extra_registry()
  raw  <- paste0(reg$glm_aed[match(sub("_lake$", "", v), reg$var_aeme)],
                 if (info$lake) "_lake" else "")

  # `vars_sim = NULL`: only the extra variable is wanted, not the depth
  # profiles. A `_lake` request makes AEME read the zone array behind it and
  # build the series (see `?AEME::read_glm_output`).
  out <- tryCatch(
    AEME::read_model_outputs(nc = nc, lake_dir = lake_dir, model = model,
                             vars_sim = NULL, date_index = date_index,
                             incl_fluxes = FALSE, extra_vars = v),
    error = function(e) structure(list(ok = FALSE, reason = conditionMessage(e)),
                                  class = "model_output_error"))
  if (AEME::is_model_error(out)) {
    .raf_warn_read(v, out$reason)
    return(NULL)
  }

  # Held by raw netCDF name: flat for a (time) series, otherwise in the
  # `sediment` / `diag` sub-list.
  val <- out[["sediment"]][[raw]] %||% out[["diag"]][[raw]] %||% out[[raw]]
  if (is.null(val)) {
    .raf_warn_read(v, paste0(raw, " not in the GLM output (is the sediment / ",
                             "AED module that writes it switched on?)"))
    return(NULL)
  }

  if (inherits(val, "aeme_grouped_var")) {
    arr <- val$value
    pick <- lapply(dim(arr), seq_len)
    pos <- function(d) which(val$dim_names == d)
    if (length(pos("nzones")) == 1) {
      if (is.null(zone) || zone > dim(arr)[pos("nzones")]) {
        .raf_warn_read(v, paste0("zone ", zone %||% "<none>", " not in the ",
                                 dim(arr)[pos("nzones")], " sediment zone(s)"))
        return(NULL)
      }
      pick[[pos("nzones")]] <- zone
    }
    if (length(pos("sed_layers")) == 1) {
      layer <- sed_layer %||% 1L
      if (layer > dim(arr)[pos("sed_layers")]) {
        .raf_warn_read(v, paste0("sed_layer ", layer, " not in the ",
                                 dim(arr)[pos("sed_layers")], " layer(s)"))
        return(NULL)
      }
      pick[[pos("sed_layers")]] <- layer
    }
    val <- as.vector(do.call(`[`, c(list(arr), pick, list(drop = TRUE))))
  }
  val <- as.numeric(val)

  if (length(val) < length(dates)) {
    AEME::cli_safe(paste0("Fewer timesteps than requested for variable ", v,
                          "; trimming dates."), FUN = cli::cli_alert_warning)
    dates <- dates[seq_along(val)]
  }
  df <- data.frame(Date = dates, depth = NA_real_, model = val[seq_along(dates)],
                   var_aeme = v, stringsAsFactors = FALSE)
  if (!is.null(name)) df$name <- name
  df
}
