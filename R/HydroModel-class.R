#' HydroModel S4 class
#'
#' An S4 class whose primary focus is spatial data for a catchment — river
#' channels, the target lake polygon, subcatchments, and optional DEM / land
#' cover / soil layers.  Computed airGR model inputs (produced by
#' [make_GR_inputs()]) are stored in the same object as optional slots that
#' start as `NULL` and are populated in a second step.
#'
#' **Typical workflow**
#' ```
#' hm <- make_hydro_model(id, reaches, lake, catchments)
#' hm <- make_GR_inputs(hm, met = met, obs_flow = obs_flow,
#'                      FUN_MOD = airGR::RunModel_GR4J)
#' calib  <- calib_GR(hm)
#' output <- run_GR(hm, param = calib$ParamFinalR)
#' ```
#'
#' @slot channels ANY; `sf` object of upstream river channels clipped to
#'   exclude the lake polygon.
#' @slot lake ANY; `sf` polygon of the target lake.
#' @slot catchments ANY; `sf` polygon of the combined upstream catchment with
#'   the lake hole-punched out.
#' @slot dem ANY; optional DEM raster (e.g. a `terra::SpatRaster`).
#'   Defaults to `NULL`.
#' @slot land_cover ANY; optional land cover data. Defaults to `NULL`.
#' @slot soil ANY; optional soil data. Defaults to `NULL`.
#' @slot outlet ANY; optional outlet point(s) as an `sf` object.
#'   Defaults to `NULL`.
#' @slot crs ANY; coordinate reference system of the spatial data (typically
#'   from `sf::st_crs()`). Populated by [make_hydro_model()].
#' @slot catchment_area numeric; total catchment area in m² (excluding lake).
#'   Populated by [make_hydro_model()]. Defaults to `NA_real_`.
#' @slot inputs_model ANY; airGR `InputsModel` object created by
#'   [airGR::CreateInputsModel()]. Populated by [make_GR_inputs()].
#'   Defaults to `NULL`.
#' @slot data ANY; `data.frame` of combined meteorological and observed flow
#'   data. Populated by [make_GR_inputs()]. Defaults to `NULL`.
#' @slot start numeric; row index in `data` where the observation period
#'   begins (rows before this index are the warm-up period). Populated by
#'   [make_GR_inputs()]. Defaults to `NA_real_`.
#' @slot fun_mod ANY; airGR model function (e.g. [airGR::RunModel_GR4J]).
#'   Populated by [make_GR_inputs()]. Defaults to `NULL`.
#' @slot fun_mod_name character; display name of the model function.
#'   Populated by [make_GR_inputs()]. Defaults to `""`.
#'
#' @importFrom methods new setClass setValidity setMethod show
#' @exportClass HydroModel

setClass(
  "HydroModel",
  representation(
    # ---- spatial (primary) --------------------------------------------------
    channels       = "ANY",
    lake           = "ANY",
    catchments     = "ANY",
    dem            = "ANY",
    land_cover     = "ANY",
    soil           = "ANY",
    outlet         = "ANY",
    crs            = "ANY",
    catchment_area = "numeric",
    # ---- computed by make_GR_inputs() ---------------------------------------
    inputs_model   = "ANY",
    data           = "ANY",
    start          = "numeric",
    fun_mod        = "ANY",
    fun_mod_name   = "character"
  ),
  prototype(
    dem            = NULL,
    land_cover     = NULL,
    soil           = NULL,
    outlet         = NULL,
    crs            = NULL,
    catchment_area = NA_real_,
    inputs_model   = NULL,
    data           = NULL,
    start          = NA_real_,
    fun_mod        = NULL,
    fun_mod_name   = ""
  )
)

setValidity("HydroModel", function(object) {
  messages <- character(0)

  if (!is.na(object@catchment_area) && object@catchment_area <= 0) {
    messages <- c(messages,
                  "'catchment_area' must be a positive number (or NA when not yet computed).")
  }

  # Only validate GR slots when they have been populated
  if (!is.null(object@data) && is.data.frame(object@data)) {
    n <- nrow(object@data)
    if (!is.na(object@start) &&
        (length(object@start) != 1 || object@start < 1 || object@start > n)) {
      messages <- c(messages,
                    paste0("'start' (", object@start,
                           ") must be a valid row index in 'data' (1 to ", n, ")."))
    }
  }

  if (length(messages) > 0) return(messages)
  TRUE
})

#' @describeIn HydroModel Print a concise summary of a `HydroModel` object.
#' @param object A `HydroModel` object.
setMethod("show", "HydroModel", function(object) {
  cat("HydroModel\n")

  # ---- spatial section ------------------------------------------------------
  cat("  Spatial data:\n")

  has_crs <- !is.null(object@crs)
  crs_str <- if (has_crs) {
    tryCatch(
      {
        crs_wkt <- object@crs$wkt
        # Extract just the name from WKT if possible
        m <- regmatches(crs_wkt, regexpr('PROJCRS\\["[^"]*"|GEOGCRS\\["[^"]*"',
                                          crs_wkt))
        if (length(m) == 1L) sub('.*\\["', "", sub('"$', "", m)) else "provided"
      },
      error = function(e) "provided"
    )
  } else {
    "not set"
  }
  cat("    CRS:          ", crs_str, "\n")

  if (!is.na(object@catchment_area)) {
    cat("    Catchment:    ",
        format(object@catchment_area / 1e6, digits = 4, nsmall = 2),
        " km\u00b2\n", sep = "")
  }

  for (nm in c("channels", "lake", "catchments")) {
    slot_val <- slot(object, nm)
    if (!is.null(slot_val)) {
      n_feat <- tryCatch(nrow(slot_val), error = function(e) "?")
      cat("    ", formatC(nm, width = 12, flag = "-"),
          n_feat, " feature(s)\n", sep = "")
    }
  }

  for (nm in c("dem", "land_cover", "soil", "outlet")) {
    if (!is.null(slot(object, nm)))
      cat("    ", formatC(nm, width = 12, flag = "-"), "provided\n", sep = "")
  }

  # ---- airGR section (only when populated) ----------------------------------
  if (!is.null(object@inputs_model)) {
    cat("  airGR inputs:\n")
    cat("    Model:        ", object@fun_mod_name, "\n")

    if (!is.null(object@data) && is.data.frame(object@data)) {
      date_range <- tryCatch(
        format(range(object@data$Date), "%Y-%m-%d"),
        error = function(e) c("?", "?")
      )
      n_warmup <- if (!is.na(object@start)) object@start - 1L else "?"
      n_obs    <- if (!is.na(object@start))
        nrow(object@data) - object@start + 1L else "?"

      cat("    Data rows:    ", nrow(object@data),
          " (", date_range[1], " to ", date_range[2], ")\n", sep = "")
      cat("    Warm-up rows: ", n_warmup, "\n")
      cat("    Obs. rows:    ", n_obs, "\n")
    }
  }

  invisible(object)
})

