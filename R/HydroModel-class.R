#' HydroModel S4 class
#'
#' An S4 class representing a hydrological model configuration for use with
#' the `airGR` package. It bundles the model inputs, observed data, and
#' optional spatial data (DEM, land cover, soil) into a single object.
#'
#' @slot inputs_model airGR `InputsModel` object created by
#'   [airGR::CreateInputsModel()].
#' @slot data data.frame; combined meteorological and observed flow data.
#' @slot start numeric; row index in `data` where the observation period
#'   begins (rows before this are used as the warm-up period).
#' @slot fun_mod function; airGR model function (e.g.
#'   [airGR::RunModel_GR4J]).
#' @slot fun_mod_name character; display name of the model function.
#' @slot catchment_area numeric; total catchment area in m².
#' @slot dem ANY; optional DEM raster (e.g. a `terra::SpatRaster`).
#'   Defaults to `NULL`.
#' @slot land_cover ANY; optional land cover data. Defaults to `NULL`.
#' @slot soil ANY; optional soil data. Defaults to `NULL`.
#'
#' @importFrom methods new setClass setValidity setMethod show
#' @exportClass HydroModel

setClass(
  "HydroModel",
  representation(
    inputs_model  = "ANY",
    data          = "data.frame",
    start         = "numeric",
    fun_mod       = "function",
    fun_mod_name  = "character",
    catchment_area = "numeric",
    dem           = "ANY",
    land_cover    = "ANY",
    soil          = "ANY"
  ),
  prototype(
    dem        = NULL,
    land_cover = NULL,
    soil       = NULL
  )
)

setValidity("HydroModel", function(object) {
  messages <- character(0)

  if (length(object@start) != 1 ||
      object@start < 1 ||
      object@start > nrow(object@data)) {
    messages <- c(messages,
                  paste0("'start' (", object@start, ") must be a valid row ",
                         "index in 'data' (1 to ", nrow(object@data), ")."))
  }

  if (length(object@catchment_area) != 1 || object@catchment_area <= 0) {
    messages <- c(messages, "'catchment_area' must be a single positive number.")
  }

  if (length(messages) > 0) {
    return(messages)
  }
  TRUE
})

#' @describeIn HydroModel Print a concise summary of a `HydroModel` object.
#' @param object A `HydroModel` object.
setMethod("show", "HydroModel", function(object) {
  date_range <- tryCatch(
    format(range(object@data$Date), "%Y-%m-%d"),
    error = function(e) c("?", "?")
  )
  n_warmup <- object@start - 1L
  n_obs    <- nrow(object@data) - object@start + 1L

  cat("HydroModel\n")
  cat("  Model:          ", object@fun_mod_name, "\n")
  cat("  Data rows:      ", nrow(object@data),
      " (", date_range[1], " to ", date_range[2], ")\n", sep = "")
  cat("  Warm-up rows:   ", n_warmup, "\n")
  cat("  Observation rows:", n_obs, "\n")
  cat("  Catchment area: ",
      format(object@catchment_area / 1e6, digits = 4, nsmall = 2), " km\u00b2\n",
      sep = "")
  if (!is.null(object@dem))        cat("  DEM:             provided\n")
  if (!is.null(object@land_cover)) cat("  Land cover:      provided\n")
  if (!is.null(object@soil))       cat("  Soil data:       provided\n")
  invisible(object)
})
