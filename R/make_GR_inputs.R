#' Compute airGR inputs from a spatial HydroModel
#'
#' Takes an existing [HydroModel] (created by [make_hydro_model()]) and
#' populates its airGR model-input slots (`inputs_model`, `data`, `start`,
#' `fun_mod`, `fun_mod_name`) using the supplied meteorological and observed
#' discharge data.
#'
#' Potential evapotranspiration is estimated using the Oudin method via
#' [airGR::PE_Oudin()].
#'
#' @param hydro_model [HydroModel]; a spatial model object created by
#'   [make_hydro_model()].
#' @param met data.frame; containing `Date`, air temperature
#'   (`MET_tmpair`) and precipitation (`MET_pprain`).
#' @param obs_flow data.frame; containing `Date` and flow in m³/s.
#'   If `NULL`, no observed discharge is merged.
#' @param lat numeric; latitude in decimal degrees. If `NULL`, the centroid
#'   latitude of `hydro_model@lake` (in WGS 84) is used.
#' @param FUN_MOD function; `airGR` model function to use. Defaults to
#'   [airGR::RunModel_GR6J].
#'
#' @import airGR
#' @importFrom methods new
#' @importFrom sf st_transform st_centroid st_coordinates
#' @importFrom dplyr left_join mutate pull
#'
#' @return The input [HydroModel] with its airGR slots (`inputs_model`,
#'   `data`, `start`, `fun_mod`, `fun_mod_name`) populated and ready for
#'   [calib_GR()] or [run_GR()].
#' @export

make_GR_inputs <- function(hydro_model, met, obs_flow = NULL, lat = NULL,
                           FUN_MOD = airGR::RunModel_GR6J) {

  fun_mod_name <- deparse(substitute(FUN_MOD))

  if (is.null(lat)) {
    lat <- hydro_model@lake |>
      sf::st_transform(4326) |>
      sf::st_centroid() |>
      sf::st_coordinates() |>
      as.data.frame() |>
      dplyr::pull(Y)
  }

  catch_area <- hydro_model@catchment_area

  obs_flow <- obs_flow |>
    dplyr::mutate(Qmm = 1000 * (obs_flow[, 2] * 86400 / catch_area),
                  Qm3 = obs_flow[, 2])

  all <- dplyr::left_join(met, obs_flow, by = "Date")

  if (any(is.na(all[, 4]))) {
    if (all(is.na(all[, 4]))) {
      stop("No overlapping data between met and inflow data.")
    }

    warning("NA values present. Selecting period with less NA's.")

    start <- min(which(!is.na(all[, 4])))
    end   <- max(which(!is.na(all[, 4])))

    if (start < 547) {
      start <- 547
    }

    all <- all[1:end, ]
  } else {
    start <- ceiling(1.5 * 365)
  }

  all$MET_poteva <- airGR::PE_Oudin(
    JD          = as.numeric(strftime(all$Date, format = "%j")),
    Temp        = all$MET_tmpair,
    Lat         = lat,
    LatUnit     = "deg",
    TimeStepIn  = "daily",
    TimeStepOut = "daily"
  )

  InputsModel <- airGR::CreateInputsModel(
    FUN_MOD = FUN_MOD,
    DatesR  = as.POSIXct(all$Date),
    Precip  = all$MET_pprain,
    PotEvap = all$MET_poteva
  )

  # Return a new HydroModel with the GR slots populated; spatial slots
  # are copied unchanged from the input object.
  methods::new(
    "HydroModel",
    channels       = hydro_model@channels,
    lake           = hydro_model@lake,
    catchments     = hydro_model@catchments,
    dem            = hydro_model@dem,
    land_cover     = hydro_model@land_cover,
    soil           = hydro_model@soil,
    outlet         = hydro_model@outlet,
    crs            = hydro_model@crs,
    catchment_area = catch_area,
    inputs_model   = InputsModel,
    data           = all,
    start          = as.numeric(start),
    fun_mod        = FUN_MOD,
    fun_mod_name   = fun_mod_name
  )
}

