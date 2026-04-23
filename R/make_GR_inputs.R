#' Make inputs for the GR models
#'
#' Builds a [HydroModel] object containing all inputs required to run or
#' calibrate a hydrological model from the `airGR` package.
#'
#' The function recursively assembles the upstream catchment network from the
#' supplied `nzsegment` ID, calculates the total catchment area (excluding the
#' lake polygon), and constructs the `airGR` `InputsModel` object along with
#' potential evapotranspiration estimates using the Oudin method.
#'
#' @param id numeric; Reach ID (`nzsegment`).
#' @param reaches sf; object with reaches as linestrings.
#' @param lake sf; polygon of lake shore.
#' @param catchments sf; polygon of catchments (including sub-catchments).
#' @param obs_flow data.frame; containing `Date` and flow in m³/s.
#'   If `NULL`, no observed discharge is merged.
#' @param met data.frame; containing `Date`, air temperature
#'   (`MET_tmpair`) and precipitation (`MET_pprain`).
#' @param lat numeric; latitude (degrees). If `NULL`, uses the centroid
#'   latitude of the lake polygon.
#' @param FUN_MOD function; `airGR` model function to use. Defaults to
#'   [airGR::RunModel_GR6J].
#' @param plot logical; plot the reaches, lake and catchment? Defaults to
#'   `FALSE`.
#'
#' @import airGR
#' @importFrom methods new
#' @importFrom sf st_crs st_difference st_union st_area st_transform
#'   st_centroid st_coordinates
#' @importFrom dplyr filter mutate select rename
#' @importFrom units drop_units
#'
#' @return A [HydroModel] object.
#' @export

make_GR_inputs <- function(id, reaches, lake, catchments, obs_flow = NULL,
                           met, lat = NULL, FUN_MOD = airGR::RunModel_GR6J,
                           plot = FALSE) {

  fun_mod_name <- deparse(substitute(FUN_MOD))

  if (!(sf::st_crs(reaches) == sf::st_crs(lake) &
        sf::st_crs(reaches) == sf::st_crs(catchments))) {
    stop(strwrap("Coordinate reference systems are different between reaches,
                 lake and catchment. Ensure they are all on the same CRS."))
  }

  if (is.null(lat)) {
    lat <- lake |>
      sf::st_transform(4326) |>
      sf::st_centroid() |>
      sf::st_coordinates() |>
      as.data.frame() |>
      dplyr::pull(Y)
  }

  hyd_id <- reaches$HydroID[reaches$nzsegment == id]
  upstr <- get_upstream_rec(HydroID = hyd_id, reaches = reaches)
  # Remove reach if it goes through the lake
  upstr <- sf::st_difference(upstr, lake)

  sub_catch <- catchments |>
    dplyr::filter(nzsegment %in% upstr$nzsegment)

  tot_catchm <- sf::st_union(sub_catch)
  tot_catchm <- sf::st_difference(tot_catchm, lake)

  tot_rivers <- sf::st_union(upstr)

  if (plot) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = lake, fill = "cyan") +
      ggplot2::geom_sf(data = tot_catchm, fill = "#EDB48E") +
      ggplot2::geom_sf(data = tot_rivers, colour = "blue") +
      ggplot2::theme_bw()
    print(p)
  }

  # Calculate catchment area for conversion from m3/s to mm/day
  catch_area <- units::drop_units(sf::st_area(tot_catchm)) # m^2

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

  methods::new(
    "HydroModel",
    inputs_model   = InputsModel,
    data           = all,
    start          = as.numeric(start),
    fun_mod        = FUN_MOD,
    fun_mod_name   = fun_mod_name,
    catchment_area = catch_area
  )
}
