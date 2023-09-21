#' Make inputs for the GR models
#'
#' @param id numeric; Reach ID
#' @param reaches sf; object with reaches as linestrings.
#' @param lake sf; polygon of lake shore.
#' @param catchments sf; polygon of catchmentss.
#' @param obs_flow dataframe; containing Date and flow in m3/s.
#' @param met dataframe; containing Date, air temperature and precipitation
#' @param lat numeric; latitude. If NULL, uses the latitude from the centre
#' of the lake.
#' @param FUN_MOD function from the `airGR` package to be used. Defaults to
#' `airGR::RunModel_GR6J`
#' @param plot logical; plot the reaches, lake and catchment? Defaults to
#' FALSE.
#'
#' @import airGR
#' @importFrom sf st_crs st_difference st_union st_area
#' @importFrom dplyr filter mutate select rename
#' @importFrom units drop_units
#'
#' @return list of inputs for `run_GR()`.
#' @export

make_GR_inputs <- function(id, reaches, lake, catchments, obs_flow = NULL,
                           met, lat = NULL, FUN_MOD = airGR::RunModel_GR6J,
                           plot = FALSE) {

  if (!(sf::st_crs(reaches) == sf::st_crs(lake) &
        sf::st_crs(reaches) == sf::st_crs(catchments))) {
    stop(strwrap("Coordinate reference systems are different between reaches,
                 lake and catchment. Ensure they are all on the same CRS."))
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
      # ggplot2::geom_sf(data = catchments, fill = "#F0C9C0") +
      ggplot2::geom_sf(data = tot_catchm, fill = "#EDB48E") +
      ggplot2::geom_sf(data = tot_rivers, colour = "blue") +
      ggplot2::theme_bw()
    print(p)
  }

  # Calculate catchment area for conversion from m3/s to mm/day
  catch_area <- units::drop_units(sf::st_area(tot_catchm)) #m^2

  obs_flow <- obs_flow |>
    dplyr::mutate(Qmm = 1000 * (obs_flow[, 2] * 86400 / catch_area),
                  Qm3 = obs_flow[, 2])

  all <- dplyr::left_join(met, obs_flow, by = "Date")


  if (any(is.na(all[, 4]))) {
    # warning("Missing dates!\n", paste0(date_seq[!(date_seq %in% obs_flow$Date)], collapse = "\n"), "\nAdding in 0 values")

    if (all(is.na(all[, 4]))) {
      stop("No overlapping data between met and inflow data.")
    }

    warning("NA values present. Selecting period with less NA's.")


    start <- min(which(!is.na(all[, 4])))
    end <- max(which(!is.na(all[, 4])))

    if (start > end) {
      end <- max(which(!is.na(all[, 4])))
    }

    if (start < 547) {
      start <- 547
    }

    all <- all[1:end, ]
  } else {
    start <- ceiling(1.5 * 365)
  }

  all$MET_poteva <- airGR::PE_Oudin(JD = as.numeric(strftime(all$Date,
                                                             format = "%j")),
                                    Temp = all$MET_tmpair, Lat = lat,
                                    LatUnit = "deg", TimeStepIn = "daily",
                                    TimeStepOut = "daily")


  InputsModel <- airGR::CreateInputsModel(FUN_MOD = FUN_MOD,
                                          DatesR = as.POSIXct(all$Date),
                                          Precip = all$MET_pprain,
                                          PotEvap = all$MET_poteva)

  list(InputsModel = InputsModel, data = all, start = start, FUN_MOD = FUN_MOD,
       catchment_area = catch_area)

}
