#' Download ERA5 GRIB files
#'
#' @description
#' Download ERA5 meteorological data from the Copernicus Data Store (CDS).
#' Create a free CDS user account by \href{https://cds.climate.copernicus.eu/user/register}{self
#'  registering}.
#' Once your user account has been verified you can get your personal user ID
#' and key by visiting the \href{https://cds.climate.copernicus.eu/user}{user
#' profile}. Further information can be found on the
#' \href{https://bluegreen-labs.github.io/ecmwfr/#use}{ecmwfr} package page.
#'
#' @param shape sf object; shapefile or spatial object to download data for.
#' @param lat numeric; latitude
#' @param lon numeric; longitude
#' @param buffer numeric; buffer around the point in degrees. Default is 0.1.
#' @param variable vector of ERA5 variable names e.g.
#' "2m_temperature", "total_precipitation". Default variables are:
#' "10m_u_component_of_wind", "10m_v_component_of_wind",
#' "2m_dewpoint_temperature", "2m_temperature", "snowfall",
#' "surface_pressure", "surface_solar_radiation_downwards",
#' "surface_thermal_radiation_downwards", "total_precipitation".
#' @param year numeric; year or vector of years.
#' @param month numeric; month or vector of months. Defaults to 1:12.
#' @param site string of site name which will be appended to the file
#' @inheritParams ecmwfr::wf_set_key
#' @param era5_dataset string; of which ERA5 dataset to use. Can be 'reanalysis-era5-single-levels' or 'reanalysis-era5-land'
#' @param path filepath to store downloaded file
#'
#' @importFrom plyr round_any
#' @importFrom ecmwfr wf_check_request wf_request_batch
#'
#' @examples
#' \dontrun{
#' lat <- -38.07782
#' lon <- 176.2673
#' year <- 2024
#' month <- 1:2
#' variable <- "2m_temperature"
#' path <- "data/test"
#' ecmwfr::wf_set_key(key = "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX",
#'                    user = "person@email.com")
#' files <- download_era5_grib(lat = lat, lon = lon, year = year, month = month,
#'                             variable = variable, path = path, user = user)
#' files
#' }
#' @export
#'

download_era5_grib <- function(shape = NULL,
                               lat,
                               lon,
                               buffer = 0.1,
                               variable = c("10m_u_component_of_wind",
                                            "10m_v_component_of_wind",
                                            "2m_dewpoint_temperature",
                                            "2m_temperature", "snowfall",
                                            "surface_pressure",
                                            "surface_solar_radiation_downwards",
                                            "surface_thermal_radiation_downwards",
                                            "total_precipitation"),
                               year = 2022,
                               month = 1:2,
                               site  = "test",
                               user = NULL,
                               era5_dataset = "reanalysis-era5-land",
                               path = ".") {

  # Create dir if it does not exist
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  time <- c('00:00', '01:00', '02:00',
            '03:00', '04:00', '05:00',
            '06:00', '07:00', '08:00',
            '09:00', '10:00', '11:00',
            '12:00', '13:00', '14:00',
            '15:00', '16:00', '17:00',
            '18:00', '19:00', '20:00',
            '21:00', '22:00', '23:00')
  timestep <- "hourly"

  if (!is.null(shape)) {
    bbox <- shape |>
      sf::st_bbox()
    area <- c(plyr::round_any(bbox["ymax"], accuracy = 0.1, f = ceiling),
              plyr::round_any(bbox["xmin"], accuracy = 0.1, f = floor),
              plyr::round_any(bbox["ymin"], accuracy = 0.1, f = floor),
              plyr::round_any(bbox["xmax"], accuracy = 0.1, f = ceiling)
              )
  } else {
    area <- c(plyr::round_any(lat + buffer, accuracy = 0.1, f = ceiling),
              plyr::round_any(lon - buffer, accuracy = 0.1, f = floor),
              plyr::round_any(lat - buffer, accuracy = 0.1, f = floor),
              plyr::round_any(lon + buffer, accuracy = 0.1, f = ceiling)
              )
  }


  # Generate a list of requests for each variable fo each year and each month
  request_list <- list()
  for (v in variable) {
    for (y in year) {
      for (m in month) {
        list_name <- paste0(v, "_", y, "_", m)
        request_list[[list_name]] <- list(
          dataset_short_name = era5_dataset, # "reanalysis-era5-single-levels", #
          product_type   = "reanalysis",
          download_format = "unarchived",
          data_format = "grib",
          variable = v,
          year = y,
          month = m,
          day = c("01","02","03","04","05","06","07","08","09","10",
                  "11","12","13","14","15","16","17","18","19","20",
                  "21","22","23","24","25","26","27","28","29","30","31"),
          time = time,
          area = area,
          target = paste0(era5_dataset, "_", v, "_", timestep, "_", y, "_", m,
                          "_", site, ".grib")
        )
      }
    }
  }

  # Check requests
  for (req in request_list) {
    ecmwfr::wf_check_request(req)
  }

  if (length(request_list) <= 20) {
    req <- ecmwfr::wf_request_batch(request_list = request_list, path = path,
                                    user = user, workers = length(request_list))
  } else {
    stop("The number of requests is too large. Please reduce the number of requests to 20 or less.")
  }

  return(req)
}
