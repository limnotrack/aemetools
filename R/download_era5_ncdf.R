#' Download ERA5 netCDF files
#'
#' @description
#' Download ERA5 meteorological data from the Copernicus Data Store (CDS).
#' Create a free CDS user account by \href{https://cds.climate.copernicus.eu/user/register}{self
#'  registering}.
#' Once your user account has been verified you can get your personal user ID
#' and key by visiting the \href{https://cds.climate.copernicus.eu/user}{user
#' profile}.
#'
#'
#' @param lat numeric; latitude
#' @param lon numeric; longitude
#' @param variable vecto;r with string with ERA5 variable names e.g.
#' "2m_temperature", "total_precipitation"
#' @param year numeric; year or vector of years.
#' @param month numeric; month or vector of months. Defaults to 1:12.
#' @param site string of site name which will be appended to the file
#' @param user user ID linked with Copernicus account
#' @param era5_dataset string; of which ERA5 dataset to use. Can be 'reanalysis-era5-single-levels' or 'reanalysis-era5-land'
#' @param path filepath to store downloaded file
#' @param job logical; send the request to a background job in RStudio. Only
#' works in RStudio. Default = TRUE.
#'
#' @importFrom plyr round_any
#' @importFrom ecmwfr wf_request_batch
#' @importFrom job job
#'
#' @examples
#' \dontrun{
#' path <- 'era5_folder'
#' site <- 'lake'
#' ecmwfr::wf_set_key(user = '123456',
#'                    key = 'XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX',
#'                    service = 'cds')
#'
#' download_era5(lat = lat, lon = lon, year = 2022,
#'               user = user, path = path)
#' met <- convert_era5(lat = lat, lon = lon, year = 2022,
#'                     site = site, path = path)
#' }
#'
#'
#' @export
#'

download_era5_ncdf <- function(lat,
                               lon,
                               variable = c("10m_u_component_of_wind",
                                       "10m_v_component_of_wind",
                                       "2m_dewpoint_temperature",
                                       "2m_temperature", "snowfall",
                                       "surface_pressure",
                                       "surface_solar_radiation_downwards",
                                       "surface_thermal_radiation_downwards",
                                       "total_precipitation"),
                               year = 2022,
                               month = 1:12,
                               site  = "test",
                               user = NULL,
                               era5_dataset = "reanalysis-era5-land",
                               path = ".",
                               job = TRUE) {

  time <- c('00:00', '01:00', '02:00',
             '03:00', '04:00', '05:00',
             '06:00', '07:00', '08:00',
             '09:00', '10:00', '11:00',
             '12:00', '13:00', '14:00',
             '15:00', '16:00', '17:00',
             '18:00', '19:00', '20:00',
             '21:00', '22:00', '23:00')
  timestep <- "hourly"
  area <- paste0(plyr::round_any(lat - 0.1, accuracy = 0.1, f = floor), "/",
                 plyr::round_any(lon - 0.1, accuracy = 0.1, f = floor), "/",
                 plyr::round_any(lat + 0.1, accuracy = 0.1, f = ceiling), "/",
                 plyr::round_any(lon + 0.1, accuracy = 0.1, f = ceiling))

  if (length(variable) >= length(year)) {
    for (v in variable) {
      request_list <- lapply(year, \(y) {
        list(
          dataset_short_name = era5_dataset, # "reanalysis-era5-single-levels", #
          product_type   = "reanalysis",
          format = "netcdf",
          variable = v,
          year = y,
          month = month,
          day = c("01","02","03","04","05","06","07","08","09","10",
                  "11","12","13","14","15","16","17","18","19","20",
                  "21","22","23","24","25","26","27","28","29","30","31"),
          time = time,
          area = area,
          target = paste0("era5", "_", v, "_", timestep, "_", y, "_", site, ".nc")
        )
      })

      if (job) {
        job::job({
          ecmwfr::wf_request_batch(request_list = request_list,
                                   path = path, user = user)
        }, title = paste0("Downloading data for ", v, " for ", paste0(year, collapse = ", ")))
      } else {
        ecmwfr::wf_request_batch(request_list = request_list,
                                 path = path, user = user)
      }
    }
  } else {
    for (y in year) {
      request_list <- lapply(variable, \(v) {
        list(
          dataset_short_name = era5_dataset, # "reanalysis-era5-single-levels", #
          product_type   = "reanalysis",
          format = "netcdf",
          variable = v,
          year = y,
          month = month,
          day = c("01","02","03","04","05","06","07","08","09","10",
                  "11","12","13","14","15","16","17","18","19","20",
                  "21","22","23","24","25","26","27","28","29","30","31"),
          time = time,
          area = area,
          target = paste0("era5", "_", v, "_", timestep, "_", y, "_", site, ".nc")
        )
      })

      if (job) {
        job::job({
          ecmwfr::wf_request_batch(request_list = request_list,
                                   path = path, user = user)
        }, title = paste0("Downloading data for ", v, " for ", paste0(year, collapse = ", ")))
      } else {
        ecmwfr::wf_request_batch(request_list = request_list,
                                 path = path, user = user)
      }
    }
  }
}
