#' Convert ERA5 netCDF files to AEME or LER
#'
#' @description
#' Convert ERA5 netCDF files to AEME or LakeEnsemblR formats.
#'
#' @param lat numeric; latitude
#' @param lon numeric; longitude
#' @param variable string with ERA5 variable names e.g. "2m_temperature", "total_precipitation"
#' @param year numeric; vector with years
#' @param site string of site name which was used when downloading the data.
#' @param path filepath to where the downloaded ERA5 ncdf files are stored.
#' @param format string; Either "AEME" or "LER". Default is "AEME".
#'
#' @importFrom stars read_ncdf st_extract
#' @importFrom sf st_as_sf
#' @importFrom stats aggregate
#' @importFrom utils data
#'
#' @export
#'

convert_era5_ncdf <- function(lat,
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
                              site  = "test",
                              path = ".",
                              format = "AEME") {

  # Load Rdata
  utils::data("era5_ref_table", package = "aemetools", envir = environment())

  coords <- data.frame(lat = lat, lon = lon)
  coords_sf <- sf::st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
  timestep <- "hourly"

  out <- lapply(variable, \(v) {
    out2 <- lapply(year, \(y) {
      file <- file.path(path, paste0("era5", "_", v, "_", timestep, "_", y,
                                     "_", site, ".nc"))

      if (!file.exists(file)) {
        stop("Missing the file: ", file)
      }
      var <- era5_ref_table$nc[era5_ref_table$era5 == v]
      dat <- stars::read_ncdf(file, var = var)
      if (var %in% c("t2m", "d2m")) {
        met_max <- stats::aggregate(dat, by = "day", FUN = max)
        met_min <- stats::aggregate(dat, by = "day", FUN = min)
        met <- stats::aggregate(dat, by = "day", FUN = mean)
      } else if(var %in% c("sf", "tp")) {
        met <- stats::aggregate(dat, by = "day", FUN = max)
      } else if(var %in% c("ssrd", "strd")) {
        met <- stats::aggregate(dat, by = "day", FUN = function(x) {
          max(x) / (24 * 60 * 60)
          }
          )
      } else {
        met <- stats::aggregate(dat, by = "day", FUN = mean)
      }

      df <- met |>
        stars::st_extract(coords_sf) |>
        as.data.frame()
      df <- df[, c("time", var)]
      if (var %in% c("t2m", "d2m")) {
        df_max <- met_max |>
          stars::st_extract(coords_sf) |>
          as.data.frame()
        df_max <- df_max[, var]
        df_min <- met_min |>
          stars::st_extract(coords_sf) |>
          as.data.frame()
        df_min <- df_min[, var]
        df[[paste0(var, "_max")]] <- df_max
        df[[paste0(var, "_min")]] <- df_min
      }
      return(df)
    })
    out2 <- dplyr::bind_rows(out2)
  })

  met <- Reduce(merge, out)

  if ("t2m" %in% colnames(met) | "d2m" %in% colnames(met)) {
    sel_cols <- which(colnames(met) %in% c("t2m", "d2m"))
    sel_cols <- grepl("t2m|d2m", names(met))
    met[, sel_cols] <- met[, sel_cols] - 273.15
  }
  if ("tp" %in% colnames(met) | "sf" %in% colnames(met)) {
    sel_cols <- which(colnames(met) %in% c("tp", "sf"))
    met[, sel_cols] <- met[,sel_cols] * 1000 # Convert to m
  }

  if (format == "AEME") {
    names(met)[!grepl("min|max", names(met))] <- era5_ref_table$aeme[match(
      names(met[!grepl("min|max", names(met))]), era5_ref_table$nc)]

    names(met)[names(met) == "d2m_max"] <- "MET_dewmax"
    names(met)[names(met) == "d2m_min"] <- "MET_dewmin"
    names(met)[names(met) == "t2m_max"] <- "MET_airmax"
    names(met)[names(met) == "t2m_min"] <- "MET_airmin"

  } else if (format == "LER") {
    names(met) <- era5_ref_table$ler[match(names(met), era5_ref_table$nc)]
  }

  return(met)
}
