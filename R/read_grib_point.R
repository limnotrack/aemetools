#' Read a point from a grib file
#'
#' @param file character; path to the grib file. Can be a vector of paths.
#' @inheritParams download_era5_grib
#' @inheritParams terra::extract
#' 
#' @importFrom terra rast extract vect project units describe
#'
#' @return A data frame with the extracted data.
#' @export
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
#' df <- read_grib_point(file = files, lat = lat, lon = lon)
#' }

read_grib_point <- function(file, shape = NULL, lat, lon, method = "bilinear") {
  out <- lapply(file, \(f) {

    if (!file.exists(f)) {
      stop("Missing the file: ", f)
    }

    # Read the file
    r <- terra::rast(f)

    # Extract point data
    if (!is.null(shape)) {
      shape_vec <- terra::vect(shape)
      shape_vec <- terra::project(shape_vec, r)
      v <- terra::extract(r, shape_vec, fun = mean, ID = FALSE, weights = TRUE) |>
        as.numeric()
    } else {
      v <- terra::extract(r, cbind(lon, lat), method = method) |>
        as.numeric()
    }

    if (all(is.na(v))) {
      warning("No data found for the point for ", f, ". Returning NA.")
    }

    # Extract units
    units <- terra::units(r) |>
      unique()

    # Extract variable name
    var_name <- terra::names(r) |>
      unique()

    # Extract variable short name
    metadata <- terra::describe(f)
    grib_element <- metadata[which(grepl("GRIB_ELEMENT", metadata))[1]]
    short_name <- trimws(gsub("GRIB_ELEMENT=", "", grib_element))

    # Extract timestamp data
    time <- terra::time(r)
    df <- data.frame(DateTime = time, #lat = lat, lon = lon,
                     value = v, units = units, variable = var_name, short_name = short_name)

    return(df)
  })

  dat <- out |>
    dplyr::bind_rows() |>
    dplyr::arrange(DateTime)
  return(dat)
}
