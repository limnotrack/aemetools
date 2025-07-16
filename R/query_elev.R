#' Query elevation data for a shape within a DEM raster
#'
#' @description
#' This function queries the elevation data for a shape within a DEM raster. If
#' the latitude and longitude are not provided, the function will use the
#' centroid of the shape. If the elevation data is not found for the first
#' point, the function will try other points on the shape.
#'
#'
#' @param x sf object
#' @param dem SpatRaster object
#' @param lat numeric; latitude to query
#' @param lon numeric; longitude to query
#' @param layer_id integer; layer id of the DEM raster
#'
#' @importFrom terra metags
#' @importFrom sf st_point_on_surface st_transform st_coordinates
#' @importFrom dplyr rename filter pull
#'
#' @return numeric; elevation value
#' @export
#'

query_elev <- function(x, dem, lat, lon, layer_id = NULL) {

  if (is.null(layer_id)) {
    meta_tags <- terra::metags(dem)
    if (length(meta_tags) == 0) stop("No metadata found for DEM!")

    if (is.data.frame(meta_tags)) {
      layer_id <- meta_tags |>
        dplyr::filter(name == "layer_id") |>
        dplyr::pull(value) |>
        as.numeric()
    } else {
      layer_id <- as.numeric(meta_tags[["layer_id"]])
    }

    if (is.na(layer_id)) stop("No layer_id found in metadata for DEM!")
  }
  crs <- sf::st_crs(x)

  if (missing(lat) | missing(lon)) {
    cent_sf <- x |>
      sf::st_point_on_surface()
    cent <- cent_sf |>
      sf::st_transform(4326) |>
      sf::st_coordinates() |>
      data.frame()  |>
      dplyr::rename(lat = Y, lon = X)
  } else {
    cent <- data.frame(lat = lat, lon = lon)
    cent_sf <- sf::st_as_sf(cent, coords = c("lon", "lat"), crs = 4326) |>
      sf::st_transform(crs)
  }

  qu_elev <- get_raster_layer_value(lat = cent$lat, lon = cent$lon,
                                    layer_id = layer_id)

  # dem_elev <- terra::extract(dem, cent_sf)

  if (is.na(qu_elev)) {
    message("No elevation data found for first point! Trying other points on the lake...")
    # dist <- c(50, 75, 100)
    pnts <- sf::st_sample(x, size = 10) |>
      sf::st_transform(4326) |>
      sf::st_coordinates() |>
      data.frame() |>
      dplyr::rename(lat = Y, lon = X)

    for (p in 1:nrow(pnts)) {
      qu_elev <- get_raster_layer_value(lat = pnts$lat[p], lon = pnts$lon[p],
                                        layer_id = layer_id)
      if (!is.na(qu_elev)) {
        message("Found a point with elevation data!")
        cent <- pnts[p, ]
        cent_sf <- sf::st_as_sf(cent, coords = c("lon", "lat"), crs = 4326) |>
          sf::st_transform(crs)
        break
      }
    }
  }

  if (is.na(qu_elev)) stop("No elevation data found!")

  # Check point in DEM
  if (!missing(dem)) {
    dem_elev <- terra::extract(dem, cent_sf)
    message("Sanity check:")
    message("\tElevation from DEM: ", dem_elev$elevation)
    message("\tElevation from query: ", qu_elev)
  }

  return(qu_elev)
}
