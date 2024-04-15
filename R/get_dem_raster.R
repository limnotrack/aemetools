#' Get DEM raster from LINZ
#'
#' @param x sf object
#' @param lake sf object; of the lake shape. Optional. Default is NULL.
#' @param zoom integer; zoom level for the raster. Default is 14.
#' @param prompt logical; whether to prompt the user to select the DEM layer.
#' Default is FALSE. This can be useful if the user wants to cycle through the
#' available DEM layers to find the best one.
#' @inheritParams get_raster_tile
#'
#' @details This function will get the DEM raster from LINZ for the given
#' shape.
#'
#' @importFrom terra nlyr subset
#' @importFrom sf st_buffer st_point_on_surface st_transform st_coordinates
#'
#' @return SpatRaster object
#' @export
#'

get_dem_raster <- function(x, lake = NULL, zoom = 14, verbose = FALSE,
                           prompt = FALSE) {
  buff_shape <- sf::st_buffer(x, 150)
  dem_df <- get_layer_ids(buff_shape, type = "dem") |>
    dplyr::arrange(res)

  for (i in 1:(nrow(dem_df))) {
    dem_lake <- tryCatch({
      get_raster_tile(x = buff_shape, layer_id = dem_df$layer_id[i],
                      zoom = zoom, verbose = verbose)
    }, error = function(e) {
      message("Error: ", e)
      return(NULL)
    })

    if (is.null(dem_lake)) {
      next
    }

    # Subset to just one band
    if (terra::nlyr(dem_lake) > 1) {
      dem_lake <- terra::subset(dem_lake, 1)
      names(dem_lake) <- c("elevation")
    }

    # Check if elevation match up
    if (is.null(lake)) {
      cent_sf  <- x |>
        sf::st_point_on_surface()
    } else {
      cent_sf  <- lake |>
        sf::st_point_on_surface()
    }

    cent <- cent_sf |>
      sf::st_transform(4326) |>
      sf::st_coordinates() |>
      data.frame()

    qu_elev <- query_elev(x, layer_id = dem_df$layer_id[i])

    # tmap plot
    # tmap::tm_shape(dem_lake) +
    #   tmap::tm_raster(style = "cont") +
    #   tmap::tm_shape(x) +
    #   tmap::tm_borders(col = "blue", lwd = 2) +
    #   tmap::tm_shape(cent_sf) +
    #   tmap::tm_dots(col = "red", size = 0.1)

    # Extract point data from dem_lake
    elev <- terra::extract(dem_lake, cent_sf) |>
      dplyr::select(elevation)
    if (elev == 0) {
      dem_lake <- dem_lake + unlist(qu_elev)
    } else {
      # Calculate translation factor
      trans_elev <- qu_elev / elev

      # Convert DEM to meters
      dem_lake <- dem_lake * unlist(trans_elev)
    }

    # Get sf object for bbox of dem_lake
    bbox <- terra::ext(dem_lake) |>
      terra::as.polygons() |>
      sf::st_as_sf() |>
      sf::st_set_crs(2193)

    # Check if lake shape is in bbox
    if (sf::st_within(x, bbox, sparse = FALSE)) {

      # tmap::tmap_mode("view")
      # t1 <- tmap::tm_shape(dem_lake) +
      #   tmap::tm_raster(style = "cont") +
      #   tmap::tm_shape(x) +
      #   tmap::tm_borders(col = "green") +
      #   tmap::tm_basemap("Esri.WorldImagery")
      # print(t1)

      if (prompt) {
        use_image <- readline(prompt = "Use image (y/n): ")
      } else {
        use_image <- "y"
      }

      if (use_image == "y") {
        break
      }
    }
  }

  # Add metadata tags
  terra::metags(dem_lake) <- c(paste0("layer_id=", dem_df$layer_id[i]),
                               paste0("layer_title=", dem_df$title[i]),
                               paste0("year=", dem_df$year[i]))

  return(dem_lake)
}
