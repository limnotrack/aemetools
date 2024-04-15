#' Get layer IDs for a given spatial object
#'
#' This function takes a spatial object and returns the layer IDs for the
#' aerial imagery and DEM layers that intersect with the spatial object.
#'
#' @inheritParams sf::st_within
#' @param type A character vector of length 1 or 2, specifying the type of
#'  layers to return. The default is c("dem", "aerial"). If only one type is
#'  specified, the function will only return layers of that type.
#'
#' @return A data frame with the layer IDs of the layers that intersect with
#' the spatial object.
#' @export
#'

get_layer_ids <- function(x, type = c("dem", "aerial")) {

  # Load in data from the package
  df <- data.frame() # Empty data frame to store results
  if ("aerial" %in% type) {
    utils::data("nz_aerial_imagery_metadata", package = "aemetools")
    lyr <- nz_aerial_imagery_metadata[sf::st_within(x = x,
                                                    nz_aerial_imagery_metadata,
                                                    sparse = FALSE), ]
    df <- rbind(df, lyr)
  }
  if ("dem" %in% type) {
    utils::data("nz_dem_metadata", package = "aemetools")
    lyr <- nz_dem_metadata[sf::st_within(x, nz_dem_metadata,
                                         sparse = FALSE), ]
    df <- rbind(df, lyr)
  }
  return(df)
}
