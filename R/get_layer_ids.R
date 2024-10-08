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
#' @importFrom dplyr bind_rows
#' @importFrom sf st_within
#'
#' @return A data frame with the layer IDs of the layers that intersect with
#' the spatial object.
#' @export
#'

get_layer_ids <- function(x, type = c("dem", "aerial")) {

  # Load in data from the package
  # df <- data.frame() # Empty data frame to store results
  if ("aerial" %in% type) {
    utils::data("nz_aerial_imagery_metadata", package = "aemetools")
    aer <- nz_aerial_imagery_metadata[sf::st_within(x = x,
                                                    nz_aerial_imagery_metadata,
                                                    sparse = FALSE), ]
    # df <- dplyr::bind_rows(df, lyr)
  }
  if ("dem" %in% type) {
    utils::data("nz_dem_metadata", package = "aemetools")
    dem <- nz_dem_metadata[sf::st_within(x, nz_dem_metadata,
                                         sparse = FALSE), ]
    # df <- dplyr::bind_rows(lyr, df)
  }

  if ("aerial" %in% type & "dem" %in% type) {
    df <- dplyr::bind_rows(aer, dem)
  } else if ("aerial" %in% type) {
    df <- aer
  } else if ("dem" %in% type) {
    df <- dem
  }

  return(df)
}
