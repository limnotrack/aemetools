#' Read spatial feature (sf) data from online databases as sf object
#'
#' @param url character, URL of the web service
#' @inheritParams get_raster_layer_value
#'
#' @importFrom httr2 request req_url_path_append req_url_query
#' @importFrom sf read_sf st_union st_cast st_as_sf st_zm st_write gdal_utils
#'
#' @return sf object
#' @export
#'

read_web_sf <- function(url, layer_id, key = NULL) {

  if (is.null(key)) {
    key <- Sys.getenv("LINZ_KEY")
    if (key == "") {
      stop("No LINZ API key found. See ?add_linz_key for more information.")
    }
  }
  
  # Build WFS request URL using httr2
  req <- httr2::request(url) |>
    httr2::req_url_path_append(
      paste0("services;key=", key, "/wfs/layer-", layer_id, "/")
    ) |>
    httr2::req_url_query(
      service = "WFS",
      request = "GetCapabilities"
    )
  
  # Get the final request URL string for sf::read_sf()
  request_url <- as.character(req$url)
  
  # Try reading as sf
  f <- tryCatch({
    sf::read_sf(request_url) |>
      sf::st_as_sf() |>
      sf::st_zm()
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(f)) {
    return(NULL)
  }
  
  # Check geometry types
  chk2 <- !any(sf::st_geometry_type(f) %in% c("CURVEPOLYGON", "COMPOUNDCURVE",
                                              "MULTISURFACE"))
  
  if (chk2) {
    return(f)
  } else {
    # Fix long column names (>10 chars)
    if (length(unique(nchar(names(f)))) > 1) {
      idx <- which(nchar(names(f)) > 10)
      names(f)[idx] <- substr(names(f)[idx], nchar(names(f)[idx]) - 15, nchar(names(f)[idx]))
      names(f) <- make.unique(names(f))
    }
    
    # Convert odd geometries to polygons
    tmpfile <- tempfile(fileext = ".shp")
    tmpfile2 <- tempfile(fileext = ".shp")
    
    sf::st_write(f, tmpfile, append = FALSE, quiet = TRUE)
    sf::gdal_utils(util = "vectortranslate", source = tmpfile, destination = tmpfile2)
    
    f2 <- sf::read_sf(tmpfile2)
    return(f2)
  }
}
