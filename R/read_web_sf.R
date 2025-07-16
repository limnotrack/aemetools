#' Read spatial feature (sf) data from online databases as sf object
#'
#' @param url character, URL of the web service
#' @inheritParams get_raster_layer_value
#'
#' @importFrom httr parse_url build_url
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

  url_req <- paste0(url, "/services;key=", key,
                    "/wfs/layer-", layer_id, "/")
  url <- httr::parse_url(url_req)
  url$query <- list(service = "WFS",
                    #version = "2.0.0", # facultative
                    request = "GetCapabilities"
  )
  request <- httr::build_url(url)
  request
  # print(request)
  chk <- tryCatch({
    f <- sf::read_sf(request) |>
      sf::st_as_sf() |>
      sf::st_zm()
    TRUE
  }, error = function(e) {
    FALSE
  })
  if (!chk) {
    return()
  }

  # Check 2 - do we need to transform the object from curve geometries?
  chk2 <- tryCatch({
    sf::st_union(f) |>
      sf::st_cast("POLYGON") |>
      sf::st_as_sf()
    TRUE
  }, error = function(e) {
    FALSE
  })

  chk2 <- !any(sf::st_geometry_type(f) %in% c("CURVEPOLYGON", "COMPOUNDCURVE",
                                              "MULTISURFACE"))

  if (chk2) {
    return(f)
  } else {
    # Check names of geometry column and if any have >10 characters
    # to truncate and ensure they are unique
    if (length(unique(nchar(names(f)))) > 1) {
      idx <- which(nchar(names(f)) > 10)
      # truncate the columns names using the last 10 characters
      names(f)[idx] <- substr(names(f)[idx], nchar(names(f)[idx]) - 15,
                              nchar(names(f)[idx]))
      # ensure the names are unique
      names(f) <- make.unique(names(f))
      # names(f) <- substr(names(f), 1, 10)
    }

    # Create temporary files for writing and removing odd geometry types
    tmpfile <- tempfile(fileext = ".shp")
    tmpfile2 <- tempfile(fileext = ".shp")

    sf::st_write(f, tmpfile, append = FALSE, quiet = TRUE)

    # Convert CURVEPOLYGON -> POLYGON
    sf::gdal_utils(util = "vectortranslate", source = tmpfile,
                   destination = tmpfile2)

    f2 <- sf::read_sf(tmpfile2)
    return(f2)
  }
}
