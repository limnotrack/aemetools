#' Get LINZ data as sf object
#'
#' @param url character, URL to LINZ API
#' @inheritParams get_raster_layer_value
#'
#' @importFrom httr parse_url build_url
#' @importFrom sf read_sf st_union st_cast st_as_sf st_zm st_write gdal_utils
#'
#' @return sf object
#' @export
#'

get_linz_sf <- function(url, shape = NULL, layer_id, key = NULL,
                        version = "2.0.0") {

  if (is.null(key)) {
    key <- Sys.getenv("LINZ_KEY")
    if (key == "") {
      stop("No LINZ API key found. See ?add_linz_key for more information.")
    }
  }

  shape <- mapedit::drawFeatures()



  # Using a bounding box to limit the request
  if (!is.null(shape)) {
    bbox <- shape |>
      sf::st_transform(4326) |>
      sf::st_bbox()
    cql_filter <- paste0("bbox(shape,", bbox["ymin"], ",", bbox["xmin"],
                         ",", bbox["ymax"], ",", bbox["xmax"], ")")
    request <- paste0(url, "/services;key=", key,
                      "/wfs?service=WFS&version=2.0.0&request=GetFeature&typeNames=layer-", layer_id, "&SRSName=EPSG:4326&cql_filter=", cql_filter)
  } else {
    url_req <- paste0(url, "/services;key=", key,
                      "/wfs/layer-", layer_id, "/")
    url <- httr::parse_url(url_req)
    url$query <- list(service = "wfs",
                      version = "2.0.0", # facultative
                      request = "GetCapabilities"
    )
    request <- httr::build_url(url)
  }

  # Get centroid of shape
  cent <- shape |>
    sf::st_transform(4326) |>
    sf::st_centroid() |>
    sf::st_coordinates() |>
    as.numeric()

  # LRIS
  url <- "https://lris.scinfo.org.nz"
  apikey <- "6a64f28728c945f99897fa3c86c1db85"
  layer <- "layer-104400"

  f <- get_wfs(x = shape, url = url, apikey = apikey, layer = layer)

  req <- build_wfs_req(x, url = url, apikey, "2.0.0", layer, spatial_filter,
                       ecql_filter, startindex = 0, crs)
  resp <- hit_api_wfs(req, ecql_filter, apikey)


  request <- paste0("https://lris.scinfo.org.nz/services/query/v1/vector.json?key=6a64f28728c945f99897fa3c86c1db85&layer=104400&x=", cent[1], "&y=", cent[2], "&max_results=3&radius=5000&geometry=true&with_field_names=true")
  request <- paste0("https://lris.scinfo.org.nz/services;key=6a64f28728c945f99897fa3c86c1db85/wfs?service=WFS&version=2.0.0&request=GetFeature&typeNames=layer-104400&count=3")
  request <- paste0("https://lris.scinfo.org.nz/services;key=6a64f28728c945f99897fa3c86c1db85/wfs?service=WFS&version=1.1.0&request=GetFeature&typeNames=layer-104400&cql_filter=", cql_filter)
  request <- paste0("https://lris.scinfo.org.nz/services;key=6a64f28728c945f99897fa3c86c1db85/wfs?service=WFS&version=1.1.0&request=GetFeature&typeNames=layer-104400&cql_filter=", cql_filter)

  # Linz API
  request <- paste0("https://data.linz.govt.nz/services;key=", key,
                    "/wfs?service=WFS&version=2.0.0&request=GetFeature&typeNames=layer-231&count=3")

  request
  request <- paste0("https://data.linz.govt.nz/services;key=ea8fa2e2575d4f3598922016f7d162f0/wfs/layer-50284/?service=WFS&version=2.0.0&request=GetFeature&cql_filter=", cql_filter)
  request <- paste0("https://data.linz.govt.nz/services;key=ea8fa2e2575d4f3598922016f7d162f0/wfs?service=WFS&version=2.0.0&request=GetFeature&typeNames=layer-50787&cql_filter=", cql_filter)
  request <- "https://data.linz.govt.nz/services;key=ea8fa2e2575d4f3598922016f7d162f0/wfs?SERVICE=WFS&VERSION=2.0.0&REQUEST=GetCapabilities&"
  request <- paste0("https://data.linz.govt.nz/services;key=ea8fa2e2575d4f3598922016f7d162f0/wfs?service=WFS&version=2.0.0&request=GetFeature&typeNames=layer-50772&cql_filter=", cql_filter)
  request <- paste0("https://data.linz.govt.nz/services;key=ea8fa2e2575d4f3598922016f7d162f0/wfs?service=WFS&version=2.0.0&request=GetFeature&typeNames=layer-50772&cql_filter=", cql_filter)

  url <- "https://lris.scinfo.org.nz"
  layer_id <- 50772
  version = "1.1.0"
  request <- paste0(url, "/services;key=", key,
                    "/wfs?service=WFS&version=", version,
                    "&request=GetFeature&typeNames=layer-", layer_id,
                    "&cql_filter=", cql_filter)

  res <- httr::GET(request)
  res <- geojson_read(request, what = "geojson")
  res$status_code

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

  if (chk2) {

    if (!is.null(shape)) {
      sf::st_crs(f) <- 4326
      f <- sf::st_transform(f, sf::st_crs(shape))
    }



    return(f)
  } else {
    # Check names of geometry column and if any have >10 characters
    # to truncate and ensure they are unique
    if (length(unique(nchar(names(f)))) > 1) {
      idx <- which(nchar(names(f)) > 10)
      # truncate the columns names using the last 10 characters
      names(f)[idx] <- substr(names(f)[idx], nchar(names(f)[idx]) - 15, nchar(names(f)[idx]))
      # ensure the names are unique
      names(f) <- make.unique(names(f))
      # names(f) <- substr(names(f), 1, 10)
    }

    # Create temporary files for writing and removing odd geometry types
    tmpfile <- tempfile(fileext = ".shp")
    tmpfile2 <- tempfile(fileext = ".shp")

    sf::st_write(f, tmpfile, append = FALSE)

    # Convert CURVEPOLYGON -> POLYGON
    sf::gdal_utils(util = "vectortranslate", source = tmpfile,
                   destination = tmpfile2)

    f2 <- sf::read_sf(tmpfile2)

    library(leaflet)
    leaflet() |>
      addWMSTiles(baseUrl = "https://basemaps.linz.govt.nz/v1/tiles/aerial/WebMercatorQuad/{z}/{x}/{y}.webp?api=c01hbexn54aeskebvrbthr8e2vf", layers = "Basemap", group = "basemap", options = list(maxZoom = 18)) |>
      # addCircleMarkers(data = f, group = "linz", color = "red", radius = 1, stroke = FALSE, fillOpacity = 1) |>
      addPolygons(data = shape, group = "shape", color = "blue", fillOpacity = 0.1) |>
      addPolygons(data = f2, group = "linz", color = "red", fillOpacity = 0.1)

    return(f2)
  }
}
