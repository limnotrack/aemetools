#' Read spatial feature (sf) data from online databases as sf object
#'
#' @param url character, URL of the web feature service (WFS) endpoint.
#' @inheritParams get_raster_layer_value
#' @param layer_id integer, layer ID of the spatial feature data to retrieve.
#' @param filter_col character, name of the column to filter on. If NULL, no
#' filtering is applied. Default is NULL.
#' @param filter_val character vector, values to filter the specified column
#' on. If NULL, no filtering is applied. Default is NULL.
#'
#' @importFrom httr2 request req_url_path_append req_url_query req_perform
#' @importFrom httr2 resp_status resp_status_desc
#' @importFrom xml2 read_xml xml_find_all xml_find_first xml_text xml_attr
#' @importFrom xml2 xml_ns
#' @importFrom rlang `%||%`
#' @importFrom sf read_sf st_union st_cast st_as_sf st_zm st_write gdal_utils
#'
#' @return sf object
#' @export
#'

read_web_sf <- function(url, layer_id, key = NULL, filter_col = NULL,
                        filter_val = NULL) {

  if (is.null(key)) {
    key <- Sys.getenv("LINZ_KEY")
    if (key == "") {
      stop("No LINZ API key found. See ?add_linz_key for more information.")
    }
  }
  
  if (!is.numeric(layer_id) || length(layer_id) != 1) {
    stop("`layer_id` must be a single numeric value.")
  }
  
  if (!is.null(filter_col)) {
    if (!is.character(filter_col)) {
      stop("`filter_col` must be a character or NULL.")
    }
    if (is.null(filter_val) || length(filter_val) == 0) {
      stop("`filter_val` must be provided when `filter_col` is specified.")
    }
    if (!is.character(filter_val)) {
      stop("`filter_val` must be a character vector.")
    }
  }
  
  # Get capabilities document to determine version and default CRS
  
  req <- httr2::request(url) |>
    httr2::req_url_path_append(
      paste0("services;key=", key, "/wfs/layer-", layer_id, "/")
    ) |>
    httr2::req_url_query(
      service = "WFS",
      request = "GetCapabilities"
    )
  
  cap_url <- as.character(req$url)
  
  # read capabilities as XML
  cap_doc <- xml2::read_xml(cap_url)
  # xml2::write_xml(cap_doc, "capabilities.xml")
  
  # extract advertised versions
  version <- xml2::xml_attr(
    xml2::xml_find_all(cap_doc, ".//ows:ServiceTypeVersion", 
                       xml2::xml_ns(cap_doc)),
    "value"
  )
  if (version == "" | is.na(version)) {
    version <- xml2::xml_attr(cap_doc, "version")  # should return "2.0.0"
  }
  if (version == "") {
    stop("Could not determine WFS version from GetCapabilities document")
  }
  
  # xml2::xml_attr(xml2::xml_find_first(cap_doc, ".//gml:Polygon"), "srsName")
  
  # Build query
  parsed <- httr2::url_parse(url)
  layer_name <- paste0(parsed$hostname, ":layer-", layer_id)
  
  # Use the XML namespace (WFS 2.0 uses ows + wfs)
  ns <- xml2::xml_ns(cap_doc)
  
  # Find the FeatureType node with that Name
  ft_node <- xml2::xml_find_first(cap_doc, 
                                  paste0(".//wfs:FeatureType[wfs:Name='",
                                         layer_name, "']"), ns)
  
  # Extract DefaultCRS
  default_crs <- xml2::xml_text(xml2::xml_find_first(ft_node, "wfs:DefaultCRS",
                                                     ns))
  epsg_code <- as.numeric(sub(".*EPSG::(\\d+)$", "\\1", default_crs))
  
  req <- httr2::request(url) |>
    httr2::req_url_path_append(
      paste0("services;key=", key, "/wfs/layer-", layer_id, "/")
    ) |>
    httr2::req_url_query(
      service = "WFS",
      version = version,
      request = "GetFeature",
      typeNames = layer_name,
    )
  
  if (!is.null(filter_col) && length(filter_val) > 0) {
    if (length(filter_val) == 1) {
      filter_str <- paste0(filter_col, " = '", filter_val, "'")
    } else {
      filter_str <- sprintf(
        "%s IN (%s)",
        filter_col,
        paste0("'", filter_val, "'", collapse = ",")
      )
    }
    req <- httr2::req_url_query(req, CQL_FILTER = filter_str)
  }
  
  
  request_url <- as.character(req$url)
  
  sf_obj <- tryCatch({
    sf::read_sf(request_url) |>
      sf::st_as_sf() |>
      sf::st_zm()
  }, error = function(e) {
    message("Failed to fetch features: ", e$message)
    NULL
  })
  
  if (is.null(sf_obj)) {
    message("No features returned from the WFS request.")
    return(NULL)
  }
  if (is.na(sf::st_crs(sf_obj))) {
    sf::st_crs(sf_obj) <- epsg_code
  }
  
  # Check geometry types
  geom_types <- sf::st_geometry_type(sf_obj)
  # Check geometry types
  chk <- !any(geom_types %in% c("CURVEPOLYGON", "COMPOUNDCURVE", 
                                "MULTISURFACE"))
  
  if (chk) {
    return(sf_obj)
  } else {
    orig_names <- names(sf_obj)
    # Get current geometry column name
    geom_col <- attr(sf_obj, "sf_column")
    orig_names <- orig_names[orig_names != geom_col]
    # Fix long column names (>10 chars)
    if (length(unique(nchar(names(sf_obj)))) > 1) {
      idx <- which(nchar(names(sf_obj)) > 10)
      names(sf_obj)[idx] <- substr(names(sf_obj)[idx], 
                                   nchar(names(sf_obj)[idx]) - 15,
                                   nchar(names(sf_obj)[idx]))
      names(sf_obj) <- make.unique(names(sf_obj))
    }
    
    # Convert odd geometries to polygons
    tmpfile <- tempfile(fileext = ".shp")
    tmpfile2 <- tempfile(fileext = ".shp")
    
    sf::st_write(sf_obj, tmpfile, append = FALSE, quiet = TRUE)
    sf::gdal_utils(util = "vectortranslate", source = tmpfile, 
                   destination = tmpfile2)
    
    sf_obj2 <- sf::read_sf(tmpfile2)
    new_names <- names(sf_obj2)
    geom_col2 <- attr(sf_obj2, "sf_column")
    new_names <- new_names[new_names != geom_col2]
    if (any(!new_names %in% orig_names)) {
      upd_names <- new_names
      names(upd_names) <- orig_names
      names(sf_obj2) <- sapply(names(sf_obj2), \(x) {
        if (x == geom_col2) return(x)
        names(upd_names[upd_names == x])
      })
    }
    return(sf_obj2)
  }
}
