#' Read table data from online databases as sf object
#'
#' @param url character, URL of the web service
#' @inheritParams get_raster_layer_value
#'
#' @importFrom httr2 request req_url_path_append req_url_query
#' @importFrom httr2 req_perform resp_status resp_status_desc
#' @importFrom httr2 resp_body_raw req_headers req_perform
#' @importFrom sf read_sf
#'
#' @return data frame
#' @export
#'

read_web_table <- function(url, layer_id, key = NULL) {

  if (is.null(key)) {
    key <- Sys.getenv("LINZ_KEY")
    if (key == "") {
      stop("No LINZ API key found. See ?add_linz_key for more information.")
    }
  }
  
  # Build request with httr2
  req <- httr2::request(url) |>
    httr2::req_url_path_append(
      paste0("services;key=", key, "/wfs/table-", layer_id, "/")
    ) |>
    httr2::req_url_query(
      service = "WFS",
      request = "GetCapabilities"
    )
  
  # Perform request, catch network/transport errors
  res <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      warning("HTTP request failed: ", e$message)
      return(NULL)
    }
  )
  if (is.null(res)) return(NULL)
  
  # Check HTTP status
  status <- httr2::resp_status(res)
  if (status < 200 || status >= 300) {
    warning("Request failed: HTTP ", status, " - ", httr2::resp_status_desc(res))
    return(NULL)
  }
  
  # Write response body to a temporary file and let sf read it
  body_raw <- httr2::resp_body_raw(res)
  tmp <- tempfile(fileext = ".xml")
  writeBin(body_raw, tmp)
  
  sf_obj <- tryCatch({
    sf::read_sf(tmp)
  }, error = function(e) {
    warning("sf::read_sf() failed: ", e$message)
    return(NULL)
  })
  
  if (is.null(sf_obj)) return(NULL)
  
  return(as.data.frame(sf_obj))
}
