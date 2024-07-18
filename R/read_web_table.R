#' Read table data from online databases as sf object
#'
#' @param url character, URL of the web service
#' @inheritParams get_raster_layer_value
#'
#' @importFrom httr parse_url build_url
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

  url_req <- paste0(url, "/services;key=", key,
                    "/wfs/table-", layer_id, "/")
  url <- httr::parse_url(url_req)
  url$query <- list(service = "WFS",
                    #version = "2.0.0", # facultative
                    request = "GetCapabilities"
  )
  request <- httr::build_url(url)
  request
  chk <- tryCatch({
    f <- sf::read_sf(request)
    TRUE
  }, error = function(e) {
    FALSE
  })
  if (!chk) {
    return()
  } else {
    f <- f |>
      as.data.frame()
    return(f)
  }
}
