#' Get lake shape as sf object
#' 
#' This function retrieves the shape of a lake from the LimnoTrack API
#' using the provided Lernzmp ID. The shape is returned as an sf object.
#'
#' @inheritParams api_request
#' @param id character or numeric; the character Lernzmp ID (e.g. "LID 1") of 
#' the lake or the numeric FENZ ID (e.g. 1).
#'
#' @returns an sf object representing the lake shape.
#' @export
#' 
#' @importFrom httr2 resp_body_string
#' @importFrom geojsonsf geojson_sf
#' @importFrom sf st_set_crs
#'
#' @examples
#' lake <- get_lake_shape(id = 1, api_key = Sys.getenv("LERNZMP_KEY"))

get_lake_shape <- function(id, api_url = "https://api.limnotrack.com",  
                           api_key = NULL) {
  query <- list()
  for (x in id) query <- c(query, list(id = x))
  
  res <- api_request(api_url = api_url, endpoint = "get_lake_shape", 
                     query = query,
                     api_key = api_key)
  lake <- res |> 
    httr2::resp_body_string() |> 
    geojsonsf::geojson_sf()
  suppressWarnings({
    lake <- sf::st_set_crs(lake, 2193)
  })
  return(lake)  
}
