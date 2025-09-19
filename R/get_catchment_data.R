#' Get Catchment Data
#' 
#' Get catchment data from the API and convert GeoJSON elements to sf objects.
#'
#' @inheritParams get_lake_shape
#'
#' @returns A list of sf objects representing the catchment data.
#' Each element in the list corresponds to a different catchment feature. These
#' are: catchment boundary ("catchment"), streams ("reaches"), lakes ("lakes), 
#' subcatchments ("subcatchments"), and land cover from the Land Cover DataBase
#'  ("lcdb").
#'  
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 resp_body_string
#' @importFrom geojsonsf geojson_sf
#' @importFrom sf st_set_crs
#' 
#' @export
#'
#' @examples
#' catchment <- get_catchment_data(id = 3, 
#' api_key = Sys.getenv("LERNZMP_KEY"))
#' names(catchment)

get_catchment_data <- function(api_url = "https://api.limnotrack.com", 
                               id = 3, api_key = NULL) {
  query <- list()
  for (x in id) query <- c(query, list(id = x))
  
  res <- api_request(api_url = api_url, endpoint = "get_catchment_data",
                     query = query, api_key = api_key)
  
  # Parse JSON into a list
  parsed <- jsonlite::fromJSON(httr2::resp_body_string(res))
  
  # Convert each GeoJSON element back to sf
  catchment_list <- lapply(parsed, function(geo) {
    sf_obj <- geojsonsf::geojson_sf(geo)
    suppressWarnings({
      sf::st_set_crs(sf_obj, 2193)
    })
  })
  
  return(catchment_list)
}
