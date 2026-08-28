#' Get Depth Contour Data
#' 
#' Get depth contour data from the API and convert GeoJSON elements to sf objects.
#'
#' @inheritParams get_lake_shape
#'
#' @returns A sf object representing the depth contours of the lake. The sf object will have
#' columns for id, name_final, depth, and geometry. The depth column will contain the depth
#' values for each contour line.
#'  
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 resp_body_string
#' @importFrom sf st_set_crs
#' 
#' @export
#'
#' @examples
#' depth_contours <- get_depth_contours(id = 1, 
#' api_key = Sys.getenv("LERNZMP_KEY"))
#' names(depth_contours)

get_depth_contours <- function(id = 1, api_url = "https://api.limnotrack.com", 
                               api_key = NULL) {
  query <- list(id = id)
  
  res <- api_request(api_url = api_url, endpoint = "get_lake_shape", 
                     query = query,
                     api_key = api_key)
  lake <- res |> 
    httr2::resp_body_string() |> 
    geojsonsf::geojson_sf() |> 
    sf::st_cast("MULTILINESTRING") |> 
    sf::st_cast("LINESTRING")
  id <- lake$id[1]
  lakename <- lake$name_final[1]
  suppressWarnings({
    lake <- lake |> 
      sf::st_set_crs(2193) |> 
      dplyr::mutate(depth = 0) |> 
      dplyr::select(depth, geometry)
  })
    
  res <- api_request(api_url = api_url, endpoint = "get_contours",
                     query = query, api_key = api_key)
  
  # Parse JSON into a list
  # parsed <- jsonlite::fromJSON(httr2::resp_body_string(res))
  depth_contours <- res |> 
    httr2::resp_body_string() |> 
    geojsonsf::geojson_sf()
  suppressWarnings({
    depth_contours <- sf::st_set_crs(depth_contours, 2193)
  })
  
  all_contours <- dplyr::bind_rows(lake, depth_contours) |> 
    dplyr::mutate(
      id = id,
      name_final = lakename
    ) |> 
    dplyr::select(id, name_final, depth, geometry)
  return(all_contours)
}
