#' Get Aeme object from LimnoTrack API
#' 
#' This function retrieves an Aeme object from the LimnoTrack API using the provided
#' lake ID. The Aeme object is returned as an R object.
#'
#' @inheritParams get_lake_shape
#'
#' @returns An Aeme object.
#' @export
#' 
#' @importFrom httr2 resp_body_raw
#'
#' @examples
#' aeme <- get_aeme(id = "LID1", api_key = Sys.getenv("LERNZMP_KEY"))
#' aeme

get_aeme <- function(id = "LID1", api_url = "https://api.limnotrack.com", 
                     api_key = NULL) {
  res <- api_request(
    api_url = api_url,
    endpoint = "get_aeme",
    query = list(id = id),
    api_key = api_key,
    headers = list(accept = "application/rds")
  )
  
  raw_resp <- httr2::resp_body_raw(res)
  aeme <- readRDS(rawConnection(raw_resp))
  return(aeme)
}
