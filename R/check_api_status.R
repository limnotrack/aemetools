#' Check API status
#' 
#' This function checks the availability of the LimnoTrack API by sending a request
#' to the health endpoint. It returns TRUE if the API is available (HTTP 200),
#' FALSE otherwise, and provides informative messages about the status.
#'
#' @inheritParams api_request
#' 
#' @importFrom httr2 request req_url_path_append req_perform resp_status 
#' resp_body_string
#'
#' @returns TRUE if the API is available, FALSE otherwise.
#' @export
#'
#' @examples
#' check_api_status()

check_api_status <- function(api_url = "https://api.limnotrack.com") {

  req <- httr2::request(api_url) |>
    httr2::req_url_path_append("health") 
  
  res <- tryCatch(httr2::req_perform(req), error = function(e) return(NULL))
  
  if (!is.null(res) && httr2::resp_status(res) == 200) {
    parsed <- httr2::resp_body_string(res) |> 
      jsonlite::fromJSON()
    
    if (parsed$api) {
      message("✅ API is available")
    }
    if (parsed$db) {
      message("✅ Database connection is healthy")
    } else {
      warning("⚠️ Database connection issue")
    }
    return(TRUE)
  } else if (!is.null(res)) {
    warning(sprintf("⚠️ API request failed [%s]: %s",
                                httr2::resp_status(res),
                                httr2::resp_body_string(res)))
    return(FALSE)
  } else {
    warning("❌ API request error or NULL response")
    return(FALSE)
  }
}
