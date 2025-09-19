#' Request data from the LERNZMP API
#' 
#' Handles authentication, error checking, and returns the raw response.
#'
#' @param api_url character; base URL of the API.
#' @param endpoint character; specific API endpoint to call.
#' @param query list; named list of query parameters.
#' @param api_key character; API key for authentication. If NULL, will look for
#' the key in the LERNZMP_KEY environment variable. If that is not set, will 
#' throw an error.
#' @param headers list; additional headers to include in the request. Defaults 
#' to an empty list.
#'
#' @returns The raw response object from httr2; parsing is left to the caller.
#' @export
#' @importFrom httr2 request req_url_path_append req_url_query req_headers
#' @importFrom httr2 req_perform resp_status resp_body_string
#' @importFrom rlang `!!!`
 
api_request <- function(api_url, endpoint, query = list(), api_key = NULL, 
                        headers = list()) {
  if (is.null(api_key)) {
    api_key <- Sys.getenv("LERNZMP_KEY")
    if (api_key == "") {
      stop("API key is not set. Please provide it or set the 'LERNZMP_API' environment variable.")
    }
  }
  
  req <- httr2::request(api_url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_headers(`Api-Key` = api_key, !!!headers)
  
  res <- tryCatch(httr2::req_perform(req), error = function(e) return(NULL))
  
  if (!is.null(res) && httr2::resp_status(res) == 200) {
    return(res) # leave parsing up to the caller
  } else if (!is.null(res)) {
    msg <- httr2::resp_body_string(res)
    stop(sprintf("API request failed [%s]: %s", httr2::resp_status(res), msg))
  } else {
    stop("API request failed: request error or NULL response")
  }
}
