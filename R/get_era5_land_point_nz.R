#' Get ERA5 data for a coordinate
#'
#' Extract [ERA5-Land](https://www.ecmwf.int/en/era5-land) meteorological data
#' for the closest grid to a particular latitude and longitude for a selected
#' number of years (1980-2023).
#'
#' @param lat numeric; Latitude
#' @param lon numeric; Longitude
#' @param years numeric; vector of years in numeric form to be extracted.
#' Currently years are limited to 1980-2024.
#' @param vars vector; with AEME meteorological variable names to be
#' downloaded. Defaults to all available variables: c("MET_tmpair",
#'  "MET_tmpdew", "MET_wnduvu", "MET_wnduvv", "MET_pprain", "MET_ppsnow",
#'  "MET_prsttn", "MET_radswd").
#' @param api_url character; URL to the API endpoint. Default is
#' "http://170.64.143.18:80"
#' @param api_key character; API key to access the data. To get an API key,
#' please contact the package maintainer. The API key can also be set as an
#' environment variable using \code{Sys.setenv(LERNZMP_KEY = "your_api_key")}.
#'
#' @importFrom httr2 request req_url_path_append req_url_query
#' @importFrom httr2 req_headers req_perform resp_status resp_status_desc
#' @importFrom httr2 resp_body_string
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate
#' @importFrom rlang `%||%`
#'
#' @return dataframe of daily ERA5 data.
#'
#' @export

get_era5_land_point_nz <- function(lat, lon, years,
                                   vars = c("MET_tmpair",
                                            "MET_tmpdew",
                                            "MET_wnduvu",
                                            "MET_wnduvv",
                                            "MET_pprain",
                                            "MET_ppsnow",
                                            "MET_prsttn",
                                            "MET_radswd"),
                                   api_url = "http://170.64.143.18:8000",
                                   api_key = NULL) {
  if (is.null(api_key)) {
    api_key <- Sys.getenv("LERNZMP_KEY")
  }
  if (api_key == "") {
    stop("No API key found. Please set the LERNZMP_KEY environment variable.")
  }
  
  # Build request
  req <- httr2::request(api_url) |>
    httr2::req_url_path_append("get_era5_data") |>
    httr2::req_url_query(
      lat = lat,
      lon = lon,
      years = paste0(years, collapse = ","),
      variables = paste(vars, collapse = ",")
    ) |>
    httr2::req_headers("Api-Key" = api_key)
    
  
  # Perform request safely
  res <- tryCatch(
    httr2::req_perform(req),
    error = function(e) return(e$message)
  )
  
  # Handle network errors
  if (is.character(res)) {
    stop("Request failed: ", res)
  }
  
  # Handle API status codes
  status <- httr2::resp_status(res)
  if (status != 200) {
    stop("API returned status ", status, ": ", httr2::resp_status_desc(res))
  }
  
  # Parse content
  content <- httr2::resp_body_string(res)
  data <- jsonlite::fromJSON(content)
  
  if (!is.data.frame(data)) {
    stop("API error: ", data$error %||% "Unexpected response structure")
  }
  
  data <- dplyr::mutate(data, Date = as.Date(Date))
  return(data)
}
