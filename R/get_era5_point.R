#' Get ERA5 data for a coordinate
#'
#' Extract [ERA5-Land](https://www.ecmwf.int/en/era5-land) meteorological data
#' for the closest grid to a particular latitude and longitude for a selected
#' number of years (1980-2023).
#'
#' @param lat numeric; Latitude
#' @param lon numeric; Longitude
#' @param years numeric; vector of years in numeric form to be extracted.
#' Currently years are limited to 1980-2022.
#' @param variables vector; with AEME meteorological variable names to be
#' downloaded. Defaults to all available variables: c("MET_tmpair",
#'  "MET_tmpdew", "MET_wnduvu", "MET_wnduvv", "MET_pprain", "MET_ppsnow",
#'  "MET_prsttn", "MET_radswd").
#' @param api_url character; URL to the API endpoint. Default is
#' "http://170.64.143.18:80"
#' @param api_key character; API key to access the data.
#'
#' @importFrom httr GET parse_url build_url content
#' @importFrom jsonlite fromJSON
#'
#' @return dataframe of daily ERA5 data.
#'
#' @export

get_era5_point <- function(lat, lon, years, variables = c("MET_tmpair",
                                                          "MET_tmpdew",
                                                          "MET_wnduvu",
                                                          "MET_wnduvv",
                                                          "MET_pprain",
                                                          "MET_ppsnow",
                                                          "MET_prsttn",
                                                          "MET_radswd"),
                           api_url = "http://170.64.143.18:80",
                           api_key = NULL) {

  url <- httr::parse_url(api_url)

  url$path <- "get_era5_data"

  url$query <- list(lat = lat, lon = lon, years = paste0(years, collapse = ","),
                    variables = paste(variables, collapse = ","),
                    key = api_key)


  url <- httr::build_url(url)

  response <- httr::GET(url)

  if (response$status_code == 200) {
    data <- httr::content(response, as = "text", encoding = "UTF-8")
    data <- jsonlite::fromJSON(data) |>
      dplyr::mutate(Date = as.Date(Date))
    return(data)
  } else {
    stop("Error: ", response$status_code)
  }
}
