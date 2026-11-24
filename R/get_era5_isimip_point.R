#' Get ERA5 data from ISIMIP3a for a point location
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `get_era5_isimip_point()` is superseded by
#' \code{metscale::download_era5_isimip_point()}, which this function now
#' calls. ERA5 extraction, conversion and bias-correction have moved to the
#' \href{https://github.com/limnotrack/metscale}{metscale} package; install it
#' with `remotes::install_github("limnotrack/metscale")`.
#'
#' @inheritParams get_era5_land_point_nz
#' @param download_path character; path to download the data. Default is
#' the temporary directory.
#'
#' @importFrom lifecycle deprecate_soft
#'
#' @returns A data frame with the requested variables
#' @export
#'
#' @examples
#' \dontrun{
#' lon <- 13.064332
#' lat <- 52.380551
#' years <- 2015:2021
#' vars <- c("MET_tmpair", "MET_pprain")
#' get_era5_isimip_point(lon, lat, years, vars)
#' }

get_era5_isimip_point <- function(lon, lat, years,
                                  vars = c("MET_tmpair", "MET_pprain",
                                           "MET_wndspd", "MET_radswd",
                                           "MET_prsttn", "MET_radlwd",
                                           "MET_humrel"),
                                  download_path = tempdir()) {
  lifecycle::deprecate_soft("0.3.0.9000", "get_era5_isimip_point()",
                            "metscale::download_era5_isimip_point()")
  rlang::check_installed("metscale", version = "0.1.0",
                         reason = "to download ERA5-ISIMIP3a data.")

  metscale::download_era5_isimip_point(lon = lon, lat = lat, years = years,
                                       vars = vars,
                                       download_path = download_path)
}
