# Get ERA5 data from ISIMIP3a for a point location

Get ERA5 data from ISIMIP3a for a point location

## Usage

``` r
get_era5_isimip_point(
  lon,
  lat,
  years,
  vars = c("MET_tmpair", "MET_pprain", "MET_wndspd", "MET_radswd", "MET_prsttn",
    "MET_radlwd", "MET_humrel"),
  download_path = tempdir()
)
```

## Arguments

- lon:

  numeric; Longitude

- lat:

  numeric; Latitude

- years:

  numeric; vector of years in numeric form to be extracted. Currently
  years are limited to 1980-2024.

- vars:

  vector; with AEME meteorological variable names to be downloaded.
  Defaults to all available variables: c("MET_tmpair", "MET_tmpdew",
  "MET_wnduvu", "MET_wnduvv", "MET_pprain", "MET_ppsnow", "MET_prsttn",
  "MET_radswd").

- download_path:

  character; path to download the data. Default is the temporary
  directory.

## Value

A data frame with the requested variables

## Examples

``` r
if (FALSE) { # \dontrun{
lon <- 13.064332
lat <- 52.380551
years <- 2015:2021
vars <- c("MET_tmpair", "MET_pprain")
get_era5_isimip_point(lon, lat, years, vars)
} # }
```
