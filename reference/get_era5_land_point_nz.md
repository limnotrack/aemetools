# Get ERA5 data for a coordinate

Extract [ERA5-Land](https://www.ecmwf.int/en/era5-land) meteorological
data for the closest grid to a particular latitude and longitude for a
selected number of years (1980-2023).

## Usage

``` r
get_era5_land_point_nz(
  lat,
  lon,
  years,
  vars = c("MET_tmpair", "MET_tmpdew", "MET_wnduvu", "MET_wnduvv", "MET_pprain",
    "MET_ppsnow", "MET_prsttn", "MET_radswd"),
  api_url = "https://api.limnotrack.com",
  api_key = NULL
)
```

## Arguments

- lat:

  numeric; Latitude

- lon:

  numeric; Longitude

- years:

  numeric; vector of years in numeric form to be extracted. Currently
  years are limited to 1980-2024.

- vars:

  vector; with AEME meteorological variable names to be downloaded.
  Defaults to all available variables: c("MET_tmpair", "MET_tmpdew",
  "MET_wnduvu", "MET_wnduvv", "MET_pprain", "MET_ppsnow", "MET_prsttn",
  "MET_radswd").

- api_url:

  character; URL to the API endpoint. Default is
  "http://170.64.143.18:80"

- api_key:

  character; API key to access the data. To get an API key, please
  contact the package maintainer. The API key can also be set as an
  environment variable using `Sys.setenv(LERNZMP_KEY = "your_api_key")`.

## Value

dataframe of daily ERA5 data.
