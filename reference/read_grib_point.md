# Read a point from a grib file

Read a point from a grib file

## Usage

``` r
read_grib_point(file, shape = NULL, lat, lon, method = "bilinear")
```

## Arguments

- file:

  character; path to the grib file. Can be a vector of paths.

- shape:

  sf object; shapefile or spatial object to download data for.

- lat:

  numeric; latitude

- lon:

  numeric; longitude

- method:

  character. method for extracting values with points ("simple" or
  "bilinear"). With "simple" values for the cell a point falls in are
  returned. With "bilinear" the returned values are interpolated from
  the values of the four nearest raster cells

## Value

A data frame with the extracted data.

## Examples

``` r
if (FALSE) { # \dontrun{
lat <- -38.07782
lon <- 176.2673
year <- 2024
month <- 1:2
variable <- "2m_temperature"
path <- "data/test"
ecmwfr::wf_set_key(key = "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX",
                   user = "person@email.com")
files <- download_era5_grib(lat = lat, lon = lon, year = year, month = month,
                            variable = variable, path = path, user = user)
df <- read_grib_point(file = files, lat = lat, lon = lon)
} # }
```
