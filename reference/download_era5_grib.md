# Download ERA5 GRIB files

Download ERA5 meteorological data from the Copernicus Data Store (CDS).
Create a free CDS user account by [self
registering](https://cds.climate.copernicus.eu/user/register). Once your
user account has been verified you can get your personal user ID and key
by visiting the [user profile](https://cds.climate.copernicus.eu/user).
Further information can be found on the
[ecmwfr](https://bluegreen-labs.github.io/ecmwfr/#use) package page.

## Usage

``` r
download_era5_grib(
  shape = NULL,
  lat,
  lon,
  buffer = 0.1,
  variable = c("10m_u_component_of_wind", "10m_v_component_of_wind",
    "2m_dewpoint_temperature", "2m_temperature", "snowfall", "surface_pressure",
    "surface_solar_radiation_downwards", "surface_thermal_radiation_downwards",
    "total_precipitation"),
  year = 2022,
  month = 1:2,
  site = "test",
  user = NULL,
  era5_dataset = "reanalysis-era5-land",
  path = "."
)
```

## Arguments

- shape:

  sf object; shapefile or spatial object to download data for.

- lat:

  numeric; latitude

- lon:

  numeric; longitude

- buffer:

  numeric; buffer around the point in degrees. Default is 0.1.

- variable:

  vector of ERA5 variable names e.g. "2m_temperature",
  "total_precipitation". Default variables are:
  "10m_u_component_of_wind", "10m_v_component_of_wind",
  "2m_dewpoint_temperature", "2m_temperature", "snowfall",
  "surface_pressure", "surface_solar_radiation_downwards",
  "surface_thermal_radiation_downwards", "total_precipitation".

- year:

  numeric; year or vector of years.

- month:

  numeric; month or vector of months. Defaults to 1:12.

- site:

  string of site name which will be appended to the file

- user:

  user (email address) used to sign up for the ECMWF data service, if
  only a single user is needed it defaults to ("ecmwfr").

- era5_dataset:

  string; of which ERA5 dataset to use. Can be
  'reanalysis-era5-single-levels' or 'reanalysis-era5-land'

- path:

  filepath to store downloaded file

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
files
} # }
```
