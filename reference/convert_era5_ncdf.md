# Convert ERA5 netCDF files to AEME or LER

Convert ERA5 netCDF files to AEME or LakeEnsemblR formats.

## Usage

``` r
convert_era5_ncdf(
  lat,
  lon,
  variable = c("10m_u_component_of_wind", "10m_v_component_of_wind",
    "2m_dewpoint_temperature", "2m_temperature", "snowfall", "surface_pressure",
    "surface_solar_radiation_downwards", "surface_thermal_radiation_downwards",
    "total_precipitation"),
  year = 2022,
  site = "test",
  path = ".",
  format = "AEME"
)
```

## Arguments

- lat:

  numeric; latitude

- lon:

  numeric; longitude

- variable:

  string with ERA5 variable names e.g. "2m_temperature",
  "total_precipitation"

- year:

  numeric; vector with years

- site:

  string of site name which was used when downloading the data.

- path:

  filepath to where the downloaded ERA5 ncdf files are stored.

- format:

  string; Either "AEME" or "LER". Default is "AEME".
