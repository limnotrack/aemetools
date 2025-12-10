# Get DEM value for a given latitude and longitude

This function uses the LINZ data service to obtain the elevation value
for a given latitude and longitude. The function
[`get_raster_layer_value()`](get_raster_layer_value.md) is used to
obtain the value for the DEM layer (ID: 51768).

## Usage

``` r
get_dem_value(lat, lon, key = NULL)
```

## Arguments

- lat:

  numeric; Latitude

- lon:

  numeric; Longitude

- key:

  character; LINZ API key. This can be set as an environment variable
  using the [`add_linz_key()`](add_linz_key.md) function or passed as a
  character. See [`?add_linz_key`](add_linz_key.md) for more information
  with setting up the API key.

## Value

numeric value or NA if outside extent
