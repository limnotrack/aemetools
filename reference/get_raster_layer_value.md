# Get raster layer value for a given latitude and longitude

Get raster layer value for a given latitude and longitude

## Usage

``` r
get_raster_layer_value(lat, lon, layer_id, key = NULL)
```

## Arguments

- lat:

  numeric; Latitude

- lon:

  numeric; Longitude

- layer_id:

  numeric; layer ID value for the raster layer on the LINZ data service.
  See <https://data.linz.govt.nz/>

- key:

  character; LINZ API key. This can be set as an environment variable
  using the [`add_linz_key()`](add_linz_key.md) function or passed as a
  character. See [`?add_linz_key`](add_linz_key.md) for more information
  with setting up the API key.

## Value

numeric value or NA if outside extent
