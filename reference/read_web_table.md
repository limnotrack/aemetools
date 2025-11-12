# Read table data from online databases as sf object

Read table data from online databases as sf object

## Usage

``` r
read_web_table(url, layer_id, key = NULL)
```

## Arguments

- url:

  character, URL of the web service

- layer_id:

  numeric; layer ID value for the raster layer on the LINZ data service.
  See <https://data.linz.govt.nz/>

- key:

  character; LINZ API key. This can be set as an environment variable
  using the
  [`add_linz_key()`](https://limnotrack.github.io/aemetools/reference/add_linz_key.md)
  function or passed as a character. See
  [`?add_linz_key`](https://limnotrack.github.io/aemetools/reference/add_linz_key.md)
  for more information with setting up the API key.

## Value

data frame
