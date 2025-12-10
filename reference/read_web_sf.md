# Read spatial feature (sf) data from online databases as sf object

Read spatial feature (sf) data from online databases as sf object

## Usage

``` r
read_web_sf(url, layer_id, key = NULL, filter_col = NULL, filter_val = NULL)
```

## Arguments

- url:

  character, URL of the web feature service (WFS) endpoint.

- layer_id:

  integer, layer ID of the spatial feature data to retrieve.

- key:

  character; LINZ API key. This can be set as an environment variable
  using the [`add_linz_key()`](add_linz_key.md) function or passed as a
  character. See [`?add_linz_key`](add_linz_key.md) for more information
  with setting up the API key.

- filter_col:

  character, name of the column to filter on. If NULL, no filtering is
  applied. Default is NULL.

- filter_val:

  character vector, values to filter the specified column on. If NULL,
  no filtering is applied. Default is NULL.

## Value

sf object
