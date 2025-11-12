# Get lake shape as sf object

This function retrieves the shape of a lake from the LimnoTrack API
using the provided Lernzmp ID. The shape is returned as an sf object.

## Usage

``` r
get_lake_shape(id, api_url = "https://api.limnotrack.com", api_key = NULL)
```

## Arguments

- id:

  character or numeric; the character Lernzmp ID (e.g. "LID 1") of the
  lake or the numeric FENZ ID (e.g. 1).

- api_url:

  character; base URL of the API.

- api_key:

  character; API key for authentication. If NULL, will look for the key
  in the LERNZMP_KEY environment variable. If that is not set, will
  throw an error.

## Value

an sf object representing the lake shape.

## Examples

``` r
lake <- get_lake_shape(id = 1, api_key = Sys.getenv("LERNZMP_KEY"))
```
