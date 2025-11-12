# Get Catchment Data

Get catchment data from the API and convert GeoJSON elements to sf
objects.

## Usage

``` r
get_catchment_data(
  id = 3,
  api_url = "https://api.limnotrack.com",
  api_key = NULL
)
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

A list of sf objects representing the catchment data. Each element in
the list corresponds to a different catchment feature. These are:
catchment boundary ("catchment"), streams ("reaches"), lakes ("lakes),
subcatchments ("subcatchments"), and land cover from the Land Cover
DataBase ("lcdb").

## Examples

``` r
catchment <- get_catchment_data(id = 3, 
api_key = Sys.getenv("LERNZMP_KEY"))
names(catchment)
#> [1] "catchment"     "reaches"       "lakes"         "subcatchments"
#> [5] "lcdb"         
```
