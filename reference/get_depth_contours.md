# Get Depth Contour Data

Get depth contour data from the API and convert GeoJSON elements to sf
objects.

## Usage

``` r
get_depth_contours(
  id = 1,
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

A sf object representing the depth contours of the lake. The sf object
will have columns for id, name_final, depth, and geometry. The depth
column will contain the depth values for each contour line.

## Examples

``` r
depth_contours <- get_depth_contours(id = 1, 
api_key = Sys.getenv("LERNZMP_KEY"))
#> Warning: repeating attributes for all sub-geometries for which they may not be constant
names(depth_contours)
#> [1] "id"         "name_final" "depth"      "geometry"  
```
