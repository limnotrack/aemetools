# Get layer IDs for a given spatial object

This function takes a spatial object and returns the layer IDs for the
aerial imagery and DEM layers that intersect with the spatial object.

## Usage

``` r
get_layer_ids(x, type = c("dem", "aerial"))
```

## Arguments

- x:

  object of class `sf`, `sfc` or `sfg`

- type:

  A character vector of length 1 or 2, specifying the type of layers to
  return. The default is c("dem", "aerial"). If only one type is
  specified, the function will only return layers of that type.

## Value

A data frame with the layer IDs of the layers that intersect with the
spatial object.
