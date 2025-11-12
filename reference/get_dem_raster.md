# Get DEM raster from LINZ

Get DEM raster from LINZ

## Usage

``` r
get_dem_raster(x, lake = NULL, zoom = 14, verbose = FALSE, prompt = FALSE)
```

## Arguments

- x:

  sf object

- lake:

  sf object; of the lake shape. Optional. Default is NULL.

- zoom:

  integer; zoom level for the raster. Default is 14.

- verbose:

  if TRUE, tiles filepaths, zoom level and attribution are displayed.

- prompt:

  logical; whether to prompt the user to select the DEM layer. Default
  is FALSE. This can be useful if the user wants to cycle through the
  available DEM layers to find the best one.

## Value

SpatRaster object

## Details

This function will get the DEM raster from LINZ for the given shape.
