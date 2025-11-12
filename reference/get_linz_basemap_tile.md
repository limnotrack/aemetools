# Get LINZ basemap tile

Get map tiles from the LINZ basemap service. This requires an API key
which can be obtained from: https://basemaps.linz.govt.nz

## Usage

``` r
get_linz_basemap_tile(x, zoom = 16, key = NULL, verbose = FALSE)
```

## Arguments

- x:

  sf, sfc, bbox, SpatRaster, SpatVector or SpatExtent object. If `x` is
  a SpatExtent it must express coordinates in lon/lat WGS84 (epsg:4326).

- zoom:

  zoom level (see Details).

- key:

  character; LINZ API key. If NULL, will look for the key in the
  LINZ_KEY environment variable. If that is not set, will throw an
  error. Use the `add_linz_key` function to set the key.

  @importFrom maptiles create_provider get_tiles @importFrom terra
  subset

- verbose:

  if TRUE, tiles filepaths, zoom level and attribution are displayed.

## Value

A SpatRaster is returned.
