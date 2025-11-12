# Get a raster tile from LINZ

Get a raster tile from LINZ

## Usage

``` r
get_raster_tile(x, layer_id, zoom = 15, key = NULL, verbose = FALSE)
```

## Arguments

- x:

  sf, sfc, bbox, SpatRaster, SpatVector or SpatExtent object. If `x` is
  a SpatExtent it must express coordinates in lon/lat WGS84 (epsg:4326).

- layer_id:

  numeric; layer ID value for the raster layer on the LINZ data service.
  See <https://data.linz.govt.nz/>

- zoom:

  numeric; zoom level for the tile. Default is 15.

- key:

  character; LINZ API key. If NULL, will look for the key in the
  LINZ_KEY environment variable. If that is not set, will throw an
  error. Use the `add_linz_key` function to set the key.

  @importFrom maptiles create_provider get_tiles @importFrom terra
  subset

- verbose:

  if TRUE, tiles filepaths, zoom level and attribution are displayed.

## Value

a raster object
