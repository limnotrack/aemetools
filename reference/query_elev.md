# Query elevation data for a shape within a DEM raster

This function queries the elevation data for a shape within a DEM
raster. If the latitude and longitude are not provided, the function
will use the centroid of the shape. If the elevation data is not found
for the first point, the function will try other points on the shape.

## Usage

``` r
query_elev(x, dem, lat, lon, layer_id = NULL)
```

## Arguments

- x:

  sf object

- dem:

  SpatRaster object

- lat:

  numeric; latitude to query

- lon:

  numeric; longitude to query

- layer_id:

  integer; layer id of the DEM raster

## Value

numeric; elevation value
