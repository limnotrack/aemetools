# Access geospatial data

``` r

library(aemetools)
library(sf) # Used for spatial data
library(terra) # Used for raster data
library(tmap) # Used for spatial plotting

# tmap settings
tmap_mode("view")
tmap_options(basemap.server = "Esri.WorldImagery", raster.max_cells = 1e8)
```

This vignette demonstrates how to access geospatial data from various
sources including the [Land Information New Zealand (LINZ) Data
Service](https://data.linz.govt.nz/) and [Stats NZ Tatauranga Aotearoa
(New Zealand’s official data
agency)](https://datafinder.stats.govt.nz/).

## Access shapefile data from Stats NZ Tatauranga Aotearoa

The Stats NZ Tatauranga Aotearoa website provides access to a range of
geospatial data including shapefiles. The `read_web_sf` function can be
used to access this data.

Here we will access the [regional boundaries shape
file](https://datafinder.stats.govt.nz/layer/111182-regional-council-2023-generalised/)
from Stats NZ.

``` r

# Read the shapefile data from Stats NZ
url <- "https://datafinder.stats.govt.nz/" # Base URL
layer_id <- 111182 # Layer ID for the regional shapefile
key <- Sys.getenv("STATS_NZ_KEY") # Stats NZ API key

# Read the shapefile data
nz_shapefile <- read_web_sf(url = url, layer_id = layer_id, key = key)
nz_shapefile
#> Simple feature collection with 17 features and 8 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 1067061 ymin: 4701317 xmax: 2523320 ymax: 6242140
#> Projected CRS: NZGD2000 / New Zealand Transverse Mercator 2000
#> # A tibble: 17 × 9
#>    id              REGC2023_V1_00 REGC2023_V1_00_NAME     REGC2023_V1_00_NAME_…¹
#>  * <chr>           <chr>          <chr>                   <chr>                 
#>  1 layer-111182.1  01             Northland Region        Northland Region      
#>  2 layer-111182.2  02             Auckland Region         Auckland Region       
#>  3 layer-111182.3  03             Waikato Region          Waikato Region        
#>  4 layer-111182.4  04             Bay of Plenty Region    Bay of Plenty Region  
#>  5 layer-111182.5  05             Gisborne Region         Gisborne Region       
#>  6 layer-111182.6  06             Hawke's Bay Region      Hawke's Bay Region    
#>  7 layer-111182.7  07             Taranaki Region         Taranaki Region       
#>  8 layer-111182.8  08             Manawatū-Whanganui Reg… Manawatu-Whanganui Re…
#>  9 layer-111182.9  09             Wellington Region       Wellington Region     
#> 10 layer-111182.10 12             West Coast Region       West Coast Region     
#> 11 layer-111182.11 13             Canterbury Region       Canterbury Region     
#> 12 layer-111182.12 14             Otago Region            Otago Region          
#> 13 layer-111182.13 15             Southland Region        Southland Region      
#> 14 layer-111182.14 16             Tasman Region           Tasman Region         
#> 15 layer-111182.15 17             Nelson Region           Nelson Region         
#> 16 layer-111182.16 18             Marlborough Region      Marlborough Region    
#> 17 layer-111182.17 99             Area Outside Region     Area Outside Region   
#> # ℹ abbreviated name: ¹​REGC2023_V1_00_NAME_ASCII
#> # ℹ 5 more variables: LAND_AREA_SQ_KM <dbl>, AREA_SQ_KM <dbl>,
#> #   Shape_Length <dbl>, Shape_Area <dbl>, geometry <MULTIPOLYGON [m]>
```

#### Visualise the shapefile data

This can be easily visualised using the `tmap` package.

These regions are the boundaries of the regional councils in New
Zealand. However the Chatham islands are across the international date
line and can make plotting difficult. We will remove these from the
shapefile.

``` r

# Remove the Chatham Islands from the shapefile
nz_shapefile <- nz_shapefile |> 
  dplyr::filter(REGC2023_V1_00_NAME != "Area Outside Region")
```

``` r

# View the shapefile data
tm_shape(nz_shapefile, name = "Regional Council boundaries") +
  tm_borders(col = "blue") +
  tm_fill(fill = "REGC2023_V1_00_NAME", 
          fill.legend = tm_legend(title = "Region"),
          fill_alpha = 0.3, id = "REGC2023_V1_00_NAME")
```

## Access elevation data from LINZ

We will use Lake Rotoroa as an example to demonstrate how to access
elevation data from the LINZ Data Service.

### Define the location of the lake

We will define the location of Lake Rotoroa and create a spatial
dataframe of a point in the lake using the `sf` package.

``` r

# Define the location of the lake
lat <- -37.79881
lon <- 175.2742
lake <- "Lake Rotoroa"


# Create a spatial dataframe of the lake point
pnt <- sf::st_point(c(lon, lat)) |> 
  sf::st_sfc(crs = 4326) |> 
  sf::st_sf() |> 
  sf::st_set_geometry(value = "geometry") |> 
  dplyr::mutate(title = lake)
pnt
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 175.2742 ymin: -37.79881 xmax: 175.2742 ymax: -37.79881
#> Geodetic CRS:  WGS 84
#>                     geometry        title
#> 1 POINT (175.2742 -37.79881) Lake Rotoroa
```

#### Visualise the location of the lake

We can visualise the location of the lake using the
[`tmap`](https://r-tmap.github.io/tmap/) package.

``` r

# View the location of the lake in a map
map <- tm_shape(pnt, name = "Lake") +
  tm_symbols(fill = "blue")

map +
  tm_view(set_view = 15)
```

### Query DEM metadata for elevation

#### DEM metadata

Within the aemetools package, there is a spatial features (sf) dataframe
which contains metadata about various Digital Elevation Models (DEMs)
for New Zealand which are available on the LINZ Data Service. We can use
this to find the elevation of Lake Rotoroa.

``` r

nz_dem_metadata
```

#### View DEM coverage

We can view the coverage of the DEM layers to see which layers intersect
with our lake.

``` r

# View the coverage of the DEM layers
tm_shape(nz_dem_metadata, name = "DEM coverage") +
  tm_polygons(fill = "red", col = "black", fill_alpha = 0.1, id = "title") +
  tm_shape(pnt, name = "Lake") +
  tm_dots(col = "blue", size = 0.2, popup.vars = "title") 
#> [deprecated] The arguments `popup.vars` and `popup.format` of `tm_dots()` are
#> deprecated.
#> ℹ Use `popup` with `tm_popup()` instead, e.g. `popup = tm_popup(vars = ...,
#>   format = ...)`.
#> This message is displayed once every 8 hours.
```

First we will transform our lake coordinates to NZTM and use this to
find which layers intersect with our lake from the DEM metadata using
the
[`sf::st_intersects`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)
function.

``` r

# Subset the DEM database for the elevation of the lake  
pnt_nz <- pnt |>
  sf::st_transform(crs = 2193)

# Subset the DEM layers that intersect with the lake
dem_layers <- nz_dem_metadata[sf::st_intersects(pnt_nz, nz_dem_metadata, 
                                                sparse = FALSE), ]
dem_layers[, c("title", "region")]
#> Simple feature collection with 2 features and 2 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 1067061 ymin: 4701317 xmax: 2114868 ymax: 6216387
#> Projected CRS: NZGD2000 / New Zealand Transverse Mercator 2000
#>                                     title         region
#> 24   NZ 8m Digital Elevation Model (2012)    New Zealand
#> 46 Waikato - Hamilton LiDAR 1m DEM (2019) Waikato Region
#>                          geometry
#> 24 POLYGON ((1473708 5012166, ...
#> 46 POLYGON ((1787680 5823600, ...
```

There are two layers that intersect with the lake. We will do a quick
visual inspection of the layers to see which one is the most suitable
for our lake.

``` r

# View the layers
dem_84 <- dem_layers |> 
  sf::st_transform(crs = 4326) 

tm_shape(dem_84) +
  tm_polygons(fill = "red", col = "black", popup.vars = "title", fill_alpha = 0.3) +
  # tm_borders(col = "red", popup.vars = "title") +
  tm_shape(pnt, name = "Lake") +
  tm_dots(col = "blue", size = 0.2, popup.vars = "title") +
  tm_view(set_view = c(lon, lat, 7))
```

### Querying the elevation values

For raster data, point queries (lat/lon) can be sent to the LINZ API. We
can query the elevation values for the lake from the different layers
using the `get_raster_layer_value` function. This function takes the
latitude and longitude of the point, and the layer id of the raster
layer. We will compare the elevation values from the different layers.

``` r

# Compare the lake elevation values from the different layers
val1 <- get_raster_layer_value(lat = lat, lon = lon, 
                               layer = dem_layers$layer_id[1])

val2 <- get_raster_layer_value(lat = lat, lon = lon, 
                               layer = dem_layers$layer_id[2])

data.frame(layer = dem_layers$title[1:2], elevation = c(val1, val2))
#>                                    layer elevation
#> 1   NZ 8m Digital Elevation Model (2012)    38.000
#> 2 Waikato - Hamilton LiDAR 1m DEM (2019)    36.696
```

There is a discrepancy between the elevation values from the two layers.
We will use the layer from the second layer because it is a higher
resolution (1m) and is from more recent data (2019).

### Aerial imagery

Aerial imagery is also available from the LINZ Data Service. We can use
this to view the lake and its surroundings.

Within the `aemetools` package, there is also spatial features (sf)
dataframe which contains metadata about various aerial imagery for New
Zealand which are available on the LINZ Data Service. We can use this to
source an aerial image of the lake.

``` r

# Query the aerial imagery metadata for the elevation of the lake  
# Subset the aerial imagery layers that intersect with the lake
aerial_layers <- nz_aerial_imagery_metadata[sf::st_intersects(pnt_nz, nz_aerial_imagery_metadata, 
                                                sparse = FALSE), ]
aerial_layers
#> Simple feature collection with 5 features and 7 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: 1734400 ymin: 5643600 xmax: 1921600 ymax: 5971200
#> Projected CRS: NZGD2000 / New Zealand Transverse Mercator 2000
#>     layer_id                                          title
#> 114   110193 Hamilton 0.05m Urban Aerial Photos (2020-2021)
#> 118   104164       Hamilton 0.1m Urban Aerial Photos (2019)
#> 119    88132    Hamilton 0.1m Urban Aerial Photos (2016-17)
#> 120   112254   Waikato 0.3m Rural Aerial Photos (2021-2023)
#> 121   104600   Waikato 0.3m Rural Aerial Photos (2016-2019)
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       abstract
#> 114                                                                                                                                                                                                                                                                                                                                                                                                                                                                        Orthophotography of Hamilton City captured in the flying season of 2020-2021. Coverage encompasses urban areas within the Hamilton City District.\n\nImagery was captured for Hamilton City Council by AAM NZ Ltd, 6 Ossian St, Napier, New Zealand.\n\nData comprises:\n• 1470 ortho-rectified RGB GeoTIFF images in NZTM projection, tiled into the LINZ Standard 1:1000 tile layout\n• Tile layout in NZTM projection containing relevant information.\n\nThe supplied imagery is in terms of New Zealand Transverse Mercator (NZTM) map projection. Please refer to the tile index layer for specific details, naming conventions, etc.\n\nImagery supplied as 5cm pixel resolution (0.05m GSD), 3-band (RGB) uncompressed GeoTIFF. The final spatial accuracy is ±0.15 at 90% confidence level in clear flat areas.\n\nIndex tiles for this dataset are available as layer [Hamilton 0.05m Urban Aerial Photos Index Tiles (2020-2021)](http://data.linz.govt.nz/layer/110134)
#> 118                                                                                                                                                                                                                                                                                                                                                                                                                                               Orthophotography of Hamilton City taken in the flying seasons (summer period) 2018-2019. Coverage encompassed the urban areas of Hamilton City Council area.\n\nImagery was captured for Hamilton City Council by Aerial Surveys Ltd, Unit A1, 8 Saturn Place, Albany,0632, New Zealand.\n\nData comprises:\n•1,472 ortho-rectified RGB GeoTIFF images in NZTM projection, tiled into the LINZ Standard 1:1,000 tile layout\n•Tile layout in NZTM projection containing relevant information.\n\nThe supplied imagery is in terms of New Zealand Transverse Mercator (NZTM) map projection. Please refer to the supplied tile layout shape file for specific details, naming conventions, etc.\n\nImagery supplied as 10cm pixel resolution (0.1m GSD), 3-band (RGB) uncompressed GeoTIFF. The final spatial accuracy is ±0.2 m @ 68% confidence level.\n\nIndex tiles for this dataset are available as layer [Hamilton 0.1m Urban Aerial Photos Index Tiles (2019)](http://data.linz.govt.nz/layer/104130)
#> 119                                                                                                                                                                                                                                                                                                                                                                                                                                    Orthophotography over Hamilton City taken in the flying season (summer period) 2016 -17.\n\nImagery was captured for the ‘Hamilton City Council’ by Aerial Surveys Ltd, Unit A1, 8 Saturn Place, Albany,0632, New Zealand.\n\nData comprises:\n•492 ortho-rectified RGB GeoTIFF images in NZTM projection, tiled into the LINZ Standard 1:1,000 tile layout\n•Tile layout in NZTM projection containing relevant information.\n\nThe supplied imagery is in terms of New Zealand Transverse Mercator (NZTM) map projection. The products are tiled into NZTopo50 1:1,000 tiles. Please refer to the supplied tile layout shape file for specific details, naming conventions, etc.\n\nImagery supplied as 10cm pixel resolution (0.1m GSD), 3-band (RGB) uncompressed GeoTIFF. The final spatial accuracy is ±0.2 m @ 95% confidence level in clear open spaces.\n\nIndex tiles for this dataset are available as layer [Hamilton 0.1m Urban Aerial Photos Index Tiles (2016-17)](http://data.linz.govt.nz/layer/88100)
#> 120 **This dataset is the capture completed during the 2020/21, 2021/22 and 2022/23 flying seasons. The total capture for this project to-date is 96.34%. The remaining area is scheduled for capture during the 2023/2024.**\n\nOrthophotography within the Waikato Region captured in the summer flying seasons between 2020 and 2023. Coverage encompasses The area of capture is located within the upper North Island and encompasses all or part of 11 territorial authorities. It also includes parts of Bay of Plenty, Hawke's Bay and Manawatū-Whanganui. \n\nImagery was captured for Waikato Regional Aerial Photography Service (WRAPS) 2021 by Aerial Surveys Ltd, Unit A1, 8 Saturn Place, Albany 0632, New Zealand.\n\nData comprises:\n• 3620 ortho-rectified RGB GeoTIFF images in NZTM projection, tiled into the LINZ Standard 1:5000 tile layout\n• Tile layout in NZTM projection containing relevant information.\n\nThe supplied imagery is in terms of New Zealand Transverse Mercator (NZTM) map projection. Please refer to the tile index layer for specific details, naming conventions, etc.\n\nImagery supplied as 30cm pixel resolution (0.3m GSD), 3-band (RGB) uncompressed GeoTIFF. The final spatial accuracy is ±0.5 at 95% confidence level in clear flat areas.\n\nIndex tiles for this dataset are available as layer [Waikato 0.3m Rural Aerial Photos Index Tiles (2021-2023)](http://data.linz.govt.nz/layer/112048)
#> 121                                                                                                                                                                                                             Orthophotography over the Waikato Region captured over three flying seasons (summer periods) 2016/2017, 2017/2018, and 2018/2019. The area of capture is located within the upper North Island and encompasses all or part of 11 territorial authorities. It also includes parts of Bay of Plenty, Hawke's Bay and Manawatu-Whanganui.\n\nImagery was captured  for the ‘Waikato Regional Aerial Photography Service (WRAPS) 2017 - 2019 ’  by Aerial Surveys Ltd, Unit A1, 8 Saturn Place, Albany,0632, New Zealand.\n\nData comprises:\n•3668 ortho-rectified RGB GeoTIFF images in NZTM projection, tiled into the LINZ Standard 1:5,000 tile layout\n•Tile layout in NZTM projection containing relevant information.\n\nThe supplied imagery is in terms of New Zealand Transverse Mercator (NZTM) map projection. Please refer to the supplied tile layout shape file for specific details, naming conventions, etc.\n\nImagery supplied as 30cm pixel resolution (0.30 m GSD), 3-band (RGB) uncompressed GeoTIFF. The final spatial accuracy is ±0.5 m @ 95% confidence level.\n\nIndex tiles for this dataset are available as layer [Waikato 0.3m Rural Aerial Photos Index Tiles (2016-2019)](http://data.linz.govt.nz/layer/104585)
#>             region  res units year                       geometry
#> 114 Waikato Region 0.05     m 2021 POLYGON ((1787680 5819280, ...
#> 118 Waikato Region 0.10     m 2019 POLYGON ((1790080 5811360, ...
#> 119 Waikato Region 0.10     m 2016 POLYGON ((1791040 5811360, ...
#> 120 Waikato Region 0.30     m 2023 MULTIPOLYGON (((1746400 570...
#> 121 Waikato Region 0.30     m 2019 MULTIPOLYGON (((1748800 570...
```

``` r

aerial_layers
```

Here we can view the metadata of the aerial imagery layers that
intersect with our lake.

We can view the coverage of the aerial imagery layers to see which
layers intersect with our lake.

``` r

# View the coverage of the aerial imagery layers
tm_shape(aerial_layers, name = "Aerial imagery coverage") +
  tm_polygons(fill = "yellow", col = "black", fill_alpha = 0.1, id = "title") +
  tm_shape(pnt, name = "Lake") +
  tm_symbols(fill = "blue")
```

We can download two aerial images for the lake and compare them.

It is important to note that the aerial imagery resolution is dependent
on the zoom level. The higher the zoom level, the higher the resolution.
We will use the level 17, as the higher the resolution, the longer it
takes to download the image.

This is important to note as even though some layers may have a higher
resolution, we may not need that resolution for our analysis. We can
compare the

``` r

aerial1 <- get_raster_tile(x = pnt, layer_id = aerial_layers$layer_id[1],
                           zoom = 16) 
aerial2 <- get_raster_tile(x = pnt, layer_id = aerial_layers$layer_id[4],
                           zoom = 16) 

tm_shape(aerial1) +
  tm_rgb() +
  tm_shape(aerial2) +
  tm_rgb()
```

#### Compare aerial imagery resolution

We can compare the resolution of the two aerial images to see which one
is more suitable for our lake.

``` r

# Compare the resolution of the aerial imagery but first transform to NZTM
res1 <- aerial1 |> 
  terra::project("+proj=nzmg") |> 
  terra::res()

res2 <- aerial2 |> 
  terra::project("+proj=nzmg") |> 
  terra::res()

data.frame(layer = aerial_layers$title[c(1, 4)],
           resolution = c(res1[1], res2[1]))
#>                                            layer resolution
#> 1 Hamilton 0.05m Urban Aerial Photos (2020-2021)   1.880565
#> 2   Waikato 0.3m Rural Aerial Photos (2021-2023)   1.880565
```

### Access LINZ Aerial Basemap

We can also download the [LINZ Aerial
Basemap](https://basemaps.linz.govt.nz/) to see the coverage of the
lake. You will need to sign up for a free API key to use this service.

``` r

# Get the LINZ basemap
linz_basemap <- get_linz_basemap_tile(x = pnt, zoom = 16)

tm_shape(linz_basemap, name = "LINZ Aerial Basemap") +
  tm_rgb()
```
