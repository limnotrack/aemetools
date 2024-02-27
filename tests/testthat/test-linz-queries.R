test_that("can get DEM value", {

  lon <- 176.2717
  lat <- -38.079

  dem <- get_dem_value(lat = lat, lon = lon)

  testthat::expect_equal(dem, 282)

})

test_that("can get layer value", {

  lon <- 175.337788
  lat <- -37.860736

  elev <- get_raster_layer_value(lat = lat, lon = lon, layer_id = 104772)

  testthat::expect_equal(elev, 52.280998)

})

test_that("can get DEM raster tile", {

  coords <-  nz_dem_metadata[2, ] |>
    sf::st_transform(crs = 4326) |>
    sf::st_centroid() |>
    sf::st_coordinates() |>
    as.numeric()
  lon <- coords[1]
  lat <- coords[2]

  # Make an sf object
  shape <- sf::st_point(x = c(lon, lat), dim = "XY") |>
    sf::st_sfc(crs = 4326) |>
    sf::st_as_sf()

  ras <- get_raster_tile(shape = shape, layer_id = nz_dem_metadata$layer_id[2])

  testthat::expect_true(is(ras, "SpatRaster"))
  testthat::expect_equal(mean(terra::values(ras), na.rm = T), 19.592725)

})

test_that("can get aerial image raster tile", {

  coords <- nz_aerial_imagery_metadata[1, ] |>
    sf::st_transform(crs = 4326) |>
    sf::st_centroid() |>
    sf::st_coordinates() |>
    as.numeric()
  lon <- coords[1]
  lat <- coords[2]

  # Make an sf object
  shape <- sf::st_point(x = c(lon, lat), dim = "XY") |>
    sf::st_sfc(crs = 4326) |>
    sf::st_as_sf()

  ras <- get_raster_tile(shape = shape,
                         layer_id = nz_aerial_imagery_metadata$layer_id[1])

  testthat::expect_true(is(ras, "SpatRaster"))
  testthat::expect_equal(mean(terra::values(ras), na.rm = T), 71.196337)

})


test_that("can get LINZ sf object", {

  reg_council <- get_linz_sf(url = "https://datafinder.stats.govt.nz/",
                             layer_id = 111182)

  testthat::expect_true(is(reg_council, "sf"))
  testthat::expect_equal(reg_council$C2023_V[1], "Northland Region")

})

test_that("can pass a polygon and subset a LINZ sf object", {

  # Create a square polygon in the middle of the Northland region
  shape <- sf::st_polygon(list(rbind(c(173.5, -35),
                                    c(173.5, -34.5),
                                    c(174, -34.5),
                                    c(174, -35),
                                    c(173.5, -35)))) |>
    sf::st_sfc(crs = 4326) |>
    sf::st_as_sf()

  layer_id <- 50768

  pnts <- get_linz_sf(shape, url = "https://data.linz.govt.nz",
                      layer_id = layer_id)
  pnts <- pnts |>
    sf::st_transform(crs = 4326) |>

  library(leaflet)
  leaflet() |>
    addWMSTiles(baseUrl = "https://basemaps.linz.govt.nz/v1/tiles/aerial/WebMercatorQuad/{z}/{x}/{y}.webp?api=c01hbexn54aeskebvrbthr8e2vf", layers = "Basemap", group = "basemap", options = list(maxZoom = 18)) |>
    addCircleMarkers(data = pnts, radius = 1, color = "red", group = "points") |>
    addPolygons(data = shape, color = "blue", group = "polygon")

  testthat::expect_true(is(reg_council, "sf"))
  testthat::expect_equal(reg_council$C2023_V[1], "Northland Region")

})

test_that("can pass a polygon and subset a LDCD feature layer", {

  # Create a square polygon in the middle of the Northland region
  shape <- sf::st_polygon(list(rbind(c(173.5, -35),
                                     c(173.5, -34.5),
                                     c(174, -34.5),
                                     c(174, -35),
                                     c(173.5, -35)))) |>
    sf::st_sfc(crs = 4326) |>
    sf::st_as_sf()

  layer_id <- 104400
  url <- "https://lris.scinfo.org.nz"
  version <- "1.1.0"

  lc <- get_linz_sf(shape, url = url, version = version,
                    layer_id = layer_id, key = "6a64f28728c945f99897fa3c86c1db85")
  pnts <- pnts |>
    sf::st_transform(crs = 4326) |>

    library(leaflet)
  leaflet() |>
    addWMSTiles(baseUrl = "https://basemaps.linz.govt.nz/v1/tiles/aerial/WebMercatorQuad/{z}/{x}/{y}.webp?api=c01hbexn54aeskebvrbthr8e2vf", layers = "Basemap", group = "basemap", options = list(maxZoom = 18)) |>
    addCircleMarkers(data = pnts, radius = 1, color = "red", group = "points") |>
    addPolygons(data = shape, color = "blue", group = "polygon")

  testthat::expect_true(is(reg_council, "sf"))
  testthat::expect_equal(reg_council$C2023_V[1], "Northland Region")

})

