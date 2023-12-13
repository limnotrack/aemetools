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

test_that("can get raster tile", {

  lon <- 175.337788
  lat <- -37.860736

  # Make an sf object
  shape <- sf::st_point(x = c(lon, lat), dim = "XY") |>
    sf::st_sfc(crs = 4326) |>
    sf::st_as_sf()

  ras <- get_raster_tile(shape = shape, layer_id = 104772)

  testthat::expect_true(is(ras, "SpatRaster"))
  testthat::expect_equal(mean(terra::values(ras), na.rm = T), 35.456204)

})

