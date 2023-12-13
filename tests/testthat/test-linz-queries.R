test_that("can get DEM value", {

  lon <- 176.2717
  lat <- -38.079

  dem <- get_dem_value(lat = lat, lon = lon)

  testthat::expect_equal(dem, 282)

})

test_that("can get layer value", {

  lon <- 175.337788
  lat <- -37.860736

  elev <- get_raster_layer_value(lat = lat, lon = lon, layer = 104772)

  testthat::expect_equal(elev, 52.280998)

})
