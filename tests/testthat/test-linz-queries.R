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
  x <- sf::st_point(x = c(lon, lat), dim = "XY") |>
    sf::st_sfc(crs = 4326) |>
    sf::st_as_sf()

  ras <- get_raster_tile(x = x, layer_id = nz_dem_metadata$layer_id[2])

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
  x <- sf::st_point(x = c(lon, lat), dim = "XY") |>
    sf::st_sfc(crs = 4326) |>
    sf::st_as_sf()

  ras <- get_raster_tile(x = x,
                         layer_id = nz_aerial_imagery_metadata$layer_id[1])

  testthat::expect_true(is(ras, "SpatRaster"))
  testthat::expect_equal(mean(terra::values(ras), na.rm = T), 71.196337)

})


test_that("can get LINZ sf object", {

  reg_council <- read_web_sf(url = "https://datafinder.stats.govt.nz/",
                             layer_id = 111182)

  testthat::expect_true(is(reg_council, "sf"))
  testthat::expect_equal(reg_council$C2023_V[1], "Northland Region")

})

