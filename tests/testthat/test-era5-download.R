test_that("can download ERA5 point data", {

  lon <- 176.2717
  lat <- -38.079
  data("era5_ref_table", package = "AEME")
  variables <- c("MET_tmpair", "MET_tmpdew", "MET_wnduvu")

  met <- get_era5_point(lat = lat, lon = lon, years = 2000,
                        variables = variables, format = "aeme", parallel = TRUE)

  testthat::expect_true(is.data.frame(met))
  testthat::expect_true(ncol(met) == 4)
})

test_that("can download ERA5 point data outside of grid", {

  lon <- 179
  lat <- -38.079
  data("era5_ref_table", package = "AEME")
  variables <- c("MET_tmpair")

  met <- get_era5_point(lat = lat, lon = lon, years = 2000,
                        variables = variables, format = "aeme", parallel = FALSE)

  testthat::expect_true(is.data.frame(met))
  testthat::expect_true(ncol(met) == 2)
  testthat::expect_true(all(!is.na(met[, 2])))
})

