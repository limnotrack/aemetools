test_that("can download ERA5 point data", {
  
  lon <- 176.2717
  lat <- -38.079
  data("era5_ref_table", package = "aemetools")
  vars <- c("MET_tmpair", "MET_tmpdew", "MET_wnduvu")
  
  met <- get_era5_land_point_nz(lat = lat, lon = lon, years = 2023,
                                vars = vars)
  
  testthat::expect_true(is.data.frame(met))
  testthat::expect_true(ncol(met) == 4)
  met <- get_era5_land_point_nz(lat = lat, lon = lon, years = 2023:2024,
                                vars = vars)
  
  testthat::expect_true(is.data.frame(met))
  testthat::expect_true(ncol(met) == 4)
})

test_that("can check API status", {
  chk <- check_api_status()
  testthat::expect_true(chk)
})

test_that("can get lake shape from API", {
  lake <- get_lake_shape(id = 1)
  testthat::expect_true(inherits(lake, "sf"))
  
  lakes <- get_lake_shape(id = c(1, 3))
  testthat::expect_true(inherits(lakes, "sf"))
  testthat::expect_true(nrow(lakes) == 2)
})

test_that("can get lake catchment from API", {
  catch <- get_catchment_data(id = 3)
  testthat::expect_true(inherits(catch, "list"))
  testthat::expect_true(all(c("catchment", "reaches", "lakes",
                             "subcatchments", "lcdb") %in% names(catch)))
  
  catchments <- get_catchment_data(id = c(1, 3))
  testthat::expect_true(inherits(catchments, "list"))
  lids <- unique(catchments$catchment$lernzmp_id)
  testthat::expect_true(length(lids) == 2)
})

test_that("can get Aeme object from API", {
  aeme <- get_aeme(id = 1)
  testthat::expect_true(inherits(aeme, "Aeme"))
})
