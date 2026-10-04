test_that("can check API status", {
  skip_if_offline()
  chk <- check_api_status()
  testthat::expect_true(chk)
})

test_that("can get lake shape from API", {
  skip_if_offline()
  lake <- get_lake_shape(id = 1)
  testthat::expect_true(inherits(lake, "sf"))
  
  lakes <- get_lake_shape(id = c(1, 3))
  testthat::expect_true(inherits(lakes, "sf"))
  testthat::expect_true(nrow(lakes) == 2)
})

test_that("can get lake depth contours from API", {
  skip_if_offline()
  lake <- get_depth_contours(id = 1)
  testthat::expect_true(all(lake$depth <= 0))
  testthat::expect_true(inherits(lake, "sf"))
})



test_that("can get lake catchment from API", {
  skip_if_offline()
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
  skip_if_offline()
  aeme <- get_aeme(id = 1)
  testthat::expect_true(inherits(aeme, "Aeme"))
})
