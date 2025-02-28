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

test_that("can download ERA5 point data outside of grid", {

  lon <- 179
  lat <- -38.079
  data("era5_ref_table", package = "aemetools")
  vars <- c("MET_tmpair")

  met <- get_era5_land_point_nz(lat = lat, lon = lon, years = 1980,
                                vars = vars)

  testthat::expect_true(is.data.frame(met))
  testthat::expect_true(ncol(met) == 2)
  testthat::expect_true(all(!is.na(met[, 2])))
})

test_that("can download from CDS", {

  testthat::skip("Skip test as it requires CDS key")

  lat <- -38.07782
  lon <- 176.2673
  year <- 2024
  month <- 1
  variable <- "2m_temperature"
  path <- "data/test"
  user <- Sys.getenv("CDS_USER")
  ecmwfr::wf_set_key(key = Sys.getenv("CDS_KEY"),
                     user = Sys.getenv("CDS_USER"))
  files <- download_era5_grib(lat = lat, lon = lon, year = year, month = month,
                              variable = variable, path = path,
                              user = Sys.getenv("CDS_USER"))
  df <- read_grib_point(file = files, lat = lat, lon = lon)
  testthat::expect_true(is.data.frame(df))
  testthat::expect_true(all(!is.na(df$value)))
})

test_that("can download ERA5-ISIMIP3a point data", {

  lon <- 175.27
  lat <- -37.80
  vars <- c("MET_tmpair", "MET_humrel", "MET_pprain", "MET_radswd", "MET_wndspd")

  met <- get_era5_isimip_point(lat = lat, lon = lon, years = 2021,
                               vars = vars)

  testthat::expect_true(is.data.frame(met))
  testthat::expect_true(ncol(met) == 6)
})


