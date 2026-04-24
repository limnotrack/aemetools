test_that("can create spatial HydroModel", {

  data_dir   <- system.file("extdata/hydro/", package = "aemetools")
  lake       <- readRDS(file.path(data_dir, "lake.rds"))
  reaches    <- readRDS(file.path(data_dir, "reaches.rds"))
  catchments <- readRDS(file.path(data_dir, "catchments.rds"))
  id         <- 4087861

  hm <- make_hydro_model(id = id, reaches = reaches, lake = lake,
                         catchments = catchments, plot = TRUE)

  testthat::expect_true(is(hm, "HydroModel"))
  testthat::expect_true(hm@catchment_area > 0)
  testthat::expect_false(is.null(hm@channels))
  testthat::expect_false(is.null(hm@lake))
  testthat::expect_false(is.null(hm@catchments))
  # GR slots should be unpopulated at this stage
  testthat::expect_null(hm@inputs_model)
  testthat::expect_null(hm@data)
  testthat::expect_true(is.na(hm@start))
})

test_that("can add GR inputs to HydroModel", {

  data_dir   <- system.file("extdata/hydro/", package = "aemetools")
  lake       <- readRDS(file.path(data_dir, "lake.rds"))
  reaches    <- readRDS(file.path(data_dir, "reaches.rds"))
  catchments <- readRDS(file.path(data_dir, "catchments.rds"))
  met        <- readRDS(file.path(data_dir, "met.rds"))
  obs_flow   <- readRDS(file.path(data_dir, "obs_flow.rds"))
  id         <- 4087861

  hm <- make_hydro_model(id = id, reaches = reaches, lake = lake,
                         catchments = catchments)

  hm <- make_GR_inputs(hm, met = met, obs_flow = obs_flow,
                       FUN_MOD = airGR::RunModel_GR4J)

  testthat::expect_true(is(hm, "HydroModel"))
  testthat::expect_true(is.data.frame(hm@data))
  testthat::expect_false(is.null(hm@inputs_model))
  testthat::expect_true(hm@start >= 1 && hm@start <= nrow(hm@data))
})

test_that("can calibrate GR model", {

  data_dir   <- system.file("extdata/hydro/", package = "aemetools")
  lake       <- readRDS(file.path(data_dir, "lake.rds"))
  reaches    <- readRDS(file.path(data_dir, "reaches.rds"))
  catchments <- readRDS(file.path(data_dir, "catchments.rds"))
  met        <- readRDS(file.path(data_dir, "met.rds"))
  obs_flow   <- readRDS(file.path(data_dir, "obs_flow.rds"))
  id         <- 4087861

  hm <- make_hydro_model(id = id, reaches = reaches, lake = lake,
                         catchments = catchments)
  hm <- make_GR_inputs(hm, met = met, obs_flow = obs_flow,
                       FUN_MOD = airGR::RunModel_GR4J)

  n_obs   <- nrow(hm@data) - hm@start + 1L
  cal_idx <- hm@start:(hm@start + floor(n_obs * 0.5) - 1L)

  calib <- calib_GR(hm, run_index = cal_idx)

  testthat::expect_true(is(calib, "OutputsCalib"))
})

test_that("can run GR model", {

  data_dir   <- system.file("extdata/hydro/", package = "aemetools")
  lake       <- readRDS(file.path(data_dir, "lake.rds"))
  reaches    <- readRDS(file.path(data_dir, "reaches.rds"))
  catchments <- readRDS(file.path(data_dir, "catchments.rds"))
  met        <- readRDS(file.path(data_dir, "met.rds"))
  obs_flow   <- readRDS(file.path(data_dir, "obs_flow.rds"))
  id         <- 4087861

  hm <- make_hydro_model(id = id, reaches = reaches, lake = lake,
                         catchments = catchments)
  hm <- make_GR_inputs(hm, met = met, obs_flow = obs_flow,
                       FUN_MOD = airGR::RunModel_GR4J)

  data(Param_Sets_GR4J, package = "airGR")
  param <- unlist(Param_Sets_GR4J[1, ])

  n_obs   <- nrow(hm@data) - hm@start + 1L
  cal_idx <- hm@start:(hm@start + floor(n_obs * 0.5) - 1L)

  output <- run_GR(hm, param = param, run_index = cal_idx)

  testthat::expect_true(is(output, "OutputsModel"))
})

test_that("can calibrate and run GR model", {

  data_dir   <- system.file("extdata/hydro/", package = "aemetools")
  lake       <- readRDS(file.path(data_dir, "lake.rds"))
  reaches    <- readRDS(file.path(data_dir, "reaches.rds"))
  catchments <- readRDS(file.path(data_dir, "catchments.rds"))
  met        <- readRDS(file.path(data_dir, "met.rds"))
  obs_flow   <- readRDS(file.path(data_dir, "obs_flow.rds"))
  id         <- 4087861

  hm <- make_hydro_model(id = id, reaches = reaches, lake = lake,
                         catchments = catchments)
  hm <- make_GR_inputs(hm, met = met, obs_flow = obs_flow,
                       FUN_MOD = airGR::RunModel_GR4J)

  n_obs   <- nrow(hm@data) - hm@start + 1L
  cal_idx <- hm@start:(hm@start + floor(n_obs * 0.5) - 1L)

  calib  <- calib_GR(hm, run_index = cal_idx)
  param  <- calib$ParamFinalR
  output <- run_GR(hm, param = param, run_index = cal_idx)

  testthat::expect_true(is(output, "OutputsModel"))
})

test_that("run_GR and calib_GR use full observation range when run_index is omitted", {

  data_dir   <- system.file("extdata/hydro/", package = "aemetools")
  lake       <- readRDS(file.path(data_dir, "lake.rds"))
  reaches    <- readRDS(file.path(data_dir, "reaches.rds"))
  catchments <- readRDS(file.path(data_dir, "catchments.rds"))
  met        <- readRDS(file.path(data_dir, "met.rds"))
  obs_flow   <- readRDS(file.path(data_dir, "obs_flow.rds"))
  id         <- 4087861

  hm <- make_hydro_model(id = id, reaches = reaches, lake = lake,
                         catchments = catchments)
  hm <- make_GR_inputs(hm, met = met, obs_flow = obs_flow,
                       FUN_MOD = airGR::RunModel_GR4J)

  calib  <- calib_GR(hm)
  param  <- calib$ParamFinalR
  output <- run_GR(hm, param = param)

  expected_len <- nrow(hm@data) - hm@start + 1L
  testthat::expect_equal(length(output$Qsim), expected_len)
})

test_that("run_GR errors when GR inputs are missing", {

  data_dir   <- system.file("extdata/hydro/", package = "aemetools")
  lake       <- readRDS(file.path(data_dir, "lake.rds"))
  reaches    <- readRDS(file.path(data_dir, "reaches.rds"))
  catchments <- readRDS(file.path(data_dir, "catchments.rds"))
  id         <- 4087861

  hm <- make_hydro_model(id = id, reaches = reaches, lake = lake,
                         catchments = catchments)

  data(Param_Sets_GR4J, package = "airGR")
  param <- unlist(Param_Sets_GR4J[1, ])

  testthat::expect_error(run_GR(hm, param = param),
                         "make_GR_inputs")
})
