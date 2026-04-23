test_that("can create GR model inputs", {

  data_dir <- system.file("extdata/hydro/", package = "aemetools")
  lake <- readRDS(file.path(data_dir, "lake.rds"))
  reaches <- readRDS(file.path(data_dir, "reaches.rds"))
  catchments <- readRDS(file.path(data_dir, "catchments.rds"))
  met <- readRDS(file.path(data_dir, "met.rds"))
  obs_flow <- readRDS(file.path(data_dir, "obs_flow.rds"))
  FUN_MOD <- airGR::RunModel_GR4J
  id <- 4087861

  lat <- lake |>
    sf::st_transform(4326) |>
    sf::st_centroid() |>
    sf::st_coordinates() |>
    as.data.frame(x = _) |>
    dplyr::pull(Y)

  hydro_model <- make_GR_inputs(id = id, reaches = reaches, lake = lake,
                                catchments = catchments, obs_flow = obs_flow,
                                met = met, lat = lat, FUN_MOD = FUN_MOD,
                                plot = TRUE)

  testthat::expect_true(is(hydro_model, "HydroModel"))
  testthat::expect_true(isVirtualClass("HydroModel") ||
                          isClass("HydroModel"))
  testthat::expect_true(is.data.frame(hydro_model@data))
  testthat::expect_true(hydro_model@catchment_area > 0)
  testthat::expect_true(hydro_model@start >= 1 &&
                          hydro_model@start <= nrow(hydro_model@data))
})

test_that("can calibrate GR model", {

  data_dir <- system.file("extdata/hydro/", package = "aemetools")
  lake <- readRDS(file.path(data_dir, "lake.rds"))
  reaches <- readRDS(file.path(data_dir, "reaches.rds"))
  catchments <- readRDS(file.path(data_dir, "catchments.rds"))
  met <- readRDS(file.path(data_dir, "met.rds"))
  obs_flow <- readRDS(file.path(data_dir, "obs_flow.rds"))
  FUN_MOD <- airGR::RunModel_GR4J
  id <- 4087861

  lat <- lake |>
    sf::st_transform(4326) |>
    sf::st_centroid() |>
    sf::st_coordinates() |>
    as.data.frame(x = _) |>
    dplyr::pull(Y)

  hydro_model <- make_GR_inputs(id = id, reaches = reaches, lake = lake,
                                catchments = catchments, obs_flow = obs_flow,
                                met = met, lat = lat, FUN_MOD = FUN_MOD,
                                plot = TRUE)

  n_obs   <- nrow(hydro_model@data) - hydro_model@start + 1L
  cal_idx <- hydro_model@start:(hydro_model@start + floor(n_obs * 0.5) - 1L)

  calib <- calib_GR(hydro_model, run_index = cal_idx)

  testthat::expect_true(is(calib, "OutputsCalib"))
})

test_that("can run GR model", {

  data_dir <- system.file("extdata/hydro/", package = "aemetools")
  lake <- readRDS(file.path(data_dir, "lake.rds"))
  reaches <- readRDS(file.path(data_dir, "reaches.rds"))
  catchments <- readRDS(file.path(data_dir, "catchments.rds"))
  met <- readRDS(file.path(data_dir, "met.rds"))
  obs_flow <- readRDS(file.path(data_dir, "obs_flow.rds"))
  FUN_MOD <- airGR::RunModel_GR4J
  id <- 4087861

  lat <- lake |>
    sf::st_transform(4326) |>
    sf::st_centroid() |>
    sf::st_coordinates() |>
    as.data.frame(x = _) |>
    dplyr::pull(Y)

  hydro_model <- make_GR_inputs(id = id, reaches = reaches, lake = lake,
                                catchments = catchments, obs_flow = obs_flow,
                                met = met, lat = lat, FUN_MOD = FUN_MOD,
                                plot = TRUE)

  data(Param_Sets_GR4J, package = "airGR")
  param <- unlist(Param_Sets_GR4J[1, ])

  n_obs   <- nrow(hydro_model@data) - hydro_model@start + 1L
  cal_idx <- hydro_model@start:(hydro_model@start + floor(n_obs * 0.5) - 1L)

  output <- run_GR(hydro_model, param = param, run_index = cal_idx)

  testthat::expect_true(is(output, "OutputsModel"))
})

test_that("can calibrate and run GR model", {

  data_dir <- system.file("extdata/hydro/", package = "aemetools")
  lake <- readRDS(file.path(data_dir, "lake.rds"))
  reaches <- readRDS(file.path(data_dir, "reaches.rds"))
  catchments <- readRDS(file.path(data_dir, "catchments.rds"))
  met <- readRDS(file.path(data_dir, "met.rds"))
  obs_flow <- readRDS(file.path(data_dir, "obs_flow.rds"))
  FUN_MOD <- airGR::RunModel_GR4J
  id <- 4087861

  lat <- lake |>
    sf::st_transform(4326) |>
    sf::st_centroid() |>
    sf::st_coordinates() |>
    as.data.frame(x = _) |>
    dplyr::pull(Y)

  hydro_model <- make_GR_inputs(id = id, reaches = reaches, lake = lake,
                                catchments = catchments, obs_flow = obs_flow,
                                met = met, lat = lat, FUN_MOD = FUN_MOD,
                                plot = TRUE)

  n_obs   <- nrow(hydro_model@data) - hydro_model@start + 1L
  cal_idx <- hydro_model@start:(hydro_model@start + floor(n_obs * 0.5) - 1L)

  calib <- calib_GR(hydro_model, run_index = cal_idx)

  param <- calib$ParamFinalR

  output <- run_GR(hydro_model, param = param, run_index = cal_idx)

  testthat::expect_true(is(output, "OutputsModel"))
})

test_that("run_GR and calib_GR use full observation range when run_index is omitted", {

  data_dir <- system.file("extdata/hydro/", package = "aemetools")
  lake <- readRDS(file.path(data_dir, "lake.rds"))
  reaches <- readRDS(file.path(data_dir, "reaches.rds"))
  catchments <- readRDS(file.path(data_dir, "catchments.rds"))
  met <- readRDS(file.path(data_dir, "met.rds"))
  obs_flow <- readRDS(file.path(data_dir, "obs_flow.rds"))
  FUN_MOD <- airGR::RunModel_GR4J
  id <- 4087861

  lat <- lake |>
    sf::st_transform(4326) |>
    sf::st_centroid() |>
    sf::st_coordinates() |>
    as.data.frame(x = _) |>
    dplyr::pull(Y)

  hydro_model <- make_GR_inputs(id = id, reaches = reaches, lake = lake,
                                catchments = catchments, obs_flow = obs_flow,
                                met = met, lat = lat, FUN_MOD = FUN_MOD)

  calib  <- calib_GR(hydro_model)
  param  <- calib$ParamFinalR
  output <- run_GR(hydro_model, param = param)

  expected_len <- nrow(hydro_model@data) - hydro_model@start + 1L
  testthat::expect_equal(length(output$Qsim), expected_len)
})
