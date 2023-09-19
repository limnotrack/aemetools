test_that("can create GR model inputs", {

  data_dir <- system.file("extdata/hydro/", package = "aemetools")
  lake <- readRDS(file.path(data_dir, "lake.rds"))
  reaches <- readRDS(file.path(data_dir, "reaches.rds"))
  catchment <- readRDS(file.path(data_dir, "catchment.rds"))
  met <- readRDS(file.path(data_dir, "met.rds"))
  flow <- readRDS(file.path(data_dir, "flow.rds"))
  FUN_MOD <- airGR::RunModel_GR4J
  id <- 4087861

  lat <- lake %>%
    sf::st_transform(4236) %>%
    sf::st_centroid() %>%
    sf::st_coordinates() %>%
    as.data.frame(.) %>%
    dplyr::pull(Y)

  inputs <- make_GR_inputs(id = id, reaches = reaches, lake = lake,
                           catchment = catchment, flow = flow, met = met,
                           lat = lat, FUN_MOD = FUN_MOD,
                           plot = TRUE)

  testthat::expect_true(length(inputs) == 5)
  testthat::expect_true(all(names(inputs) == c("InputsModel", "data", "start",
                                           "FUN_MOD", "catchment_area")))
  testthat::expect_true(is.data.frame(inputs$data))
})

test_that("can calibrate GR model", {

  data_dir <- system.file("extdata/hydro/", package = "aemetools")
  lake <- readRDS(file.path(data_dir, "lake.rds"))
  reaches <- readRDS(file.path(data_dir, "reaches.rds"))
  catchment <- readRDS(file.path(data_dir, "catchment.rds"))
  met <- readRDS(file.path(data_dir, "met.rds"))
  flow <- readRDS(file.path(data_dir, "flow.rds"))
  FUN_MOD <- airGR::RunModel_GR4J
  id <- 4087861

  lat <- lake %>%
    sf::st_transform(4236) %>%
    sf::st_centroid() %>%
    sf::st_coordinates() %>%
    as.data.frame(.) %>%
    dplyr::pull(Y)

  inputs <- make_GR_inputs(id = id, reaches = reaches, lake = lake,
                           catchment = catchment, flow = flow, met = met,
                           lat = lat, FUN_MOD = FUN_MOD,
                           plot = TRUE)

  idx_spl <- floor(nrow(inputs$data[inputs$start:nrow(inputs$data), ])
                   * 0.5)
  warmup <- 1:(inputs$start - 1)
  cal_idx <- inputs$start:(idx_spl + inputs$start)

  calib <- calib_GR(inputs = inputs, warmup = warmup, run_index = cal_idx)

  testthat::expect_true(is(calib, "OutputsCalib"))
})

test_that("can run GR model", {

  data_dir <- system.file("extdata/hydro/", package = "aemetools")
  lake <- readRDS(file.path(data_dir, "lake.rds"))
  reaches <- readRDS(file.path(data_dir, "reaches.rds"))
  catchment <- readRDS(file.path(data_dir, "catchment.rds"))
  met <- readRDS(file.path(data_dir, "met.rds"))
  flow <- readRDS(file.path(data_dir, "flow.rds"))
  FUN_MOD <- airGR::RunModel_GR4J
  id <- 4087861

  lat <- lake %>%
    sf::st_transform(4236) %>%
    sf::st_centroid() %>%
    sf::st_coordinates() %>%
    as.data.frame(.) %>%
    dplyr::pull(Y)

  inputs <- make_GR_inputs(id = id, reaches = reaches, lake = lake,
                           catchment = catchment, flow = flow, met = met,
                           lat = lat, FUN_MOD = FUN_MOD,
                           plot = TRUE)

  data(Param_Sets_GR4J, package = "airGR")
  param <- unlist(Param_Sets_GR4J[1, ])

  idx_spl <- floor(nrow(inputs$data[inputs$start:nrow(inputs$data), ])
                   * 0.5)
  warmup <- 1:(inputs$start - 1)
  cal_idx <- inputs$start:(idx_spl + inputs$start)

  output <- run_GR(inputs = inputs, param = param,
                   warmup = warmup, run_index = cal_idx)

  testthat::expect_true(is(output, "OutputsModel"))
})

test_that("can calibrate and run GR model", {

  data_dir <- system.file("extdata/hydro/", package = "aemetools")
  lake <- readRDS(file.path(data_dir, "lake.rds"))
  reaches <- readRDS(file.path(data_dir, "reaches.rds"))
  catchment <- readRDS(file.path(data_dir, "catchment.rds"))
  met <- readRDS(file.path(data_dir, "met.rds"))
  flow <- readRDS(file.path(data_dir, "flow.rds"))
  FUN_MOD <- airGR::RunModel_GR4J
  id <- 4087861

  lat <- lake %>%
    sf::st_transform(4236) %>%
    sf::st_centroid() %>%
    sf::st_coordinates() %>%
    as.data.frame(.) %>%
    dplyr::pull(Y)

  inputs <- make_GR_inputs(id = id, reaches = reaches, lake = lake,
                           catchment = catchment, flow = flow, met = met,
                           lat = lat, FUN_MOD = FUN_MOD,
                           plot = TRUE)

  idx_spl <- floor(nrow(inputs$data[inputs$start:nrow(inputs$data), ])
                   * 0.5)
  warmup <- 1:(inputs$start - 1)
  cal_idx <- inputs$start:(idx_spl + inputs$start)

  calib <- calib_GR(inputs = inputs, warmup = warmup, run_index = cal_idx)

  param <- calib$ParamFinalR

  output <- run_GR(inputs = inputs, param = param,
                   warmup = warmup, run_index = cal_idx)

  # plot(output, Qobs = inputs$data$Qmm[cal_idx])

  testthat::expect_true(is(output, "OutputsModel"))
})
