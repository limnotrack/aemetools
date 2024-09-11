test_that("running GLM & GOTM works with params", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  # unlink(tmpdir, recursive = TRUE)
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  model_controls <- model_controls |>
    dplyr::mutate(simulate = dplyr::case_when(
      var_aeme == "ZOO_zoo1" ~ TRUE,
      .default = simulate
    ))
  model <- c("glm_aed", "gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                               model = model, model_controls = model_controls,
                               ext_elev = 5, use_bgc = FALSE)

  lke <- AEME::lake(aeme)

  # GLM
  glm_met_file <- file.path(path, paste0(lke$id, "_", lke$name), "glm_aed",
                        "bcs", "meteo_glm.csv")
  glm_inf_file <- file.path(path, paste0(lke$id, "_", lke$name), "glm_aed",
                        "bcs", "inflow_FWMT.csv")
  glm_outf_file <- file.path(path, paste0(lke$id, "_", lke$name), "glm_aed",
                            "bcs", "outflow_outflow.csv")
  glm_met1 <- read.csv(glm_met_file)
  glm_inf1 <- read.csv(glm_inf_file)
  glm_outf1 <- read.csv(glm_outf_file)

  # GOTM
  gotm_met_file <- file.path(path, paste0(lke$id, "_", lke$name), "gotm_wet",
                            "inputs", "meteo.dat")
  gotm_inf_file <- file.path(path, paste0(lke$id, "_", lke$name), "gotm_wet",
                            "inputs", "inf_flow_FWMT.dat")
  gotm_outf_file <- file.path(path, paste0(lke$id, "_", lke$name), "gotm_wet",
                             "inputs", "outf_outflow.dat")
  gotm_met1 <- read.delim(gotm_met_file, header = FALSE)
  gotm_inf1 <- read.delim(gotm_inf_file, header = FALSE)
  gotm_outf1 <- read.delim(gotm_outf_file, header = FALSE)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters |>
    dplyr::mutate(value = dplyr::case_when(
      name == "MET_wndspd" ~ 0,
      name == "inflow" ~ 0,
      name == "outflow" ~ 0,
      .default = value
    ))
  # param <- dplyr::bind_rows(
  #   # aeme_parameters_bgc,
  #   glm_aed_parameters
  # ) |>
  #   dplyr::filter(model == "glm_aed")
  # run_aeme_shiny(aeme = aeme, param = param, path = path,
  #                model_controls = model_controls)

  aeme <- run_aeme_param(aeme = aeme,
                         model = model,
                         param = param, path = path,
                         model_controls = model_controls,
                         na_value = 999, return_aeme = TRUE)

  # GLM
  glm_met2 <- read.csv(glm_met_file)
  testthat::expect_true(all(glm_met1$WindSpeed > 0))
  testthat::expect_true(all(glm_met2$WindSpeed == 0))

  glm_inf2 <- read.csv(glm_inf_file)
  testthat::expect_true(any(glm_inf1$flow > 0))
  testthat::expect_true(all(glm_inf2$flow == 0))

  glm_outf2 <- read.csv(glm_outf_file)
  testthat::expect_true(any(glm_outf1$flow > 0))
  testthat::expect_true(all(glm_outf2$flow == 0))


  # GOTM
  gotm_met2 <- read.delim(gotm_met_file, header = FALSE)
  testthat::expect_true(any(gotm_met1[, 3] > 0 | gotm_met1[, 4] > 0))
  testthat::expect_true(all(gotm_met2[, 3] == 0 & gotm_met2[, 4] == 0))

  gotm_inf2 <- read.delim(gotm_inf_file, header = FALSE)
  testthat::expect_true(any(gotm_inf1[, 3] > 0))
  testthat::expect_true(all(gotm_inf2[, 3] == 0))

  gotm_outf2 <- read.delim(gotm_outf_file, header = FALSE)
  testthat::expect_true(any(gotm_outf1[, 3] < 0))
  testthat::expect_true(all(gotm_outf2[, 3] == 0))

  # AEME::plot_output(aeme, model = "glm_aed", var_sim = "PHY_tchla")
  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(all(file_chk))
})

test_that("running GOTM with different grid", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  # unlink(tmpdir, recursive = TRUE)
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  model_controls <- model_controls |>
    dplyr::mutate(simulate = dplyr::case_when(
      var_aeme == "ZOO_zoo1" ~ TRUE,
      .default = simulate
    ))
  model <- c("gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           ext_elev = 5, use_bgc = FALSE)
  lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)

  cfg <- AEME::configuration(aeme)
  nlev <- cfg$gotm_wet$hydrodynamic$gotm$grid$nlev
  depth <- cfg$gotm_wet$hydrodynamic$gotm$location$depth
  method <- 1
  AEME::set_gotm_grid(depth = depth, aeme = aeme, path = path,
                      thickness_factor = 2)

  aeme <- AEME::run_aeme(aeme = aeme, model = model, path = path,
                 model_controls = model_controls, verbose = TRUE)
  nc <- ncdf4::nc_open(file.path(lake_dir, model, "output", "output.nc"))
  h <- ncdf4::ncvar_get(nc, "h")
  ncdf4::nc_close(nc)
  testthat::expect_true(nrow(h) == 28)
})

test_that("running DYRESM works with params", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  # unlink(tmpdir, recursive = TRUE)
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  model_controls <- model_controls |>
    dplyr::mutate(simulate = dplyr::case_when(
      var_aeme == "ZOO_zoo1" ~ TRUE,
      .default = simulate
    ))
  model <- c("dy_cd")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           ext_elev = 5, use_bgc = FALSE)

  lke <- AEME::lake(aeme)

  # DYRESM
  dy_met_file <- file.path(path, paste0(lke$id, "_", lke$name), "dy_cd",
                            "wainamu.met")
  dy_inf_file <- file.path(path, paste0(lke$id, "_", lke$name), "dy_cd",
                            "wainamu.inf")
  dy_outf_file <- file.path(path, paste0(lke$id, "_", lke$name), "dy_cd",
                             "wainamu.wdr")
  dy_met1 <- read.delim(dy_met_file, skip = 5)
  dy_inf1 <- read.delim(dy_inf_file, skip = 3)
  dy_outf1 <- read.delim(dy_outf_file, skip = 2)


  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters |>
    dplyr::mutate(value = dplyr::case_when(
      name == "MET_wndspd" ~ 0,
      name == "inflow" ~ 0,
      name == "outflow" ~ 0,
      .default = value
    ))
  # param <- dplyr::bind_rows(
  #   # aeme_parameters_bgc,
  #   glm_aed_parameters
  # ) |>
  #   dplyr::filter(model == "glm_aed")
  # run_aeme_shiny(aeme = aeme, param = param, path = path,
  #                model_controls = model_controls)

  aeme <- run_aeme_param(aeme = aeme,
                         model = model,
                         param = param, path = path,
                         model_controls = model_controls,
                         na_value = 999, return_aeme = TRUE)

  # DYRESM
  dy_met2 <- read.delim(dy_met_file, skip = 5)
  testthat::expect_true(all(dy_met1$WindSpeed > 0))
  testthat::expect_true(all(dy_met2$WindSpeed == 0))

  dy_inf2 <- read.delim(dy_inf_file, skip = 3)
  testthat::expect_true(any(dy_inf1$VOL > 0))
  testthat::expect_true(all(dy_inf2$VOL == 0))

  dy_outf2 <- read.delim(dy_outf_file, skip = 2)
  testthat::expect_true(any(dy_outf1$outflow > 0))
  testthat::expect_true(all(dy_outf2$flow == 0))

  # AEME::plot_output(aeme, model = "glm_aed", var_sim = "PHY_tchla")
  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(all(file_chk))
})

test_that("running GLM-AED works with bgc_params", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  # unlink(tmpdir, recursive = TRUE)
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls(use_bgc = TRUE)
  model_controls <- model_controls |>
    dplyr::mutate(simulate = dplyr::case_when(
      var_aeme == "ZOO_zoo1" ~ TRUE,
      .default = simulate
    ))
  model <- c("glm_aed")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                               model = model, model_controls = model_controls,
                               ext_elev = 5, use_bgc = TRUE)

  utils::data("glm_aed_parameters", package = "AEME")
  param <- glm_aed_parameters
  param <- param |>
    dplyr::filter(grepl("aed2_carbon|aed2_oxygen|aed2_phytoplankton|aed2_nitrogen|aed2_organic_matter|aed2_phosphorus|phyto_data|zoop_params", name))

  aeme <- run_aeme_param(aeme = aeme, model = model,
                         param = param, path = path,
                         model_controls = model_controls,
                         na_value = 999, return_aeme = TRUE)

  # AEME::plot_output(aeme, model = "glm_aed", var_sim = "PHY_tchla")
  lke <- AEME::lake(aeme)
  lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)
  file_chk <- file.exists(file.path(lake_dir, model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running GOTM-WET works with bgc_params", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  # unlink(tmpdir, recursive = TRUE)
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls(use_bgc = TRUE)
  model_controls <- model_controls |>
    dplyr::mutate(simulate = dplyr::case_when(
      var_aeme == "ZOO_zoo1" ~ TRUE,
      .default = simulate
    ))
  model <- c("gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme, model = model,
                           model_controls = model_controls,
                           ext_elev = 5, use_bgc = TRUE)

  utils::data("gotm_wet_parameters", package = "AEME")
  param <- gotm_wet_parameters |>
    dplyr::filter(grepl("oxygen|phytoplankton|nitrogen|carbon|phytoplankton|zooplankton", module))

  aeme <- run_aeme_param(aeme = aeme,
                         model = model,
                         param = param, path = path,
                         model_controls = model_controls,
                         na_value = 999, return_aeme = TRUE)

  # AEME::plot_output(aeme, model = "gotm_wet")
  lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)
  file_chk <- file.exists(file.path(lake_dir,
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("sensitivity analysis for GOTM-WET works with bgc_params", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls(use_bgc = TRUE)
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                               model = model, model_controls = model_controls,
                               inf_factor = inf_factor, ext_elev = 5,
                               use_bgc = TRUE)

  utils::data("gotm_wet_parameters", package = "AEME")
  param <- gotm_wet_parameters |>
    dplyr::filter(module %in% c("oxygen", "phytoplankton"))

  # Function to calculate fitness
  fit <- function(df) {
    mean(df$model)
  }

  FUN_list <- list(HYD_temp = fit, PHY_tchla = fit)

  ctrl <- create_control(method = "sa", N = 2^1, ncore = 2, na_value = 999,
                         parallel = TRUE, file_type = "db",
                         file_name = "results.db",
                         vars_sim = list(
                           surf_temp = list(var = "HYD_temp",
                                            month = c(10:12, 1:3),
                                            depth_range = c(0, 2)
                           ),
                           bot_temp = list(var = "HYD_temp",
                                           month = c(10:12, 1:3),
                                           depth_range = c(10, 13)
                           ),
                           surf_chla = list(var = "PHY_tchla",
                                            month = c(10:12, 1:3),
                                            depth_range = c(0, 2)
                           )
                         )
  )

  # Run sensitivity analysis AEME model
  sim_id <- sa_aeme(aeme = aeme, path = path, param = param, model = model,
                    ctrl = ctrl, model_controls = model_controls,
                    FUN_list = FUN_list)

  sa_res <- read_sa(ctrl = ctrl, sim_id = sim_id, boot = FALSE)

  testthat::expect_true(is.data.frame(sa_res[[1]]$df))

  p1 <- plot_uncertainty(sa = sa_res)
  testthat::expect_true(ggplot2::is.ggplot(p1))
})
