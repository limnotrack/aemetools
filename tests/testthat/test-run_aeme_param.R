test_that("running GLM & GOTM works with params", {
  skip_if_slow()
  model <- c("glm_aed", "gotm_wet")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                vars_sim = "ZOO_zoo1", run = FALSE)
  aeme <- cached$aeme
  path <- cached$path
  model_controls <- AEME::get_model_controls()
  model_controls <- AEME::set_vars_sim(model_controls, vars_sim = "ZOO_zoo1")

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
  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, file.exists)
  testthat::expect_true(all(file_chk))
})

test_that("running GOTM with different grid", {
  skip_if_slow()
  model <- c("gotm_wet")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                vars_sim = "ZOO_zoo1", run = FALSE)
  aeme <- cached$aeme
  path <- cached$path
  model_controls <- AEME::get_model_controls()
  model_controls <- AEME::set_vars_sim(model_controls, vars_sim = "ZOO_zoo1")
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
  skip_if_slow()
  model <- c("dy_cd")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                vars_sim = "ZOO_zoo1", run = FALSE)
  aeme <- cached$aeme
  path <- cached$path
  model_controls <- AEME::get_model_controls()
  model_controls <- AEME::set_vars_sim(model_controls, vars_sim = "ZOO_zoo1")

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
  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, file.exists)
  testthat::expect_true(all(file_chk))
})

test_that("running GLM-AED works with bgc_params", {
  skip_if_slow()
  model <- c("glm_aed")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = TRUE,
                                vars_sim = "ZOO_zoo1", run = FALSE)
  aeme <- cached$aeme
  path <- cached$path
  model_controls <- AEME::get_model_controls(use_bgc = TRUE)
  model_controls <- AEME::set_vars_sim(model_controls, vars_sim = "ZOO_zoo1")

  utils::data("glm_aed_parameters", package = "AEME")
  param <- glm_aed_parameters
  param <- param |>
    dplyr::filter(
      grepl("aed_carbon|aed_oxygen|aed_phytoplankton|aed_nitrogen|aed_organic_matter|aed_phosphorus|phyto_data|zoop_params", name),
      !grepl("aed2", file), !grepl("the_phytos", name)
      )

  aeme <- run_aeme_param(aeme = aeme, model = model,
                         param = param, path = path,
                         model_controls = model_controls,
                         na_value = 999, return_aeme = TRUE)

  # AEME::plot_output(aeme, model = "glm_aed", var_sim = "PHY_tchla")
  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, file.exists)
  testthat::expect_true(all(file_chk))
})

test_that("running GOTM-WET works with bgc_params", {
  skip_if_slow()
  model <- c("gotm_wet")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = TRUE,
                                vars_sim = "ZOO_zoo1", run = FALSE)
  aeme <- cached$aeme
  path <- cached$path
  model_controls <- AEME::get_model_controls(use_bgc = TRUE)
  model_controls <- AEME::set_vars_sim(model_controls, vars_sim = "ZOO_zoo1")

  utils::data("gotm_wet_parameters", package = "AEME")
  param <- gotm_wet_parameters |>
    dplyr::filter(grepl("oxygen|phytoplankton|nitrogen|carbon|phytoplankton|zooplankton", module))

  aeme <- run_aeme_param(aeme = aeme,
                         model = model,
                         param = param, path = path,
                         model_controls = model_controls,
                         na_value = 999, return_aeme = TRUE)

  # AEME::plot_output(aeme, model = "gotm_wet")
  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, file.exists)
  testthat::expect_true(all(file_chk))
})

test_that("sensitivity analysis for GOTM-WET works with bgc_params", {
  skip_if_slow()
  model <- c("gotm_wet")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = TRUE,
                                run = FALSE)
  aeme <- cached$aeme
  path <- cached$path
  model_controls <- AEME::get_model_controls(use_bgc = TRUE)

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
  testthat::expect_true(ggplot2::is_ggplot(p1))
})

test_that("run_aeme_param passes the correct sediment temperature and flux values", {
  model <- "glm_aed"
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = TRUE,
                                run = FALSE)
  aeme <- cached$aeme
  path <- cached$path

  row <- function(file, name, value, min, max) {
    data.frame(model = "glm_aed", file = file, name = name, value = value,
               min = min, max = max, group = NA_character_, index = 1L,
               stringsAsFactors = FALSE)
  }
  # GLM sediment temperature (3 zones), AED fluxes (2 zones) and an
  # ordinary scalar parameter
  n_zones <- AEME::get_glm_sed_zones(aeme)
  temp <- zone_ratio_param(row("glm4.nml", "sediment/sed_temp_mean", 20, 5, 30),
                           n_zones = n_zones)
  oxy <- zone_ratio_param(row("aed.nml", "aed_sed_const2d/fsed_oxy",
                              -40, -80, -10), n_zones = n_zones)
  amm <- zone_ratio_param(row("aed.nml", "aed_sed_const2d/fsed_amm", 8, 1, 16),
                          n_zones = n_zones)
  kw <- row("glm4.nml", "light/Kw", 0.4, 0.1, 1)
  kw$index <- NA_integer_
  param <- dplyr::bind_rows(temp, oxy, amm, kw)
  param$value[param$name == "sediment/sed_temp_mean_zratio"] <- 0.75
  param$value[param$name == "aed_sed_const2d/fsed_oxy_zratio"] <- 0.25
  param$value[param$name == "aed_sed_const2d/fsed_amm_zratio"] <- 0.5
  
  e <- expand_zone_ratios(param)
  
  a <- run_aeme_param(aeme = aeme, param = param, model = "glm_aed", 
                      path = path, return_aeme = TRUE)
  
  cfg_files <- AEME::get_model_config_files(a)
  glm <- AEME::read_nml(cfg_files$glm_aed["glm4"])
  testthat::expect_equal(glm$light$Kw, 0.4)
  testthat::expect_equal(glm$sediment$sed_temp_mean, e$value[e$name == "sediment/sed_temp_mean"])
  aed <- AEME::read_nml(cfg_files$glm_aed["aed"])
  testthat::expect_equal(aed$aed_sed_const2d$fsed_oxy, e$value[e$name == "aed_sed_const2d/fsed_oxy"])
  testthat::expect_equal(aed$aed_sed_const2d$fsed_amm, e$value[e$name == "aed_sed_const2d/fsed_amm"])

})

test_that("run_aeme_param passes independent zone values through unchanged", {
  model <- "glm_aed"
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = TRUE,
                                run = FALSE)
  aeme <- cached$aeme
  path <- cached$path

  n_zones <- AEME::get_glm_sed_zones(aeme)
  temp_vals <- seq(12, by = 2, length.out = n_zones)
  oxy_vals <- seq(-40, by = 10, length.out = n_zones)
  param <- rbind(
    data.frame(model = "glm_aed", file = "glm4.nml",
               name = "sediment/sed_temp_mean", value = temp_vals,
               min = 5, max = 30, group = NA_character_,
               index = seq_len(n_zones), stringsAsFactors = FALSE),
    data.frame(model = "glm_aed", file = "aed.nml",
               name = "aed_sed_const2d/fsed_oxy", value = oxy_vals,
               min = -80, max = 0, group = NA_character_,
               index = seq_len(n_zones), stringsAsFactors = FALSE)
  )

  # No zone-ratio rows, so expansion must leave the table as it is
  expect_identical(expand_zone_ratios(param), param)

  a <- run_aeme_param(aeme = aeme, param = param, model = model,
                      path = path, return_aeme = TRUE)

  cfg_files <- AEME::get_model_config_files(a)
  glm <- AEME::read_nml(cfg_files$glm_aed["glm4"])
  testthat::expect_equal(glm$sediment$sed_temp_mean, temp_vals)
  aed <- AEME::read_nml(cfg_files$glm_aed["aed"])
  testthat::expect_equal(aed$aed_sed_const2d$fsed_oxy, oxy_vals)
})

test_that("run_aeme_param passes the correct sediment temperature offsets", {
  model <- "glm_aed"
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = TRUE,
                                run = FALSE)
  aeme <- cached$aeme
  path <- cached$path

  n_zones <- AEME::get_glm_sed_zones(aeme)
  temp <- zone_offset_param(
    data.frame(model = "glm_aed", file = "glm4.nml",
               name = "sediment/sed_temp_mean", value = 10, min = 5,
               max = 25, group = NA_character_, index = 1L,
               stringsAsFactors = FALSE),
    n_zones = n_zones, lower = 0, upper = 5
  )
  # zone 1 = 10, each shallower zone 1.5 degC warmer than the one below
  temp$value[temp$name == "sediment/sed_temp_mean_zoffset"] <- 1.5

  e <- expand_zone_ratios(temp)
  testthat::expect_equal(e$value, 10 + 1.5 * (seq_len(n_zones) - 1))

  a <- run_aeme_param(aeme = aeme, param = temp, model = model,
                      path = path, return_aeme = TRUE)

  cfg_files <- AEME::get_model_config_files(a)
  glm <- AEME::read_nml(cfg_files$glm_aed["glm4"])
  testthat::expect_equal(glm$sediment$sed_temp_mean, e$value)
  # shallower zones are never cooler than deeper ones
  testthat::expect_true(all(diff(glm$sediment$sed_temp_mean) >= 0))
})
