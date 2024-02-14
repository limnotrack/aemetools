test_that("running GLM-AED works with bgc_params", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  # unlink(tmpdir, recursive = TRUE)
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  mod_ctrls <- mod_ctrls |>
    dplyr::mutate(simulate = dplyr::case_when(
      name == "ZOO_zoo1" ~ 1,
      .default = simulate
    ))
  model <- c("glm_aed")
  aeme_data <- AEME::build_ensemble(path = path, aeme_data = aeme_data,
                                    model = model, mod_ctrls = mod_ctrls,
                                    ext_elev = 5, use_bgc = TRUE)

  # utils::data("aeme_parameters_bgc", package = "aemetools")
  utils::data("glm_aed_parameters", package = "aemetools")
  param <- glm_aed_parameters
  # param <- dplyr::bind_rows(
  #   # aeme_parameters_bgc,
  #   glm_aed_parameters
  # ) |>
  #   dplyr::filter(model == "glm_aed")
  # run_aeme_shiny(aeme_data = aeme_data, param = param, path = path,
  #                mod_ctrls = mod_ctrls)

  aeme_data <- run_aeme_param(aeme_data = aeme_data,
                              model = model,
                              param = param, path = path,
                              mod_ctrls = mod_ctrls,
                              na_value = 999, return_aeme = TRUE)

  # AEME::plot_output(aeme_data, model = "glm_aed", var_sim = "PHY_tchla")
  lke <- AEME::lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("running GOTM-WET works with bgc_params", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  # unlink(tmpdir, recursive = TRUE)
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  mod_ctrls <- mod_ctrls |>
    dplyr::mutate(simulate = dplyr::case_when(
      name == "ZOO_zoo1" ~ 1,
      .default = simulate
    ))
  model <- c("gotm_wet")
  aeme_data <- AEME::build_ensemble(path = path, aeme_data = aeme_data, model = model,
                                    mod_ctrls = mod_ctrls,
                                    ext_elev = 5, use_bgc = TRUE)

  utils::data("gotm_wet_parameters", package = "aemetools")
  param <- gotm_wet_parameters


  aeme_data <- run_aeme_param(aeme_data = aeme_data,
                              model = model,
                              param = param, path = path,
                              mod_ctrls = mod_ctrls,
                              na_value = 999, return_aeme = TRUE)

  # AEME::plot_output(aeme_data, model = "gotm_wet")
  lke <- AEME::lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)
})

test_that("sensitivity analysis for GOTM-WET works with bgc_params", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("gotm_wet")
  aeme_data <- AEME::build_ensemble(path = path, aeme_data = aeme_data,
                                    model = model, mod_ctrls = mod_ctrls,
                                    inf_factor = inf_factor, ext_elev = 5,
                                    use_bgc = TRUE)

  utils::data("gotm_wet_parameters", package = "aemetools")
  param <- gotm_wet_parameters |>
    dplyr::filter(module %in% c("oxygen", "phytoplankton"))

  # Function to calculate fitness
  fit <- function(df) {
    mean(df$model)
  }

  FUN_list <- list(HYD_temp = fit, PHY_tchla = fit)

  ctrl <- create_control(method = "sa", N = 2^1, ncore = 14, na_value = 999,
                         parallel = TRUE, out_file = "results.db",
                         vars_sim = list(
                           surf_temp = list(var = "HYD_temp",
                                            month = c(10:12, 1:3),
                                            depth_range = c(0, 2)
                           ),
                           bot_temp = list(var = "HYD_temp",
                                           month = c(10:12, 1:3),
                                           depth_range = c(10, 13)
                           ),
                           PHY_tchla = list(var = "PHY_tchla",
                                            month = c(10:12, 1:3),
                                            depth_range = c(0, 2)
                           )
                         )
  )

  # Run sensitivity analysis AEME model
  sim_id <- sa_aeme(aeme_data = aeme_data, path = path, param = param,
                    model = model, ctrl = ctrl, mod_ctrls = mod_ctrls,
                    FUN_list = FUN_list)

  sa_res <- read_sa(ctrl = ctrl, sim_id = sim_id, boot = FALSE)

  testthat::expect_true(is.data.frame(sa_res[[1]]$df))

  p1 <- plot_uncertainty(sa = sa_res)
  testthat::expect_true(ggplot2::is.ggplot(p1))
})

