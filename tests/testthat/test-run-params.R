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
  # param <- dplyr::bind_rows(
  #   # aeme_parameters_bgc,
  #   glm_aed_parameters
  # ) |>
  #   dplyr::filter(model == "glm_aed")
  run_aeme_shiny(aeme_data = aeme_data, param = param, path = path,
                 mod_ctrls = mod_ctrls)

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

