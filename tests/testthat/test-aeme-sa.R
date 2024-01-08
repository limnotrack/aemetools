test_that("can calibrate temperature for AEME-DYRESM in parallel", {

  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd")
  aeme_data <- AEME::build_ensemble(path = path, aeme_data = aeme_data,
                                    model = model, mod_ctrls = mod_ctrls,
                                    inf_factor = inf_factor, ext_elev = 5,
                                    use_bgc = FALSE)

  aeme_data <- AEME::run_aeme(aeme_data = aeme_data, model = model,
                              verbose = FALSE, mod_ctrls = mod_ctrls,
                              path = path)

  # AEME::plot(aeme_data, model = model)
  lke <- AEME::lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "aemetools")

  # Function to calculate fitness
  fit <- function(df) {
    mean(df$model)
  }

  FUN_list <- list(HYD_temp = fit)


  ctrl <- list(N = 10, ncore = 2L, na_value = 999, parallel = TRUE,
               out_file = "results.csv",
               HYD_temp = list(month = c(10:12, 1:3),
                               depths = c(0, 2)
               )
  )

  vars_sim <- c("HYD_temp")
  weights <- c("HYD_temp" = 1)

  # Run sensitivity analysis AEME model
  ctrl <- sa_aeme(aeme_data = aeme_data, path = path, param = aeme_parameters,
                  model = model, ctrl = ctrl, mod_ctrls = mod_ctrls,
                  vars_sim = vars_sim, FUN_list = FUN_list, weights = weights)

  calib_res <- read_calib(ctrl = ctrl, model = model, path = path)

  testthat::expect_true(is.data.frame(calib_res))
})

test_that("can calibrate temperature for AEME-GLM in parallel", {

  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  mod_ctrls <- read.csv(file.path(path, "model_controls.csv"))
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed")
  aeme_data <- AEME::build_ensemble(path = path, aeme_data = aeme_data,
                                    model = model, mod_ctrls = mod_ctrls,
                                    inf_factor = inf_factor, ext_elev = 5,
                                    use_bgc = FALSE)

  aeme_data <- AEME::run_aeme(aeme_data = aeme_data, model = model,
                              verbose = FALSE, mod_ctrls = mod_ctrls,
                              path = path)

  lke <- AEME::lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "aemetools")

  # Function to calculate fitness
  fit <- function(df) {
    mean(df$model)
  }

  FUN_list <- list(HYD_temp = fit)

  ctrl <- list(N = 10, ncore = 2L, na_value = 999, parallel = F,
               out_file = "results.csv",
               HYD_temp = list(month = c(10:12, 1:3),
                               depths = c(0, 2)
               )
  )

  vars_sim <- c("HYD_temp")
  weights <- c("HYD_temp" = 1)

  # Run sensitivity analysis AEME model
  ctrl <- sa_aeme(aeme_data = aeme_data, path = path, param = aeme_parameters,
                  model = model, ctrl = ctrl, mod_ctrls = mod_ctrls,
                  vars_sim = vars_sim, FUN_list = FUN_list, weights = weights)

  calib_res <- read_calib(ctrl = ctrl, model = model, path = path)

  testthat::expect_true(is.data.frame(calib_res))
})

test_that("can calibrate temperature for AEME-GOTM in parallel", {

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
                                    use_bgc = FALSE)

  aeme_data <- AEME::run_aeme(aeme_data = aeme_data, model = model,
                              verbose = FALSE, mod_ctrls = mod_ctrls,
                              path = path)

  lke <- AEME::lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "aemetools")

  # Function to calculate fitness
  fit <- function(df) {
    mean(df$model)
  }

  FUN_list <- list(HYD_temp = fit)


  ctrl <- list(N = 10, ncore = 2L, na_value = 999, parallel = TRUE,
               out_file = "results.csv",
               HYD_temp = list(month = c(10:12, 1:3),
                               depths = c(0, 2)
               )
  )

  vars_sim <- c("HYD_temp")
  weights <- c("HYD_temp" = 1)

  # Run sensitivity analysis AEME model
  ctrl <- sa_aeme(aeme_data = aeme_data, path = path, param = aeme_parameters,
                  model = model, ctrl = ctrl, mod_ctrls = mod_ctrls,
                  vars_sim = vars_sim, FUN_list = FUN_list, weights = weights)

  calib_res <- read_calib(ctrl = ctrl, model = model, path = path)

  testthat::expect_true(is.data.frame(calib_res))
})
