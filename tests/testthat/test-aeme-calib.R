test_that("can run AEME-GLM with parameters", {
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

  utils::data("aeme_parameters", package = "aemetools")

  run_aeme_param(aeme_data = aeme_data, param = aeme_parameters,
                       model = model, path = path, mod_ctrls = mod_ctrls)
  lke <- AEME::lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  nc <- run_aeme_param(aeme_data = aeme_data, param = aeme_parameters,
                 model = model, path = path, mod_ctrls = mod_ctrls,
                 return_nc = TRUE)
  testthat::expect_true(is(nc, "ncdf4"))

  ncdf4::nc_close(nc)

})

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
                              path = path, parallel = FALSE)
  # AEME::plot(aeme_data, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  lke <- AEME::lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "aemetools")

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- list(VTR = -Inf, NP = 10, itermax = 30, reltol = 0.07, cutoff = 0.5,
               mutate = 0.1, parallel = TRUE, out_file = "results.csv",
               na_value = 999, ncore = 2L)

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 10)
  names(weights) <- vars_sim

  # Calibrate AEME model
  ctrl <- calib_aeme(aeme_data = aeme_data, path = path,
                     param = aeme_parameters, model = model,
                     mod_ctrls = mod_ctrls, FUN_list = FUN_list, ctrl = ctrl,
                     vars_sim = vars_sim, weights = weights)

  calib_res <- read_calib(ctrl = ctrl, model = model, path = path)

  testthat::expect_true(is.data.frame(calib_res))

  plist <- plot_calib(calib = calib_res, model = model,
                      na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})

test_that("can calibrate temperature for AEME-GLM in series with DB output", {
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

  utils::data("aeme_parameters", package = "aemetools")

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  mae <- function(df) {
    mean(abs(df$obs - df$model))
  }

  FUN_list <- list(HYD_temp = mae, LKE_lvlwtr = fit)

  ctrl <- list(VTR = -Inf, NP = 15, itermax = 45, reltol = 0.07, cutoff = 0.25,
               mutate = 0.1, parallel = FALSE, out_file = "results.db",
               na_value = 999, ncore = 2)

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 1)

  # Calibrate AEME model
  ctrl <- calib_aeme(aeme_data = aeme_data, path = path,
                     param = aeme_parameters, model = model,
                     mod_ctrls = mod_ctrls, FUN_list = FUN_list, ctrl = ctrl,
                     vars_sim = vars_sim, weights = weights)

  calib_res <- read_calib(ctrl = ctrl, model = model, path = path)

  testthat::expect_true(is.data.frame(calib_res))

  plist <- plot_calib(calib = calib_res, model = model, fit_col = "LKE_lvlwtr",
                      na_value = ctrl$na_value)

  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

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
  # AEME::plot(aeme_data, model = model)
  lke <- AEME::lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "aemetools")

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- list(VTR = -Inf, NP = 15, itermax = 45, reltol = 0.07, cutoff = 0.25,
               mutate = 0.1, parallel = TRUE, out_file = "results.csv",
               na_value = 999, ncore = 2)

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 10, "LKE_lvlwtr" = 1)

  # Calibrate AEME model
  ctrl <- calib_aeme(aeme_data = aeme_data, path = path,
                     param = aeme_parameters, model = model,
                     mod_ctrls = mod_ctrls, FUN_list = FUN_list, ctrl = ctrl,
                     vars_sim = vars_sim, weights = weights)

  calib_res <- read_calib(ctrl = ctrl, model = model)

  testthat::expect_true(is.data.frame(calib_res))

  plist <- plot_calib(calib = calib_res, model = model, fit_col = "LKE_lvlwtr",
                      na_value = ctrl$na_value)

  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

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
  # AEME::plot(aeme_data, model = model)
  lke <- AEME::lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "aemetools")

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- list(VTR = -Inf, NP = 10, itermax = 30, reltol = 0.07, cutoff = 0.5,
               mutate = 0.1, parallel = TRUE, out_file = "results.csv",
               na_value = 999, ncore = 2L)

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c(1, 10)
  names(weights) <- vars_sim

  # Calibrate AEME model
  ctrl <- calib_aeme(aeme_data = aeme_data, path = path,
                     param = aeme_parameters, model = model,
                     mod_ctrls = mod_ctrls, FUN_list = FUN_list, ctrl = ctrl,
                     vars_sim = vars_sim, weights = weights)

  calib_res <- read_calib(ctrl = ctrl, model = model, path = path)

  testthat::expect_true(is.data.frame(calib_res))

  plist <- plot_calib(calib = calib_res, model = model,
                      na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})

test_that("can calibrate lake level for AEME-GOTM in parallel", {
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
  # AEME::plot(aeme_data, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  lke <- AEME::lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "aemetools")

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- list(VTR = -Inf, NP = 10, itermax = 30, reltol = 0.07, cutoff = 0.5,
               mutate = 0.1, parallel = TRUE, out_file = "results.csv",
               na_value = 999, ncore = 2L)

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  # Calibrate AEME model
  ctrl <- calib_aeme(aeme_data = aeme_data, path = path,
                     param = aeme_parameters, model = model,
                     mod_ctrls = mod_ctrls, FUN_list = FUN_list, ctrl = ctrl,
                     vars_sim = vars_sim, weights = weights)

  calib_res <- read_calib(ctrl = ctrl, model = model, path = path)

  testthat::expect_true(is.data.frame(calib_res))

  plist <- plot_calib(calib = calib_res, model = model,
                      na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})

test_that("can calibrate lake level only for AEME-DYRESM in parallel", {
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
  # AEME::plot(aeme_data, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  lke <- AEME::lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "aemetools")

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- list(VTR = -Inf, NP = 10, itermax = 30, reltol = 0.07, cutoff = 0.5,
               mutate = 0.1, parallel = TRUE, out_file = "results.csv",
               na_value = 999, ncore = 2L)

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  # Calibrate AEME model
  ctrl <- calib_aeme(aeme_data = aeme_data, path = path,
                     param = aeme_parameters, model = model,
                     mod_ctrls = mod_ctrls, FUN_list = FUN_list, ctrl = ctrl,
                     vars_sim = vars_sim, weights = weights)

  calib_res <- read_calib(ctrl = ctrl, model = model, path = path)

  testthat::expect_true(is.data.frame(calib_res))

  plist <- plot_calib(calib = calib_res, model = model,
                      na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})

test_that("can calibrate lake level only for AEME-GLM in parallel", {
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
  # AEME::plot(aeme_data, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  lke <- AEME::lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "aemetools")

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- list(VTR = -Inf, NP = 10, itermax = 30, reltol = 0.07, cutoff = 0.5,
               mutate = 0.1, parallel = TRUE, out_file = "results.csv",
               na_value = 999, ncore = 2L)

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  # Calibrate AEME model
  ctrl <- calib_aeme(aeme_data = aeme_data, path = path,
                     param = aeme_parameters, model = model,
                     mod_ctrls = mod_ctrls, FUN_list = FUN_list, ctrl = ctrl,
                     vars_sim = vars_sim, weights = weights)

  calib_res <- read_calib(ctrl = ctrl, model = model, path = path)

  testthat::expect_true(is.data.frame(calib_res))

  plist <- plot_calib(calib = calib_res, model = model,
                      na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})


test_that("can calibrate lake level only for AEME-GOTM in parallel", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  obs <- AEME::observations(aeme_data)
  obs$lake <- NULL
  AEME::observations(aeme_data) <- obs
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
  # AEME::plot(aeme_data, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  lke <- AEME::lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "aemetools")

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- list(VTR = -Inf, NP = 10, itermax = 30, reltol = 0.07, cutoff = 0.5,
               mutate = 0.1, parallel = TRUE, out_file = "results.csv",
               na_value = 999, ncore = 2L)

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  # Calibrate AEME model
  ctrl <- calib_aeme(aeme_data = aeme_data, path = path,
                     param = aeme_parameters, model = model,
                     mod_ctrls = mod_ctrls, FUN_list = FUN_list, ctrl = ctrl,
                     vars_sim = vars_sim, weights = weights)

  calib_res <- read_calib(ctrl = ctrl, model = model, path = path)

  testthat::expect_true(is.data.frame(calib_res))

  plist <- plot_calib(calib = calib_res, model = model,
                      na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})

test_that("can calibrate lake level w/ scaling outflow only for AEME-DYRESM in parallel", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  obs <- AEME::observations(aeme_data)
  obs$lake <- NULL
  AEME::observations(aeme_data) <- obs
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
  # AEME::plot(aeme_data, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  lke <- AEME::lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "aemetools")

  param <- aeme_parameters[aeme_parameters$name == "outflow", ]

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- list(VTR = -Inf, NP = 10, itermax = 30, reltol = 0.07, cutoff = 0.5,
               mutate = 0.1, parallel = TRUE, out_file = "results.csv",
               na_value = 999, ncore = 2L)

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  # Calibrate AEME model
  ctrl <- calib_aeme(aeme_data = aeme_data, path = path,
                     param = param, model = model,
                     mod_ctrls = mod_ctrls, FUN_list = FUN_list, ctrl = ctrl,
                     vars_sim = vars_sim, weights = weights)

  calib_res <- read_calib(ctrl = ctrl, model = model, path = path)

  testthat::expect_true(is.data.frame(calib_res))

  plist <- plot_calib(calib = calib_res, model = model,
                      na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})

test_that("can calibrate lake level w/ scaling outflow only for AEME-GLM in parallel", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  obs <- AEME::observations(aeme_data)
  obs$lake <- NULL
  AEME::observations(aeme_data) <- obs
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
  # AEME::plot(aeme_data, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  lke <- AEME::lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "aemetools")

  param <- aeme_parameters[aeme_parameters$name == "outflow", ]

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- list(VTR = -Inf, NP = 10, itermax = 30, reltol = 0.07, cutoff = 0.5,
               mutate = 0.1, parallel = F, out_file = "results.csv",
               na_value = 999, ncore = 2L)

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  # Calibrate AEME model
  ctrl <- calib_aeme(aeme_data = aeme_data, path = path,
                     param = param, model = model,
                     mod_ctrls = mod_ctrls, FUN_list = FUN_list, ctrl = ctrl,
                     vars_sim = vars_sim, weights = weights)

  calib_res <- read_calib(ctrl = ctrl, model = model, path = path)

  testthat::expect_true(is.data.frame(calib_res))

  plist <- plot_calib(calib = calib_res, model = model,
                      na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})

test_that("can calibrate lake level w/ scaling outflow only for AEME-GOTM in parallel", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme_data <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  obs <- AEME::observations(aeme_data)
  obs$lake <- NULL
  AEME::observations(aeme_data) <- obs
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
  # AEME::plot(aeme_data, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  lke <- AEME::lake(aeme_data)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "aemetools")

  param <- aeme_parameters[aeme_parameters$name == "outflow", ]

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- list(VTR = -Inf, NP = 10, itermax = 30, reltol = 0.07, cutoff = 0.5,
               mutate = 0.1, parallel = TRUE, out_file = "results.csv",
               na_value = 999, ncore = 2L)

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  # Calibrate AEME model
  ctrl <- calib_aeme(aeme_data = aeme_data, path = path,
                     param = param, model = model,
                     mod_ctrls = mod_ctrls, FUN_list = FUN_list, ctrl = ctrl,
                     vars_sim = vars_sim, weights = weights)

  calib_res <- read_calib(ctrl = ctrl, model = model, path = path)

  testthat::expect_true(is.data.frame(calib_res))

  plist <- plot_calib(calib = calib_res, model = model,
                      na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})

