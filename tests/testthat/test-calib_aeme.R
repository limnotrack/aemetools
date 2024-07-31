test_that("can run AEME-GLM with parameters", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  run_aeme_param(aeme = aeme, param = param,
                 model = model, path = path)
  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  nc <- run_aeme_param(aeme = aeme, param = param,
                       model = model, path = path,
                       return_nc = TRUE)
  testthat::expect_true(is(nc, "ncdf4"))

  ncdf4::nc_close(nc)

})

test_that("can run funs return same fit", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model, path = path)
  fit1 <- AEME::assess_model(aeme = aeme, model = model)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  AEME::parameters(aeme) <- param
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model, path = path)
  fit2 <- AEME::assess_model(aeme = aeme, model = model)


  aeme <- run_aeme_param(aeme = aeme, param = param, model = model, path = path,
                         return_aeme = TRUE)
  fit3 <- AEME::assess_model(aeme = aeme, model = model)

  # MAE fun
  # Function to calculate fitness
  mae <- function(df) {
    O <- df$obs
    P <- df$model
    mean(abs(P - O))
  }

  vars_sim <- c("HYD_temp")
  weights <- c("HYD_temp" = 1)
  FUN_list <- list("HYD_temp" = mae)

  testthat::expect_equal(fit2$mae, fit3$mae)
  fit3_df <- AEME::get_var(aeme = aeme, model = model, var_sim = vars_sim, use_obs = T)

  lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)
  fit3_dir <- file.path(lake_dir, paste0(model, "_fit3"))
  dir.create(fit3_dir, showWarnings = FALSE, recursive = TRUE)
  file.copy(file.path(lake_dir, model), fit3_dir, recursive = TRUE)

  fit4 <- run_and_fit(aeme = aeme, param = param,
                     model = model, path = path, FUN_list = FUN_list,
                     vars_sim = vars_sim, weights = weights,
                     return_indices = FALSE,
                     include_wlev = FALSE,
                     fit = TRUE)
  # round(fit3$mae, 1) == round(fit4$HYD_temp, 1)
  testthat::expect_equal(round(fit3$mae, 1), round(fit4$HYD_temp, 1))
  # fit4_df <- run_and_fit(aeme = aeme, param = param,
  #                     model = model, path = path, FUN_list = FUN_list,
  #                     vars_sim = vars_sim, weights = weights,
  #                     return_indices = FALSE,
  #                     include_wlev = FALSE,
  #                     return_df = TRUE)
  # fit4_df <- fit4_df |>
  #   dplyr::arrange(Date, depth_mid)
  # aeme2 <- AEME::load_output(model = model, path = path, aeme = aeme,
  #                            model_controls = model_controls)
  #
  # fit4_df2 <- AEME::get_var(aeme = aeme2, model = model, var_sim = vars_sim,
  #                           use_obs = T)
  # head(fit4_df2)
  # head(fit4_df)
  # head(fit3_df)
  #
  # nc_file1 <- file.path(lake_dir, model, "output", "output.nc")
  # nc_file2 <- file.path(fit3_dir, model, "output", "output.nc")
  #
  # nc1 <- ncdf4::nc_open(nc_file1)
  # nc2 <- ncdf4::nc_open(nc_file2)
  #
  # temp1 <- ncdf4::ncvar_get(nc1, "temp")
  # temp2 <- ncdf4::ncvar_get(nc2, "temp")
  #
  # # Replace NA with 999
  # temp1[is.na(temp1)] <- 999
  # temp2[is.na(temp2)] <- 999
  #
  # all(temp1 == temp2)
  #
  #
  # head(fit4_df)
  # head(fit3_df)
  # comp <- dplyr::left_join(fit3_df, fit4_df, by = c("Date", "depth_mid", "obs")) |>
  #   dplyr::mutate(mod_diff = sim - model)
  # sub <- comp |>
  #   dplyr::filter(#mod_diff < -0.5,
  #                 Date == "2020-11-10")
  # plot(sub$sim, sub$depth_mid)
  # points(sub$model, sub$depth_mid, col = "red")
  #
  # plot(round(comp$sim, 2) - round(comp$model, 2))
  # mean(comp$sim - comp$model)
  # max(comp$sim - comp$model)
  # min(comp$sim - comp$model)
  #
  # library(ggplot2)
  # ggplot() +
  #   geom_point(data = comp, aes(x = obs, y = sim, color = depth_mid)) +
  #   geom_abline(intercept = 0, slope = 1) +
  #   theme_bw()
  #
  #
  # lke <- AEME::lake(aeme)
  # file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
  #                                                tolower(lke$name)),
  #                                   model, "output", "output.nc"))
  # testthat::expect_true(file_chk)
  #
  # nc <- run_aeme_param(aeme = aeme, param = param,
  #                      model = model, path = path,
  #                      return_nc = TRUE)
  # testthat::expect_true(is(nc, "ncdf4"))
  #
  # ncdf4::nc_close(nc)

})

test_that("can calibrate temperature for AEME-DYRESM in parallel", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model,
                         verbose = FALSE, path = path, parallel = FALSE)
  # AEME::plot(aeme, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- create_control(method = "calib", VTR = -Inf, NP = 10, itermax = 30,
                         reltol = 0.07, cutoff = 0.5, mutate = 0.1,
                         parallel = TRUE, file_type = "csv",
                         na_value = 999, ncore = 2)

  testthat::expect_true(is.list(ctrl))

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 10)
  names(weights) <- vars_sim

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id, path = path)

  testthat::expect_true(is.list(calib))

  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})

test_that("can calibrate temperature for AEME-GLM in series with DB output", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed")
  # model <- c("gotm_wet", "glm_aed")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

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

  ctrl <- create_control(method = "calib", NP = 10, itermax = 30, ncore = 2,
                         parallel = FALSE, file_type = "db",
                         file_name = "results.db")

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 0.5)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id, path = path)
  testthat::expect_true("time_elapsed" %in% names(calib$calibration_metadata))
  testthat::expect_true(is.list(calib))

  psum <- plot_calib(calib = calib, fit_col = vars_sim,
                     na_value = ctrl$na_value)

  plist <- plot_calib(calib = calib, fit_col = "LKE_lvlwtr",
                      na_value = ctrl$na_value)

  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})

test_that("can calibrate temperature for AEME-GLM & GOTM in parallel", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model,
                         verbose = FALSE, path = path)
  # AEME::plot(aeme, model = model)
  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(all(file_chk))

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- create_control(method = "calib", NP = 10, itermax = 30, ncore = 2,
                         parallel = TRUE, file_type = "db",
                         file_name = "results.db")

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 10, "LKE_lvlwtr" = 1)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id, path = path)

  testthat::expect_true(is.list(calib))

  plist <- plot_calib(calib = calib, fit_col = "LKE_lvlwtr",
                      na_value = ctrl$na_value)

  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

  sim_meta <- read_simulation_meta(file = ctrl$file_name, path = path)
  testthat::expect_true(is.data.frame(sim_meta))

})

test_that("can calibrate lake level for AEME-GOTM in parallel", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model,
                         verbose = FALSE, path = path)
  # AEME::plot(aeme, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- create_control(method = "calib", NP = 10, itermax = 50, ncore = 2,
                         parallel = TRUE, file_type = "csv")

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id, path = path)

  testthat::expect_true(is.list(calib))

  calib_meta <- read_calib_meta(file = ctrl$file_name, path = path)
  testthat::expect_true(is.data.frame(calib_meta))

  param2 <- update_param(calib = calib)

  testthat::expect_true(is.data.frame(param2))
  testthat::expect_true(!all(param2$value == param$value))
  mod_pars1 <- param |>
    dplyr::filter(model == "gotm_wet")
  mod_pars2 <- param2 |>
    dplyr::filter(model == "gotm_wet")

  testthat::expect_true(all(mod_pars2$min > mod_pars1$min))
  testthat::expect_true(all(mod_pars2$max < mod_pars1$max))

  best_pars <- get_param(calib = calib, na_value = ctrl$na_value, best = TRUE)

  testthat::expect_true(is.data.frame(best_pars))
  testthat::expect_true(all(best_pars$parameter_value %in% param2$value))

  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))
})

test_that("can calibrate lake level only for AEME-DYRESM in parallel", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model,
                         verbose = FALSE, path = path)
  # AEME::plot(aeme, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- create_control(method = "calib", VTR = -Inf, NP = 10, itermax = 30,
                         reltol = 0.07, cutoff = 0.25, mutate = 0.1,
                         parallel = TRUE, file_type = "csv",
                         na_value = 999, ncore = 2)

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id, path = path)

  testthat::expect_true(is.list(calib))

  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})

test_that("can calibrate lake level only for AEME-GLM in parallel", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model,
                         verbose = FALSE, path = path)
  # AEME::plot(aeme, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- create_control(method = "calib", VTR = -Inf, NP = 10, itermax = 30,
                         reltol = 0.07, cutoff = 0.25, mutate = 0.1,
                         parallel = TRUE, file_type = "csv",
                         na_value = 999, ncore = 2)

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id, path = path)

  testthat::expect_true(is.list(calib))

  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})

test_that("can calibrate lake level only for AEME-GOTM in parallel", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  obs <- AEME::observations(aeme)
  obs$lake <- NULL
  AEME::observations(aeme) <- obs
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model,
                         verbose = FALSE, path = path)
  # AEME::plot(aeme, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- create_control(method = "calib", NP = 10, itermax = 30, ncore = 2,
                         file_type = "csv")

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id, path = path)

  testthat::expect_true(is.list(calib))

  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})

test_that("can calibrate lake level w/ scaling outflow only for AEME-DYRESM in parallel", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  obs <- AEME::observations(aeme)
  obs$lake <- NULL
  AEME::observations(aeme) <- obs
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model,
                         verbose = FALSE, path = path)
  # AEME::plot(aeme, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  param <- aeme_parameters[aeme_parameters$name == "outflow", ]

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- create_control(method = "calib", VTR = -Inf, NP = 10, itermax = 30,
                         reltol = 0.07, cutoff = 0.25, mutate = 0.1,
                         parallel = TRUE, file_type = "csv",
                         na_value = 999, ncore = 2L)

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id, path = path)

  testthat::expect_true(is.list(calib))

  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})

test_that("can calibrate lake level w/ scaling outflow and level from wbal only for AEME-GLM in parallel", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  obs <- AEME::observations(aeme)
  obs$lake <- NULL
  obs$level <- NULL
  AEME::observations(aeme) <- obs
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model,
                         verbose = FALSE, path = path)
  # AEME::plot(aeme, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  param <- aeme_parameters[aeme_parameters$name == "outflow", ]

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    mean(abs(O - P))
    # -1 * (cor(x = O, y = P, method = "pearson") -
    #         (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- create_control(method = "calib", VTR = -Inf, NP = 10, itermax = 30,
                         reltol = 0.07, cutoff = 0.25, mutate = 0.1,
                         parallel = TRUE, file_type = "csv",
                         na_value = 999, ncore = 2L)

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  fit <- run_and_fit(aeme = aeme, param = param,
                     model = model, path = path, FUN_list = FUN_list,
                     vars_sim = vars_sim, weights = weights,
                     return_indices = FALSE,
                     include_wlev = TRUE,
                     fit = TRUE)

  testthat::expect_true(fit$LKE_lvlwtr < 0.25)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id, path = path)

  testthat::expect_true(is.list(calib))

  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})

test_that("can calibrate lake level w/ scaling outflow and level from wbal only for AEME-GLM in parallel", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  obs <- AEME::observations(aeme)
  obs$lake <- NULL
  obs$level <- NULL
  AEME::observations(aeme) <- obs
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model,
                         verbose = FALSE, path = path)
  # AEME::plot(aeme, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters |>
    dplyr::filter(model %in% model & grepl("wdr|inf", file)) |>
    dplyr::mutate(min = 0, max = 2.5)

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    mean(abs(O - P))
    # -1 * (cor(x = O, y = P, method = "pearson") -
    #         (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- create_control(method = "calib", VTR = -Inf, NP = 10, itermax = 30,
                         reltol = 0.07, cutoff = 0.25, mutate = 0.1,
                         parallel = TRUE, file_type = "csv",
                         na_value = 999, ncore = 2L)

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  fit <- run_and_fit(aeme = aeme, param = param,
                     model = model, path = path, FUN_list = FUN_list,
                     vars_sim = vars_sim, weights = weights,
                     return_indices = FALSE,
                     include_wlev = TRUE,
                     fit = TRUE)

  testthat::expect_true(fit$LKE_lvlwtr < 0.25)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id, path = path)

  testthat::expect_true(is.list(calib))

  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})

test_that("can calibrate lake level w/ scaling outflow only for AEME-GOTM in parallel", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, file = "aeme.yaml")
  obs <- AEME::observations(aeme)
  obs$lake <- NULL
  AEME::observations(aeme) <- obs
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)

  obs <- AEME::observations(aeme)
  obs$level <- NULL
  AEME::observations(aeme) <- obs
  inp <- AEME::input(aeme)
  inp$hypsograph
  # aeme <- AEME::runaeme = # aeme <- AEME::run_aeme(aeme = aeme, model = model,
  #                        verbose = FALSE, path = path)
  # AEME::plot(aeme, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  lke <- AEME::lake(aeme)
  # file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
  #                                                tolower(lke$name)),
  #                                   model, "output", "output.nc"))
  # testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  param <- aeme_parameters[aeme_parameters$name == "outflow", ]

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    abs(cumsum(P -O))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- create_control(method = "calib", VTR = -Inf, NP = 10, itermax = 30,
                         reltol = 0.07, cutoff = 0.25, mutate = 0.1,
                         parallel = TRUE, file_type = "csv",
                         na_value = 1e20, ncore = 2L)

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id, path = path)
  testthat::expect_true(is.list(calib))

  # param2 <- update_param(param = param, calib = calib, na_value = ctrl$na_value)

  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

  sim_meta <- read_simulation_meta(file = ctrl$file_name, path = path)
  testthat::expect_true(is.data.frame(sim_meta))

})

test_that("can calibrate lake level with no data for target time period", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, file = "aeme.yaml")
  obs <- AEME::observations(aeme)
  obs$lake <- NULL
  AEME::observations(aeme) <- obs
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)

  obs <- AEME::observations(aeme)
  obs$level <- obs$level |>
    dplyr::mutate(Date = Date - 700)
  AEME::observations(aeme) <- obs
  inp <- AEME::input(aeme)
  inp$hypsograph
  # aeme <- AEME::runaeme = # aeme <- AEME::run_aeme(aeme = aeme, model = model,
  #                        verbose = FALSE, path = path)
  # AEME::plot(aeme, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  lke <- AEME::lake(aeme)
  # file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
  #                                                tolower(lke$name)),
  #                                   model, "output", "output.nc"))
  # testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  param <- aeme_parameters[aeme_parameters$name == "outflow", ]

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    abs(cumsum(P -O))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- create_control(method = "calib", VTR = -Inf, NP = 10, itermax = 30,
                         reltol = 0.07, cutoff = 0.25, mutate = 0.1,
                         parallel = TRUE, file_type = "csv",
                         na_value = 1e20, ncore = 2L)

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id, path = path)
  testthat::expect_true(is.list(calib))

  # param2 <- update_param(param = param, calib = calib, na_value = ctrl$na_value)

  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

  sim_meta <- read_simulation_meta(file = ctrl$file_name, path = path)
  testthat::expect_true(is.data.frame(sim_meta))

})


test_that("can calibrate temperature with LHC for AEME-GLM in series with DB output", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed")
  # model <- c("gotm_wet", "glm_aed")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

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

  ctrl <- create_control(method = "calib", NP = 10, itermax = 30, ncore = 2,
                         parallel = FALSE, file_type = "db",
                         file_name = "results.db", c_method = "LHC")

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 0.5)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id, path = path)
  testthat::expect_true(is.list(calib))

  calib_meta <- read_calib_meta(file = ctrl$file_name, path = path)
  testthat::expect_true(is.data.frame(calib_meta))


  psum <- plot_calib(calib = calib, fit_col = vars_sim,
                     na_value = ctrl$na_value)

  plist <- plot_calib(calib = calib, fit_col = "LKE_lvlwtr",
                      na_value = ctrl$na_value)

  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})

test_that("can calibrate temperature with LHC for AEME-GOTM in parallel with csv output", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("gotm_wet")
  # model <- c("gotm_wet", "glm_aed")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

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

  ctrl <- create_control(method = "calib", NP = 10, itermax = 30, ncore = 2,
                         parallel = TRUE, file_type = "csv", c_method = "LHC")

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 0.5)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id, path = path)

  testthat::expect_true("c_method" %in% names(calib$calibration_metadata))
  testthat::expect_true("time_elapsed" %in% names(calib$calibration_metadata))
  testthat::expect_true(is.list(calib))

  psum <- plot_calib(calib = calib, fit_col = vars_sim,
                     na_value = ctrl$na_value)

  plist <- plot_calib(calib = calib, fit_col = "LKE_lvlwtr",
                      na_value = ctrl$na_value)

  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is.ggplot)))

})

test_that("can calibrate HYD_thmcln for AEME-GLM & GOTM in parallel", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)

  # Get parameters for calibration
  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    mean(abs(P - O))
  }
  FUN_list <- list(HYD_thmcln = fit)

  ctrl <- create_control(method = "calib", NP = 10, itermax = 30, ncore = 2,
                         parallel = F, file_type = "db", na_value = 1e20,
                         file_name = "results.db")

  vars_sim <- c("HYD_thmcln")
  weights <- c("HYD_thmcln" = 1)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id, path = path)

  testthat::expect_true(is.list(calib))

  plist <- plot_calib(calib = calib, fit_col = "HYD_thmcln",
                      na_value = ctrl$na_value)

  testthat::expect_true(is.list(plist))

  best_pars <- get_param(calib = calib, na_value = ctrl$na_value, best = TRUE)

  aeme <- update_param(calib = calib, aeme = aeme)
  upd_param <- AEME::parameters(aeme)
  upd_param2 <- update_param(calib = calib)
  testthat::expect_true(all(upd_param$value == upd_param2$value))
  testthat::expect_true(all(upd_param$min == upd_param2$min))
  testthat::expect_true(all(upd_param$max == upd_param2$max))

  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)

  aeme <- AEME::run_aeme(aeme = aeme, path = path, model = model)
  mod_fit <- AEME::assess_model(aeme = aeme, model = model, var_sim = vars_sim)


  testthat::expect_true(all(best_pars$parameter_value %in% upd_param$value))
  # testthat::expect_true(all(best_pars$fit_value %in% mod_fit$mae))

  glm_res <- run_and_fit(aeme = aeme, param = upd_param, model = "glm_aed",
                         vars_sim = vars_sim, path = path, FUN_list = FUN_list,
                         weights = weights, na_value = ctrl$na_value,
                         return_df = TRUE)
  glm_mae <- mean(abs(glm_res$obs - glm_res$model))
  gotm_res <- run_and_fit(aeme = aeme, param = upd_param, model = "gotm_wet",
                         vars_sim = vars_sim, path = path, FUN_list = FUN_list,
                         weights = weights, na_value = ctrl$na_value,
                         return_df = TRUE)
  gotm_mae <- mean(abs(gotm_res$obs - gotm_res$model))

})

test_that("can update bgc parameters for GLM-AED2", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls(use_bgc = TRUE)
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = TRUE)

  # Get parameters for calibration
  utils::data("aeme_parameters", package = "AEME")
  phy_param <- AEME::retrieve_params(model = model, module = "phytoplankton")
  param <- dplyr::bind_rows(aeme_parameters, phy_param)

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    mean(abs(P - O))
  }
  FUN_list <- list(PHY_tchla = fit)

  ctrl <- create_control(method = "calib", NP = 10, itermax = 30, ncore = 2,
                         parallel = TRUE, file_type = "db", na_value = 1e20,
                         file_name = "results.db", c_method = "LHC")

  vars_sim <- c("PHY_tchla")
  weights <- c("PHY_tchla" = 1)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(sim_id = sim_id, path = path, ctrl = ctrl)

  testthat::expect_true(is.list(calib))

  plist <- plot_calib(calib = calib, fit_col = "PHY_tchla",
                      na_value = ctrl$na_value)

  testthat::expect_true(is.list(plist))

  best_pars <- get_param(calib = calib, na_value = ctrl$na_value, best = TRUE)

  aeme <- update_param(calib = calib, aeme = aeme)
  upd_param <- AEME::parameters(aeme)
  upd_param2 <- update_param(calib = calib)
  testthat::expect_true(all(upd_param$value == upd_param2$value))
  testthat::expect_true(all(upd_param$min == upd_param2$min))
  testthat::expect_true(all(upd_param$max == upd_param2$max))

  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = TRUE)

  aeme <- AEME::run_aeme(aeme = aeme, path = path, model = model)
  mod_fit <- AEME::assess_model(aeme = aeme, model = model, var_sim = vars_sim)

  testthat::expect_true(all(best_pars$parameter_value %in% upd_param$value))
})

test_that("can write csv output to database", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model,
                         verbose = FALSE, path = path)
  # AEME::plot(aeme, model = model)
  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(all(file_chk))

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- create_control(method = "calib", NP = 10, itermax = 30, ncore = 2,
                         parallel = TRUE, file_type = "csv")

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 10, "LKE_lvlwtr" = 1)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id, path = path)

  testthat::expect_true(is.list(calib))

  db_file <- write_csv_to_db(path = path)

  testthat::expect_true(file.exists(db_file))



})
