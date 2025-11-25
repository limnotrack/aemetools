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
  
  data("aeme_parameters", package = "AEME")
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
  
  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters
  
  AEME::parameters(aeme) <- param
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = TRUE)
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
    mean(abs(P - O), na.rm = TRUE)
  }
  
  vars_sim <- c("HYD_temp")
  weights <- c("HYD_temp" = 1)
  FUN_list <- list("HYD_temp" = mae)
  
  testthat::expect_equal(fit2$mae, fit3$mae)
  fit3_df <- AEME::get_var(aeme = aeme, model = model, var_sim = vars_sim,
                           use_obs = TRUE)
  
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
  testthat::expect_equal(round(fit3$mae, 3), round(fit4$HYD_temp, 3))
  
  test_vars <- c("HYD_thmcln", "CHM_oxynal", "LKE_tli4")
  v = test_vars[2]
  for (v in test_vars) {
    weights <- c(1)
    names(weights) <- c(v)
    FUN_list <- list()
    FUN_list[[v]] <- mae
    fit_tmp <- run_and_fit(aeme = aeme, param = param,
                           model = model, path = path, FUN_list = FUN_list,
                           vars_sim = c(v), weights = weights,
                           return_indices = FALSE,
                           include_wlev = TRUE,
                           fit = TRUE)
    testthat::expect_true(!is.na(fit_tmp[[v]]))
    testthat::expect_true(!is.na(fit_tmp$LKE_lvlwtr))
  }
  
  weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 1)
  FUN_list$HYD_temp <- mae
  FUN_list$LKE_lvlwtr <- mae
  
  fit5 <- run_and_fit(aeme = aeme, param = param,
                      model = model, path = path, FUN_list = FUN_list,
                      vars_sim = vars_sim, weights = weights,
                      return_indices = FALSE,
                      include_wlev = TRUE,
                      fit = TRUE)
  testthat::expect_equal(round(fit3$mae, 3), round(fit5$HYD_temp, 3))
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
  
  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters
  
  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)
  
  ctrl <- create_control(method = "calib", VTR = -Inf, NP = 10, itermax = 20,
                         reltol = 0.07, cutoff = 0.5, mutate = 0.1,
                         parallel = TRUE, file_type = "csv",
                         na_value = 999, ncore = 2L)
  
  testthat::expect_true(is.list(ctrl))
  
  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 10)
  names(weights) <- vars_sim
  
  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)
  
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  
  testthat::expect_true(is.list(calib))
  
  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))
  
  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))
  
})

test_that("can calibrate temperature for AEME-GLM in series with DB output", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  model <- c("glm_aed")
  # model <- c("gotm_wet", "glm_aed")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)
  
  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters
  ctrl <- create_control(method = "calib", NP = 10, itermax = 20, ncore = 2L,
                         parallel = FALSE, file_type = "db",
                         file_name = "results.db")
  
  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  
  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model, ctrl = ctrl,
                       vars_sim = vars_sim)
  
  # calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  calib <- read_calib(file_name = ctrl$file_name, file_dir = ctrl$file_dir, 
                      file_type = "db", type = "calib", sim_id = sim_id)
  testthat::expect_true("time_elapsed" %in% names(calib$calibration_metadata))
  testthat::expect_true(is.list(calib))
  
  psum <- plot_calib_summary(calib = calib)
  testthat::expect_true(ggplot2::is_ggplot(psum))
  
  plist <- plot_calib(calib = calib, fit_col = "LKE_lvlwtr",
                      na_value = ctrl$na_value)
  
  testthat::expect_true(is.list(plist))
  
  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))
  
})

test_that("can calibrate temperature for AEME-GLM & GOTM in parallel", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  model <- c("glm_aed", "gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           ext_elev = 5, use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model, path = path)
  # AEME::plot(aeme, model = model)
  lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)
  file_chk <- file.exists(file.path(lake_dir, model, "output", "output.nc"))
  testthat::expect_true(all(file_chk))
  
  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters
  
  # Function to calculate fitness
  fit <- function(df) {
    mean(abs(df$obs - df$model))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)
  
  ctrl <- create_control(method = "calib", NP = 10, itermax = 20, ncore = 2,
                         parallel = TRUE, file_type = "db",
                         file_name = "results.db")
  
  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 1)
  
  sim_times <- get_simulation_time(aeme = aeme, model = model, path = path,
                                   param = param, FUN_list = FUN_list,
                                   vars_sim = vars_sim, weights = weights)
  testthat::expect_true(all(sim_times < 3))
  
  out <- run_and_fit(aeme = aeme, param = param,
                     model = model, path = path, FUN_list = FUN_list,
                     vars_sim = vars_sim, weights = weights, 
                     return_indices = F,
                     include_wlev = TRUE,
                     fit = TRUE)
  
  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, model = model, path = path,
                       param = param, FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)
  
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  
  testthat::expect_true(is.list(calib))
  
  plist <- plot_calib(calib = calib, fit_col = "LKE_lvlwtr",
                      na_value = ctrl$na_value)
  
  testthat::expect_true(is.list(plist))
  
  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))
  
  sim_meta <- read_simulation_meta(ctrl = ctrl)
  testthat::expect_true(is.data.frame(sim_meta))
  
  sim_meta2 <- read_simulation_meta(file = ctrl$file_name,
                                    file_dir = ctrl$file_dir)
  testthat::expect_true(is.data.frame(sim_meta2))
  
  calib_meta <- read_calib_meta(file = ctrl$file_name, file_dir = ctrl$file_dir)
  testthat::expect_true(is.data.frame(calib_meta))
  testthat::expect_true(ncol(calib_meta) > ncol(sim_meta))
  
  
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
  
  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters
  
  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)
  
  ctrl <- create_control(method = "calib", NP = 10, itermax = 30, ncore = 2L,
                         parallel = TRUE, file_name = "results.db")
  
  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)
  
  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path, param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl, vars_sim = vars_sim,
                       weights = weights)
  
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  
  testthat::expect_true(is.list(calib))
  
  calib_meta <- read_calib_meta(file = ctrl$file_name, file_dir = ctrl$file_dir)
  testthat::expect_true(is.data.frame(calib_meta))
  
  param2 <- update_param(calib = calib)
  mod_param <- param |> 
    dplyr::filter(model == "gotm_wet")
  
  testthat::expect_true(is.data.frame(param2))
  testthat::expect_true(!all(param2$value == mod_param$value))
  mod_pars1 <- param |>
    dplyr::filter(model == "gotm_wet")
  mod_pars2 <- param2 |>
    dplyr::filter(model == "gotm_wet")
  
  testthat::expect_true(all(mod_pars2$min >= mod_pars1$min))
  testthat::expect_true(all(mod_pars2$max <= mod_pars1$max))
  
  best_pars <- get_param(calib = calib, na_value = ctrl$na_value, best = TRUE)
  
  testthat::expect_true(is.data.frame(best_pars))
  testthat::expect_true(all(best_pars$parameter_value %in% param2$value))
  
  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))
  
  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))
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
  
  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters
  
  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)
  
  ctrl <- create_control(method = "calib", VTR = -Inf, NP = 10, itermax = 20,
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
  
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  
  testthat::expect_true(is.list(calib))
  
  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))
  
  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))
  
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
  
  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters
  
  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)
  
  ctrl <- create_control(method = "calib", VTR = -Inf, NP = 10, itermax = 20,
                         reltol = 0.07, cutoff = 0.25, mutate = 0.1,
                         parallel = TRUE, file_type = "csv",
                         na_value = 999, ncore = 2L)
  
  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)
  
  sim_times <- get_simulation_time(aeme = aeme, model = model, path = path,
                                   param = param, FUN_list = FUN_list,
                                   vars_sim = vars_sim, weights = weights)
  
  testthat::expect_error({
    ctrl$timeout <- 0.01
    # Calibrate AEME model
    sim_id <- calib_aeme(aeme = aeme, path = path,
                         param = param, model = model,
                         FUN_list = FUN_list, ctrl = ctrl,
                         vars_sim = vars_sim, weights = weights)
  })
  
  # Calibrate AEME model
  ctrl$timeout <- 2
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)
  
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  
  testthat::expect_true(is.list(calib))
  
  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))
  
  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))
  
})

test_that("can calibrate sediment parameters only for AEME-GLM", {
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  # path <- tempdir()
  path <- "aeme"
  aeme <- AEME::yaml_to_aeme(path = aeme_dir, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  model <- c("glm_aed")
  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters |> 
    dplyr::filter(module != "sediment")
  sed_param <- AEME::glm_sed_params(n_zones = 3, zone_heights = c(5, 10, 14),
                                    sed_temp_mean = c(12, 14, 18))
  param <- dplyr::bind_rows(param, sed_param)
  
  aeme <- AEME::add_param(aeme = aeme, param = param)
  
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           ext_elev = 5, use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model, path = path)
  AEME::plot_output(aeme)
  lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)
  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(lake_dir, model, "output", "output.nc"))
  testthat::expect_true(file_chk)
  
  
  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)
  
  ctrl <- create_control(method = "calib", VTR = -Inf, NP = 10, itermax = 20,
                         reltol = 0.07, cutoff = 0.25, mutate = 0.1,
                         parallel = F, file_type = "csv",
                         na_value = 999, ncore = 2L)
  
  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 1)
  
  # remove n_zones and zone_heights from calibration
  param <- param |>
    dplyr::filter(!grepl("n_zones|benthic_mode|zone_heights", name))
  tst_param <- param |> 
    dplyr::mutate(
      value = dplyr::case_when(
        name == "sediment/sed_temp_mean" ~ 10,
        TRUE ~ value)
    )
  tst <- run_aeme_param(aeme = aeme, param = param, model = model, path = path)
  
  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path, param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)
  
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  
  testthat::expect_true(is.list(calib))
  
  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))
  
  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))
  
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
  
  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters
  
  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)
  
  ctrl <- create_control(method = "calib", NP = 10, itermax = 20, ncore = 2L,
                         file_type = "csv")
  
  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)
  
  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)
  
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  
  testthat::expect_true(is.list(calib))
  
  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))
  
  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))
  
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
  
  data("aeme_parameters", package = "AEME")
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
  
  ctrl <- create_control(method = "calib", VTR = -Inf, NP = 10, itermax = 20,
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
  
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  
  testthat::expect_true(is.list(calib))
  
  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))
  
  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))
  
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
  
  data("aeme_parameters", package = "AEME")
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
  
  ctrl <- create_control(method = "calib", VTR = -Inf, NP = 10, itermax = 20,
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
  
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  
  testthat::expect_true(is.list(calib))
  
  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))
  
  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))
  
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
  
  data("aeme_parameters", package = "AEME")
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
  
  ctrl <- create_control(method = "calib", VTR = -Inf, NP = 10, itermax = 20,
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
  
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  
  get_param(calib = calib, na_value = ctrl$na_value, best = TRUE)
  
  testthat::expect_true(is.list(calib))
  
  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))
  
  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))
  
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
  
  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters
  
  param <- aeme_parameters[aeme_parameters$name == "outflow", ]
  
  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    abs(cumsum(P -O))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)
  
  ctrl <- create_control(method = "calib", VTR = -Inf, NP = 10, itermax = 20,
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
  
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  testthat::expect_true(is.list(calib))
  
  # param2 <- update_param(param = param, calib = calib, na_value = ctrl$na_value)
  
  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))
  
  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))
  
  sim_meta <- read_simulation_meta(ctrl = ctrl)
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
  
  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters
  
  param <- aeme_parameters[aeme_parameters$name == "outflow", ]
  
  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    abs(cumsum(P -O))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)
  
  ctrl <- create_control(method = "calib", VTR = -Inf, NP = 10, itermax = 20,
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
  
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  testthat::expect_true(is.list(calib))
  
  # param2 <- update_param(param = param, calib = calib, na_value = ctrl$na_value)
  
  plist <- plot_calib(calib = calib, na_value = ctrl$na_value)
  testthat::expect_true(is.list(plist))
  
  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))
  
  sim_meta <- read_simulation_meta(ctrl = ctrl)
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
  
  data("aeme_parameters", package = "AEME")
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
  
  ctrl <- create_control(method = "calib", NP = 10, itermax = 20, ncore = 2L,
                         parallel = FALSE, file_type = "db",
                         file_name = "results.db", c_method = "LHC")
  
  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 0.5)
  
  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)
  
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  testthat::expect_true(is.list(calib))
  
  calib_meta <- read_calib_meta(file = ctrl$file_name, file_dir = ctrl$file_dir)
  testthat::expect_true(is.data.frame(calib_meta))
  
  
  psum <- plot_calib_summary(calib = calib)
  testthat::expect_true(ggplot2::is_ggplot(psum))
  
  plist <- plot_calib(calib = calib, fit_col = "LKE_lvlwtr",
                      na_value = ctrl$na_value)
  
  testthat::expect_true(is.list(plist))
  
  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))
  
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
  
  data("aeme_parameters", package = "AEME")
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
  
  ctrl <- create_control(method = "calib", NP = 10, itermax = 20, ncore = 2L,
                         parallel = TRUE, file_type = "csv", c_method = "LHC")
  
  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 0.5)
  
  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)
  
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  
  testthat::expect_true("c_method" %in% names(calib$calibration_metadata))
  testthat::expect_true("time_elapsed" %in% names(calib$calibration_metadata))
  testthat::expect_true(is.list(calib))
  
  psum <- plot_calib_summary(calib = calib)
  testthat::expect_true(ggplot2::is_ggplot(psum))
  
  plist <- plot_calib(calib = calib, fit_col = "LKE_lvlwtr",
                      na_value = ctrl$na_value)
  
  testthat::expect_true(is.list(plist))
  
  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))
  
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
  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters
  
  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    mean(abs(P - O))
  }
  FUN_list <- list(HYD_thmcln = fit)
  
  ctrl <- create_control(method = "calib", NP = 10, itermax = 20, ncore = 2L,
                         parallel = TRUE, file_type = "db", na_value = 1e20,
                         file_name = "results.db")
  
  vars_sim <- c("HYD_thmcln")
  weights <- c("HYD_thmcln" = 1)
  aeme <- AEME::run_aeme(aeme = aeme, path = path, model = model[1], verbose = T)
  
  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)
  
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  
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
  
  testthat::expect_true(all(best_pars$value %in% upd_param$value))
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)
  
  aeme <- AEME::run_aeme(aeme = aeme, path = path, model = model)
  aeme_temp <- AEME::get_var(aeme = aeme, model = "glm_aed", var = "HYD_temp", use_obs = TRUE)
  mod_fit <- AEME::assess_model(aeme = aeme, model = model, var_sim = vars_sim)
  
  glm_temp <- run_and_fit(aeme = aeme, param = upd_param, model = "glm_aed",
                          vars_sim = "HYD_temp", path = path, FUN_list = FUN_list,
                          weights = weights, na_value = ctrl$na_value,
                          return_df = TRUE) |> 
    dplyr::select(Date, depth_mid, model, obs) |> 
    dplyr::arrange(Date, depth_mid)
  
  glm_sub <- glm_temp |> 
    dplyr::select(-obs)
  
  comp <- aeme_temp |> 
    dplyr::select(Date, depth_mid, sim) |> 
    dplyr::left_join(glm_sub, by = c("Date", "depth_mid")) |> 
    dplyr::mutate(diff = sim - model)
  
  library(ggplot2)
  ggplot() +
    geom_point(data = comp, aes(x = diff, y = model, color = factor(depth_mid))) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "AEME-GLM Simulated", y = "Run and Fit Simulated",
         title = "Comparison of AEME-GLM HYD_temp Simulations") +
    theme_minimal()
  
  aeme_temp$obs == glm_temp$obs
  aeme_temp$sim == glm_temp$model
  diff <- (aeme_temp$sim - glm_temp$model)
  # testthat::expect_true(all(best_pars$fit_value %in% mod_fit$mae))
  
  glm_res <- run_and_fit(aeme = aeme, param = upd_param, model = "glm_aed",
                         vars_sim = vars_sim, path = path, FUN_list = FUN_list,
                         weights = weights, na_value = ctrl$na_value,
                         return_df = TRUE)
  
  glm <- load_output(aeme = aeme, model = "glm_aed", path = path)
  glm_temp <- AEME::get_var(aeme = glm, model = "glm_aed", var = "HYD_temp")
  testthat::expect_true(all(glm_temp$sim == glm_res$model))
  AEME::assess_model(aeme = glm, model = "glm_aed", var_sim = vars_sim)
  
  glm_mae <- mean(abs(glm_res$obs - glm_res$model))
  
  gotm_res <- run_and_fit(aeme = aeme, param = upd_param, model = "gotm_wet",
                          vars_sim = vars_sim, path = path, FUN_list = FUN_list,
                          weights = weights, na_value = ctrl$na_value,
                          return_df = TRUE)
  gotm_mae <- mean(abs(gotm_res$obs - gotm_res$model))
  
  
  
})

test_that("can calibrate HYD_strat for AEME-GLM & GOTM in parallel", {
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
  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters
  
  # Function to calculate fitness
  fit <- function(df) {
    -1 * mean(df$obs == df$model)
  }
  FUN_list <- list(HYD_strat = fit)
  
  ctrl <- create_control(method = "calib", NP = 10, itermax = 20, ncore = 2L,
                         parallel = F, file_type = "db", na_value = 1e20,
                         file_name = "results.db")
  
  vars_sim <- c("HYD_strat")
  weights <- c("HYD_strat" = 1)
  
  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)
  
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  
  testthat::expect_true(is.list(calib))
  
  plist <- plot_calib(calib = calib, fit_col = "HYD_strat",
                      na_value = ctrl$na_value)
  
  testthat::expect_true(is.list(plist))
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
  model <- c("glm_aed", "gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = TRUE)
  
  # Get parameters for calibration
  data("aeme_parameters", package = "AEME")
  phy_param <- AEME::get_aeme_parameters(model = model,
                                         module = "phytoplankton")
  param <- dplyr::bind_rows(aeme_parameters, phy_param)
  
  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    mean(abs(P - O))
  }
  FUN_list <- list(PHY_tchla = fit)
  
  ctrl <- create_control(method = "calib", NP = 10, itermax = 20, ncore = 2L,
                         parallel = TRUE, file_type = "db", na_value = 1e20,
                         file_name = "results.db", c_method = "LHC")
  
  vars_sim <- c("PHY_tchla")
  weights <- c("PHY_tchla" = 1)
  
  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)
  
  calib <- read_calib(sim_id = sim_id, ctrl = ctrl)
  
  testthat::expect_true(is.list(calib))
  
  plist <- plot_calib(calib = calib, fit_col = "PHY_tchla",
                      na_value = ctrl$na_value)
  
  testthat::expect_true(is.list(plist))
  
  best_pars <- get_param(calib = calib, na_value = ctrl$na_value, best = TRUE)
  
  aeme <- update_param(calib = calib, aeme = aeme)
  upd_param <- AEME::parameters(aeme)
  upd_param$param_name <- paste0(upd_param$model, "/", upd_param$group, "/", upd_param$name)
  upd_param2 <- update_param(calib = calib)
  upd_param2$param_name <- paste0(upd_param2$model, "/", upd_param2$group, "/", upd_param2$name)
  upd_param2 <- upd_param2[match(upd_param$param_name, upd_param2$param_name), ]
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
  
  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters
  
  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)
  
  ctrl <- create_control(method = "calib", NP = 10, itermax = 20, ncore = 2L,
                         parallel = TRUE, file_type = "csv")
  
  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 10, "LKE_lvlwtr" = 1)
  
  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)
  
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  
  testthat::expect_true(is.list(calib))
  
  db_file <- write_csv_to_db(file_dir = ctrl$file_dir,
                             file_name = "csv2db.db")
  
  testthat::expect_true(file.exists(db_file))
  
})
