# --- Config ---
options(ncore = 2L)

test_that("can calibrate temperature for AEME-GLM & GOTM in parallel", {
  model <- c("glm_aed", "gotm_wet")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
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

  ctrl <- create_calib_control(NP = 10, itermax = 20, ncore = 2,
                               parallel = TRUE, file_type = "db",
                               file_name = "results.db")

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 1)

  sim_times <- get_simulation_time(aeme = aeme, model = model, path = path,
                                   param = param, FUN_list = FUN_list,
                                   vars_sim = vars_sim, weights = weights)
  testthat::expect_lt(sim_times[1], 3)
  testthat::expect_lt(sim_times[2], 3)

  # out <- run_and_fit(aeme = aeme, param = param,
  #                    model = model, path = path, FUN_list = FUN_list,
  #                    vars_sim = vars_sim, weights = weights,
  #                    return_indices = F,
  #                    include_wlev = TRUE,
  #                    fit = TRUE)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, model = model, path = path,
                       param = param, FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)

  testthat::expect_true(is.list(calib))

  plist <- plot_calib(calib = calib, fit_col = "LKE_lvlwtr")

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

test_that("can return NA if timeout is too low", {
  model <- c("glm_aed", "gotm_wet")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
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

  ctrl <- create_calib_control(NP = 10, itermax = 20, ncore = 2,
                               parallel = FALSE, file_type = "db",
                               file_name = "results.db")

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 1)

  sim_times <- get_simulation_time(aeme = aeme, model = model, path = path,
                                   param = param, FUN_list = FUN_list,
                                   vars_sim = vars_sim, weights = weights)
  ctrl$timeout <- 0.1
  sim_id <- calib_aeme(aeme = aeme, model = model, path = path,
                       param = param, FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

})

test_that("can calibrate derived vars for AEME-GLM & GOTM in parallel", {
  # Not using get_cached_aeme_run(): model_controls is customised via
  # AEME::set_vars_sim() before building, and the cache key doesn't
  # currently distinguish custom model_controls from the default, so
  # sharing the cache here risks a stale/mismatched cache hit.
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  path <- tempdir()
  aeme <- AEME::yaml_to_aeme(path = aeme_dir, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  vars_sim <- c("HYD_thmcln", "HYD_strat", "HYD_schstb")
  model_controls <- AEME::set_vars_sim(model_controls, vars_sim = vars_sim)
  model <- c("glm_aed", "gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           ext_elev = 5, use_bgc = FALSE)

  # Get parameters for calibration
  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  # Update Sed params
  sed_param <- AEME::get_glm_sed_params(aeme = aeme) |>
    dplyr::mutate(
      min = dplyr::case_when(
        grepl("sed_temp_mean", name) ~ 8,
        .default = min
      ),
      max = dplyr::case_when(
        grepl("sed_temp_mean", name) ~ 22,
        .default = max
      )
    )

  param <- param |>
    dplyr::filter(!(name %in% sed_param$name)) |>
    dplyr::bind_rows(sed_param)

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    mean(abs(P - O))
  }
  FUN_list <- list(HYD_thmcln = fit, HYD_strat = fit, HYD_schstb = fit)

  ctrl <- create_calib_control(NP = 10, itermax = 100,
                               ncore = getOption("ncore"),
                               parallel = TRUE, file_type = "db",
                               na_value = 999, file_name = "results.db",
                               cutoff = 0.5, cutoff_final = 0.1,
                               mutate = 0.1, mutate_final = 0.3)

  aeme <- AEME::run_aeme(aeme = aeme)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)

  testthat::expect_true(is.list(calib))

  plist <- plot_calib(calib = calib, fit_col = "HYD_thmcln")

  testthat::expect_true(is.list(plist))
  testthat::expect_true(ggplot2::is_ggplot(plist[[1]]))

  best_pars <- get_best_params(calib = calib)
  best_pars2 <- get_param(calib = calib,
                          fit_col = "fit", quantile = 0.1, best = TRUE)
  testthat::expect_true(all(best_pars$value %in% best_pars2$value))

  aeme <- update_param(calib = calib, aeme = aeme)
  upd_param <- AEME::parameters(aeme)
  upd_param2 <- update_param(calib = calib)
  testthat::expect_true(all(upd_param$value %in% upd_param2$value))
  testthat::expect_true(all(upd_param$min %in% upd_param2$min))
  testthat::expect_true(all(upd_param$max %in% upd_param2$max))
  # best_pars[!best_pars$value %in% upd_param$value, ]
  testthat::expect_true(all(best_pars$value %in% upd_param$value))
  aeme <- AEME::build_aeme(aeme = aeme) |>
    AEME::run_aeme()
  aeme_temp <- AEME::get_var(aeme = aeme, model = "glm_aed", var = "HYD_temp",
                             use_obs = TRUE)
  mod_fit <- AEME::assess_model(aeme = aeme, model = model, var_sim = vars_sim)

})

test_that("can calibrate HYD_strat for AEME-GLM & GOTM in parallel", {
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                inf_factor = inf_factor, run = FALSE)
  aeme <- cached$aeme
  path <- cached$path

  # Get parameters for calibration
  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  # Function to calculate fitness
  fit <- function(df) {
    -1 * mean(df$obs == df$model)
  }
  FUN_list <- list(HYD_strat = fit)

  ctrl <- create_calib_control(NP = 10, itermax = 20, ncore = getOption("ncore"),
                               parallel = F, file_type = "db", na_value = 999,
                               file_name = "results.db")

  vars_sim <- c("HYD_strat")
  weights <- set_weights(vars_sim = vars_sim)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)

  testthat::expect_true(is.list(calib))

  plist <- plot_calib(calib = calib, fit_col = "HYD_strat")

  testthat::expect_true(is.list(plist))
})
