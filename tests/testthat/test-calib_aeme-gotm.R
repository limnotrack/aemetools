# --- Config ---
options(ncore = 2L)

test_that("can calibrate lake level for AEME-GOTM in parallel", {
  model <- c("gotm_wet")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, file.exists)
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

  ctrl <- create_calib_control(NP = 10, itermax = 30, ncore = getOption("ncore"),
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

  param2 <- get_param(calib = calib, best = TRUE)
  best_param <- get_best_params(calib)
  testthat::expect_true(all.equal(param2, best_param))
  param2 <- update_param(calib = calib)
  mod_param <- param |>
    dplyr::filter(model == "gotm_wet")

  testthat::expect_true(is.data.frame(param2))
  testthat::expect_true(!all(param2$value == mod_param$value))
  mod_pars1 <- param |>
    dplyr::filter(model == "gotm_wet")
  mod_pars2 <- param2 |>
    dplyr::filter(model == "gotm_wet") |>
    dplyr::arrange(match(name, mod_pars1$name))

  testthat::expect_true(all(mod_pars2$min >= mod_pars1$min))
  testthat::expect_true(all(mod_pars2$max <= mod_pars1$max))

  best_pars <- get_param(calib = calib, best = TRUE)

  testthat::expect_true(is.data.frame(best_pars))
  testthat::expect_true(all(best_pars$parameter_value %in% param2$value))

  plist <- plot_calib(calib = calib)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))
})

test_that("can calibrate lake level only for AEME-GOTM in parallel", {
  # Not using get_cached_aeme_run(): observations() is mutated before
  # build_aeme() here, and build_aeme()'s behaviour could depend on what
  # observations are present at build time - reusing a cached build made
  # without that mutation could silently produce a different result.
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  obs <- AEME::observations(aeme)
  obs$lake <- NULL
  AEME::observations(aeme) <- obs
  model_controls <- AEME::get_model_controls()
  model <- c("gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           ext_elev = 5, use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model,
                         verbose = FALSE, path = path)
  # AEME::plot(aeme, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, file.exists)
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

  ctrl <- create_calib_control(NP = 10, itermax = 20, ncore = getOption("ncore"),
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

  plist <- plot_calib(calib = calib)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))

})

test_that("can calibrate lake level w/ scaling outflow only for AEME-GOTM in parallel", {
  # Not using get_cached_aeme_run(): observations() is mutated before
  # build_aeme() here - see note in the previous test.
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  obs <- AEME::observations(aeme)
  obs$lake <- NULL
  AEME::observations(aeme) <- obs
  model_controls <- AEME::get_model_controls()
  model <- c("gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           ext_elev = 5, use_bgc = FALSE)

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

  ctrl <- create_calib_control(VTR = -Inf, NP = 10, itermax = 20,
                               reltol = 0.07, cutoff = 0.25, mutate = 0.1,
                               parallel = TRUE, file_type = "csv",
                               na_value = 999, ncore = getOption("ncore"))

  testthat::expect_true(is.list(ctrl))
  testthat::expect_true(!is.null(ctrl$file_name))

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  testthat::expect_true(is.list(calib))

  # param2 <- update_param(param = param, calib = calib)

  plist <- plot_calib(calib = calib)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))

  sim_meta <- read_simulation_meta(ctrl = ctrl)
  testthat::expect_true(is.data.frame(sim_meta))

})

test_that("can calibrate lake level with no data for target time period", {
  # Not using get_cached_aeme_run(): observations() is mutated before
  # build_aeme() here - see note earlier in this file.
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  obs <- AEME::observations(aeme)
  obs$lake <- NULL
  AEME::observations(aeme) <- obs
  model_controls <- AEME::get_model_controls()
  model <- c("gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           ext_elev = 5, use_bgc = FALSE)

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

  ctrl <- create_calib_control(VTR = -Inf, NP = 10, itermax = 20,
                               reltol = 0.07, cutoff = 0.25, mutate = 0.1,
                               parallel = TRUE, file_type = "csv",
                               na_value = 999, ncore = getOption("ncore"))

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  testthat::expect_true(is.list(calib))

  # param2 <- update_param(param = param, calib = calib)

  plist <- plot_calib(calib = calib)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))

  sim_meta <- read_simulation_meta(ctrl = ctrl)
  testthat::expect_true(is.data.frame(sim_meta))

})

test_that("can calibrate temperature with LHC for AEME-GOTM in parallel with csv output", {
  model <- c("gotm_wet")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = FALSE)
  aeme <- cached$aeme
  path <- cached$path

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

  ctrl <- create_calib_control(NP = 10, itermax = 20, ncore = getOption("ncore"),
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

  plist <- plot_calib(calib = calib, fit_col = "LKE_lvlwtr")

  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))

})
