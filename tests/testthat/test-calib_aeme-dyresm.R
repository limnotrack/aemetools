# --- Config ---
options(ncore = 2L)

test_that("can calibrate temperature for AEME-DYRESM in parallel", {
  model <- c("dy_cd")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
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

  ctrl <- create_calib_control(VTR = -Inf, NP = 10, itermax = 20,
                               reltol = 0.07, cutoff = 0.5, mutate = 0.1,
                               parallel = TRUE, file_type = "csv",
                               na_value = 999, ncore = getOption("ncore"))

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

  plist <- plot_calib(calib = calib)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))

})

test_that("can calibrate lake level only for AEME-DYRESM in parallel", {
  model <- c("dy_cd")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
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

  plist <- plot_calib(calib = calib)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))

})

test_that("can calibrate lake level w/ scaling outflow only for AEME-DYRESM in parallel", {
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
  model <- c("dy_cd")
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

  param <- aeme_parameters[aeme_parameters$name == "outflow", ]

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
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

  plist <- plot_calib(calib = calib)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))

})
