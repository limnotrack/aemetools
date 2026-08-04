# --- Config ---
options(ncore = 2L)

# NOTE: modelled closely on the equivalent dy_cd/gotm_wet tests in
# test-calib_aeme-dyresm.R / test-calib_aeme-gotm.R, since this repo didn't
# previously have any simstrat_aed2 coverage to adapt directly from. Not
# verified against a real Simstrat run in this environment - check these
# actually pass once you can run them against the real model binary.

test_that("can calibrate temperature and lake level for AEME-Simstrat in parallel", {
  model <- c("simstrat_aed2")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, \(f) file.exists(f))
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

test_that("can calibrate lake level only for AEME-Simstrat in parallel", {
  model <- c("simstrat_aed2")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
  lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)
  file_chk <- file.exists(file.path(lake_dir, model, "output", "output.nc"))
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

  plist <- plot_calib(calib = calib)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))
})
