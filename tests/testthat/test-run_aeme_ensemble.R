test_that("can run an ensemble of AEME-GLM & GOTM in parallel", {
  model <- c("glm_aed", "gotm_wet")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = FALSE)
  aeme <- cached$aeme
  path <- cached$path

  data("aeme_parameters", package = "AEME")

  AEME::parameters(aeme) <- aeme_parameters

  # Run ensemble
  aeme <- run_aeme_ensemble(aeme = aeme, model = model, n = 5, path = path,
                            parallel = TRUE, ncore = 2)

  outp <- AEME::output(aeme)
  testthat::expect_true(outp$n_members == 5)

})

test_that("can run an ensemble of AEME-GLM in series", {
  model <- c("glm_aed")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
  # AEME::plot(aeme, model = model)
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

  ctrl <- create_control(method = "calib", NP = 10, itermax = 30, ncore = 2,
                         parallel = TRUE, file_type = "db",
                         file_name = "results_series.db")

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 10, "LKE_lvlwtr" = 1)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  testthat::expect_true(is.list(calib))

  aeme <- update_param(calib = calib, aeme = aeme)

  # Run ensemble
  aeme <- run_aeme_ensemble(aeme = aeme, model = model, n = 5, path = path,
                            parallel = FALSE)

  outp <- AEME::output(aeme)
  testthat::expect_true(outp$n_members == 5)
})

test_that("can run an ensemble of AEME-GOTM-WET in parallel and plot", {
  model <- c("gotm_wet")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = FALSE)
  aeme <- cached$aeme
  path <- cached$path

  data("aeme_parameters", package = "AEME")
  aeme_parameters <- aeme_parameters |>
    dplyr::mutate(min = value - 0.1 * value,
                  max = value + 0.1 * value)

  AEME::parameters(aeme) <- aeme_parameters

  # Run ensemble
  aeme <- run_aeme_ensemble(aeme = aeme, model = model, n = 5, path = path,
                            parallel = TRUE, ncore = 2)

  outp <- AEME::output(aeme)
  testthat::expect_true(outp$n_members == 5)

  p <- plot_ensemble(aeme = aeme, model = model, depth = 0)

  testthat::expect_true(ggplot2::is_ggplot(p))

  p2 <- plot_ensemble(aeme = aeme, model = model, depth = 5, type = "line")
  testthat::expect_true(ggplot2::is_ggplot(p2))

})
