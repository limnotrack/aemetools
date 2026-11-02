# The built-in engines save each generation's parameters rounded to six
# significant digits. Candidates are rounded the same way *before* they are
# scored, so a saved parameter set reproduces the fit recorded for it.

test_that("round_candidates rounds parameter columns to 6 significant digits", {
  param <- data.frame(name_full = c("a", "b"), min = c(0, 0), max = c(10, 1))
  df <- data.frame(a = c(1.23456789, 9.87654321), b = c(0.123456789, 0.987654321),
                   fit = c(1.23456789, 2.3456789))
  out <- aemetools:::round_candidates(df, param)
  expect_equal(out$a, c(1.23457, 9.87654))
  expect_equal(out$b, c(0.123457, 0.987654))
  # non-parameter columns are left alone
  expect_identical(out$fit, df$fit)
})

test_that("round_candidates is idempotent", {
  param <- data.frame(name_full = "a", min = 0, max = 10)
  df <- data.frame(a = c(1.23456789, 9.87654321))
  once <- aemetools:::round_candidates(df, param)
  expect_identical(aemetools:::round_candidates(once, param), once)
})

test_that("round_candidates keeps values inside [min, max]", {
  # 0.9999996 rounds up to 1 at 6 s.f.; a max just below must still hold
  param <- data.frame(name_full = "a", min = 0, max = 0.9999996)
  df <- data.frame(a = c(0.99999955, 0.5))
  out <- aemetools:::round_candidates(df, param)
  expect_true(all(out$a <= 0.9999996))
  expect_true(all(out$a >= 0))
})

test_that("round_candidates ignores columns that are not parameters", {
  param <- data.frame(name_full = "a", min = 0, max = 10)
  df <- data.frame(a = 1.23456789, gen = 3, other = 7.7777777777)
  out <- aemetools:::round_candidates(df, param)
  expect_equal(out$gen, 3)
  expect_equal(out$other, 7.7777777777)
})

test_that("a saved parameter set reproduces the fit recorded for it", {
  skip_if_slow()
  cached <- get_cached_aeme_run(model = "glm_aed", ext_elev = 5,
                                use_bgc = FALSE, run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
  data("aeme_parameters", package = "AEME")

  mae <- function(df) mean(abs(df$obs - df$model))
  FUN_list <- list(HYD_temp = mae)
  weights <- c("HYD_temp" = 1)
  ctrl <- create_calib_control(NP = 8, itermax = 16, parallel = FALSE,
                               ncore = 1, file_type = "csv",
                               file_dir = withr::local_tempdir())

  sim_id <- calib_aeme(aeme = aeme, path = path, param = aeme_parameters,
                       model = "glm_aed", FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = "HYD_temp", weights = weights)
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  best <- get_best_params(calib)

  res <- suppressMessages(
    run_and_fit(aeme = aeme, param = best, model = "glm_aed",
                vars_sim = "HYD_temp", path = path, FUN_list = FUN_list,
                weights = weights, include_wlev = FALSE, method = "calib",
                fit = TRUE)
  )
  # The recorded fit is itself saved to 6 significant digits.
  expect_equal(sum(unlist(res)), min(best$fit_value), tolerance = 1e-5)
})

test_that("MOEDA copes with a param_var_matrix that leaves parameters unlinked", {
  skip_if_slow()
  cached <- get_cached_aeme_run(model = "glm_aed", ext_elev = 5,
                                use_bgc = FALSE, run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
  data("aeme_parameters", package = "AEME")

  mae <- function(df) mean(abs(df$obs - df$model))
  ctrl <- create_calib_control(NP = 8, itermax = 24, parallel = FALSE,
                               ncore = 1, file_type = "csv",
                               c_method = "MOEDA",
                               file_dir = withr::local_tempdir())

  # Only five of the ten parameters are linked to a variable; the rest are
  # dropped from the calibration (with a warning), and the later generations
  # must not ask for them.
  linked <- c("light/Kw", "mixing/coef_mix_conv", "mixing/coef_wind_stir",
              "mixing/coef_mix_hyp", "mixing/coef_mix_shear")
  expect_no_error(
    suppressWarnings(suppressMessages(
      sim_id <- calib_aeme(aeme = aeme, path = path, param = aeme_parameters,
                           model = "glm_aed", FUN_list = list(HYD_temp = mae),
                           ctrl = ctrl, vars_sim = "HYD_temp",
                           weights = c("HYD_temp" = 1),
                           param_var_matrix = list(HYD_temp = linked))
    ))
  )
  expect_type(sim_id, "character")
})
