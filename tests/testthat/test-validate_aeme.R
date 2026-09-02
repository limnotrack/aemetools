# Tests for the split-sample bridge between get_calib_periods() and the
# calibration engines.
#
# validate_aeme() is scored, not run: run_and_fit() is mocked so the fit
# arithmetic, the period bookkeeping and the degradation table can be
# pinned down exactly, without paying for two model runs per assertion.
# The one thing a mock cannot check - that the window actually reaches the
# model - is covered by the set_calib_period() tests, which read it back
# off the object with AEME::time().

va_aeme <- function(obs_dates = seq(as.Date("2020-01-01"), by = "month",
                                    length.out = 12),
                    vars = "HYD_temp") {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  lake <- expand.grid(Date = obs_dates, var_aeme = vars, depth = 1:2,
                      stringsAsFactors = FALSE)
  lake$value <- seq_len(nrow(lake))
  obs <- AEME::observations(aeme)
  obs$lake <- lake
  obs$level <- NULL
  AEME::observations(aeme) <- obs
  inp <- AEME::input(aeme)
  inp$meteo <- data.frame(Date = seq(as.Date("2019-01-01"),
                                     as.Date("2023-12-31"), by = "day"),
                          MET_tmpair = 15)
  AEME::input(aeme) <- inp
  aeme
}

va_periods <- function(aeme, ...) get_calib_periods(aeme, vars_sim = "HYD_temp", ...)

# A stand-in for run_and_fit(return_df = TRUE): one row per observation in
# the object's current window, with a controllable model-observation error
# so the resulting fit is known in advance.
va_fake_run <- function(err = c(calib = 1, valid = 1)) {
  function(aeme, ...) {
    tme <- AEME::time(aeme)
    obs <- AEME::observations(aeme)$lake
    keep <- obs$Date >= as.Date(tme$start) & obs$Date <= as.Date(tme$stop)
    d <- obs[keep, , drop = FALSE]
    # Which period this is, inferred from the window the caller set.
    e <- if (as.Date(tme$start) == min(obs$Date)) err[["calib"]] else err[["valid"]]
    data.frame(var_aeme = d$var_aeme, Date = d$Date, depth = d$depth,
               obs = d$value, model = d$value + e, stringsAsFactors = FALSE)
  }
}

test_that("set_calib_period puts the period on the object", {
  aeme <- va_aeme()
  p <- va_periods(aeme)

  cal <- set_calib_period(aeme, p, "calib")
  val <- set_calib_period(aeme, p, "valid")

  expect_equal(as.Date(AEME::time(cal)$start), p$periods$start[1])
  expect_equal(as.Date(AEME::time(cal)$stop), p$periods$stop[1])
  expect_equal(as.Date(AEME::time(val)$start), p$periods$start[2])
  expect_equal(as.Date(AEME::time(val)$stop), p$periods$stop[2])

  # The two windows do not overlap - the held-out period is genuinely held out.
  expect_lt(as.Date(AEME::time(cal)$stop), as.Date(AEME::time(val)$start))
  # The source object is untouched.
  expect_false(identical(AEME::time(aeme)$start, AEME::time(cal)$start))
})

test_that("spin-up is carried over, and overridable", {
  aeme <- va_aeme()
  p <- va_periods(aeme)

  # Default keeps whatever the object had - the validation period starts on
  # an observation date, so it needs spin-up as much as calibration does.
  before <- AEME::time(aeme)$spin_up
  expect_equal(AEME::time(set_calib_period(aeme, p, "valid"))$spin_up, before)

  set <- AEME::time(set_calib_period(aeme, p, "valid", spin_up = 30))$spin_up
  expect_true(all(unlist(set) == 30))
})

test_that("set_calib_period rejects anything but a periods object", {
  aeme <- va_aeme()
  p <- va_periods(aeme)

  expect_error(set_calib_period(aeme, list(a = 1), "calib"),
               "get_calib_periods")
  expect_error(set_calib_period(aeme, p$periods, "calib"), "get_calib_periods")
  expect_error(set_calib_period(aeme, p, "nonsense"), "period")
})

test_that("set_calib_period takes a no-split object with no period named", {
  aeme <- va_aeme()
  all <- get_calib_periods(aeme, vars_sim = "HYD_temp", split = FALSE)

  # The whole point of defaulting to the first period: the same call works
  # for a split and a no-split result.
  a <- set_calib_period(aeme, all)
  obs <- AEME::observations(aeme)$lake
  expect_equal(as.Date(AEME::time(a)$start), min(obs$Date))
  expect_equal(as.Date(AEME::time(a)$stop), max(obs$Date))
  expect_equal(a, set_calib_period(aeme, all, "all"))

  # Asking for a period it does not have says so, and why.
  expect_error(set_calib_period(aeme, all, "valid"), "split = FALSE")
})

test_that("validate_aeme refuses a no-split object", {
  aeme <- va_aeme()
  all <- get_calib_periods(aeme, vars_sim = "HYD_temp", split = FALSE)
  fl <- list(HYD_temp = function(df) mean(abs(df$obs - df$model)))

  # There is nothing held out to score against, so this must not quietly
  # report a degradation of NA.
  expect_error(validate_aeme(aeme = aeme, param = data.frame(), periods = all,
                             model = "glm_aed", vars_sim = "HYD_temp",
                             path = tempdir(), FUN_list = fl),
               "no validation period")
})

test_that("validate_aeme scores both periods with the same parameters", {
  aeme <- va_aeme()
  p <- va_periods(aeme)
  # Fit function with a known answer: mean absolute error, so a constant
  # offset of `err` scores exactly `err`.
  fl <- list(HYD_temp = function(df) mean(abs(df$obs - df$model)))

  local_mocked_bindings(run_and_fit = va_fake_run(c(calib = 1, valid = 3)),
                        .package = "aemetools")
  v <- validate_aeme(aeme = aeme, param = data.frame(), periods = p,
                     model = "glm_aed", vars_sim = "HYD_temp",
                     path = tempdir(), FUN_list = fl)

  expect_s3_class(v, "aeme_validation")
  expect_setequal(v$fit$period, c("calib", "valid"))
  expect_equal(v$fit$fit[v$fit$period == "calib"], 1)
  expect_equal(v$fit$fit[v$fit$period == "valid"], 3)

  # Observation counts match the periods table - nothing lost or duplicated.
  expect_equal(v$fit$n_obs[v$fit$period == "calib"], p$periods$n_obs[1])
  expect_equal(v$fit$n_obs[v$fit$period == "valid"], p$periods$n_obs[2])
  expect_equal(nrow(v$comparison), sum(p$periods$n_obs))
  expect_setequal(unique(v$comparison$period), c("calib", "valid"))
})

test_that("degradation is valid minus calib, so positive means worse", {
  aeme <- va_aeme()
  p <- va_periods(aeme)
  fl <- list(HYD_temp = function(df) mean(abs(df$obs - df$model)))

  # Over-fitted: the held-out period is much worse.
  local_mocked_bindings(run_and_fit = va_fake_run(c(calib = 1, valid = 5)),
                        .package = "aemetools")
  worse <- validate_aeme(aeme = aeme, param = data.frame(), periods = p,
                         model = "glm_aed", vars_sim = "HYD_temp",
                         path = tempdir(), FUN_list = fl)
  expect_equal(worse$degradation$calib, 1)
  expect_equal(worse$degradation$valid, 5)
  expect_equal(worse$degradation$degradation, 4)
  expect_gt(worse$degradation$degradation, 0)

  # Transfers cleanly: no penalty on the held-out period.
  local_mocked_bindings(run_and_fit = va_fake_run(c(calib = 2, valid = 2)),
                        .package = "aemetools")
  same <- validate_aeme(aeme = aeme, param = data.frame(), periods = p,
                        model = "glm_aed", vars_sim = "HYD_temp",
                        path = tempdir(), FUN_list = fl)
  expect_equal(same$degradation$degradation, 0)
})

test_that("each variable is scored separately", {
  aeme <- va_aeme(vars = c("HYD_temp", "CHM_oxy"))
  p <- get_calib_periods(aeme, vars_sim = c("HYD_temp", "CHM_oxy"))
  fl <- list(HYD_temp = function(df) mean(abs(df$obs - df$model)),
             # A deliberately different statistic, to prove each variable
             # goes through its own function rather than a shared one.
             CHM_oxy = function(df) max(abs(df$obs - df$model)))

  local_mocked_bindings(run_and_fit = va_fake_run(c(calib = 2, valid = 2)),
                        .package = "aemetools")
  v <- validate_aeme(aeme = aeme, param = data.frame(), periods = p,
                     model = "glm_aed", vars_sim = c("HYD_temp", "CHM_oxy"),
                     path = tempdir(), FUN_list = fl)

  expect_setequal(v$degradation$var_aeme, c("HYD_temp", "CHM_oxy"))
  expect_equal(nrow(v$fit), 4)
  expect_true(all(v$fit$fit == 2))
})

test_that("a period with no comparable observations warns and scores NA", {
  aeme <- va_aeme()
  p <- va_periods(aeme)
  fl <- list(HYD_temp = function(df) mean(abs(df$obs - df$model)))

  # Every model value missing in the validation period.
  drop_valid <- function(aeme, ...) {
    df <- va_fake_run()(aeme, ...)
    if (as.Date(AEME::time(aeme)$start) != min(AEME::observations(aeme)$lake$Date)) {
      df$model <- NA_real_
    }
    df
  }
  local_mocked_bindings(run_and_fit = drop_valid, .package = "aemetools")

  run <- function() {
    validate_aeme(aeme = aeme, param = data.frame(), periods = p,
                  model = "glm_aed", vars_sim = "HYD_temp",
                  path = tempdir(), FUN_list = fl)
  }
  # cli_alert_warning() is a styled *message*, like every other advisory in
  # the package, and setup.R silences those - so re-enable them here.
  withr::with_options(
    list(AEME.inform = TRUE),
    expect_message(run(), "No comparable observations"))
  v <- suppressMessages(run())

  expect_equal(v$fit$n_obs[v$fit$period == "valid"], 0)
  expect_true(is.na(v$fit$fit[v$fit$period == "valid"]))
  expect_true(is.na(v$degradation$degradation))
})

test_that("validate_aeme validates its arguments", {
  aeme <- va_aeme()
  p <- va_periods(aeme)
  fl <- list(HYD_temp = function(df) mean(abs(df$obs - df$model)))

  expect_error(validate_aeme(aeme = aeme, param = data.frame(),
                             periods = list(), model = "glm_aed",
                             vars_sim = "HYD_temp", path = tempdir(),
                             FUN_list = fl),
               "get_calib_periods")
  expect_error(validate_aeme(aeme = aeme, param = data.frame(), periods = p,
                             model = c("glm_aed", "gotm_wet"),
                             vars_sim = "HYD_temp", path = tempdir(),
                             FUN_list = fl),
               "single model")
  # A missing fitness function is an error, not a silent default: the two
  # periods have to be scored the same way the calibration was.
  expect_error(validate_aeme(aeme = aeme, param = data.frame(), periods = p,
                             model = "glm_aed", vars_sim = "HYD_temp",
                             path = tempdir()),
               "FUN_list")
  expect_error(validate_aeme(aeme = aeme, param = data.frame(), periods = p,
                             model = "glm_aed",
                             vars_sim = c("HYD_temp", "CHM_oxy"),
                             path = tempdir(), FUN_list = fl),
               "CHM_oxy")
})

test_that("a failed model run aborts rather than scoring nothing", {
  aeme <- va_aeme()
  p <- va_periods(aeme)
  fl <- list(HYD_temp = function(df) mean(abs(df$obs - df$model)))

  # run_and_fit() hands back its na_value list, not a dataframe, when the
  # model fails - that must not be scored as if it were a comparison.
  local_mocked_bindings(run_and_fit = function(...) list(HYD_temp = 999),
                        .package = "aemetools")
  expect_error(validate_aeme(aeme = aeme, param = data.frame(), periods = p,
                             model = "glm_aed", vars_sim = "HYD_temp",
                             path = tempdir(), FUN_list = fl),
               "did not produce a comparison")
})

test_that("the print method shows the degradation table", {
  aeme <- va_aeme()
  p <- va_periods(aeme)
  fl <- list(HYD_temp = function(df) mean(abs(df$obs - df$model)))
  local_mocked_bindings(run_and_fit = va_fake_run(c(calib = 1, valid = 3)),
                        .package = "aemetools")
  v <- validate_aeme(aeme = aeme, param = data.frame(), periods = p,
                     model = "glm_aed", vars_sim = "HYD_temp",
                     path = tempdir(), FUN_list = fl)

  out <- paste(capture.output(print(v)), collapse = "\n")
  expect_match(out, "HYD_temp")
  expect_match(out, "degradation")
  expect_invisible(print(v))
})
