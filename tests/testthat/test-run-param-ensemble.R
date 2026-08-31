# Tests for run_aeme_ensemble()'s supplied-ensemble path (`param_sets`).
# Uses the AEME test lake via the session cache; gotm_wet for most cases
# (it makes its own output dir), plus one glm_aed parallel case that only
# works once make_temp_dir() recreates the empty output/ directory.

rpe_sets <- function(model = "gotm_wet", n = 4, seed = 1) {
  utils::data("aeme_parameters", package = "AEME", envir = environment())
  pp <- aeme_parameters[aeme_parameters$model == model, ]
  pp$name_full <- encode_param(pp$group, pp$name, pp$index)
  set.seed(seed)
  lapply(seq_len(n), function(i) {
    d <- pp
    d$value <- stats::runif(nrow(d), d$min, d$max)
    d
  })
}

test_that("run_aeme_ensemble runs a supplied list of parameter sets", {
  skip_on_cran()
  model <- "gotm_wet"
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, run = FALSE)
  aeme <- cached$aeme
  path <- cached$path
  sets <- rpe_sets(model, n = 4)

  aeme2 <- run_aeme_ensemble(aeme = aeme, model = model, path = path,
                             param_sets = sets, parallel = TRUE, ncore = 2)

  outp <- AEME::output(aeme2)
  expect_equal(outp$n_members, 4)
  for (i in 1:4) {
    slot <- outp[[sprintf("ens_%03d", i)]]
    expect_true(model %in% names(slot))
    expect_true("Date" %in% names(slot[[model]]))
  }

  p <- plot_ensemble(aeme2, model = model, depth = 0)
  expect_true(ggplot2::is_ggplot(p))
  expect_true(ggplot2::is_ggplot(
    plot_ensemble(aeme2, model = model, depth = 5, type = "line")))
})

test_that("glm_aed runs a supplied ensemble in parallel (make_temp_dir output/)", {
  skip_on_cran()
  # Regression: make_temp_dir() copies the model config without output/ and,
  # unlike .pest_stage_model(), used not to recreate it - GLM 4.0.0 aborts at
  # init without output/, so every parallel member returned na_value and
  # .assemble_ens_output() aborted "No ensemble member ran successfully".
  model <- "glm_aed"
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
  sets <- rpe_sets(model, n = 3)

  aeme2 <- run_aeme_ensemble(aeme = aeme, model = model, path = path,
                             param_sets = sets, parallel = TRUE, ncore = 2)
  expect_equal(AEME::output(aeme2)$n_members, 3)
  expect_true("Date" %in% names(AEME::output(aeme2)$ens_001[[model]]))
})

test_that("serial and a long dataframe give the same ensemble", {
  skip_on_cran()
  model <- "gotm_wet"
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, run = FALSE)
  aeme <- cached$aeme
  path <- cached$path
  sets <- rpe_sets(model, n = 3)

  ser <- run_aeme_ensemble(aeme = aeme, model = model, path = path,
                           param_sets = sets, parallel = FALSE)
  expect_equal(AEME::output(ser)$n_members, 3)

  long <- dplyr::bind_rows(stats::setNames(sets, paste0("m", 1:3)),
                           .id = "ensemble")
  from_long <- run_aeme_ensemble(aeme = aeme, model = model, path = path,
                                 param_sets = long, parallel = FALSE)
  expect_equal(AEME::output(from_long)$n_members, 3)
})

test_that("an aeme_param_sets object is accepted directly", {
  skip_on_cran()
  model <- "gotm_wet"
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, run = FALSE)
  aeme <- cached$aeme
  path <- cached$path
  sets <- rpe_sets(model, n = 3)
  class(sets) <- c("aeme_param_sets", "list")

  aeme2 <- run_aeme_ensemble(aeme = aeme, model = model, path = path,
                             param_sets = sets, parallel = FALSE)
  expect_equal(AEME::output(aeme2)$n_members, 3)
})

test_that("a member that fails to run is dropped with a warning", {
  skip_on_cran()
  model <- "gotm_wet"
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, run = FALSE)
  aeme <- cached$aeme
  path <- cached$path
  sets <- rpe_sets(model, n = 3)
  # Break member 2: a nonsense turbulence parameter forces the run to fail.
  sets[[2]]$value <- sets[[2]]$value * 1e12

  expect_warning(
    aeme2 <- run_aeme_ensemble(aeme = aeme, model = model, path = path,
                               param_sets = sets, parallel = FALSE),
    "failed to run"
  )
  expect_equal(AEME::output(aeme2)$n_members, 2)
})

test_that("a malformed param_sets aborts informatively", {
  skip_on_cran()
  model <- "gotm_wet"
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, run = FALSE)
  aeme <- cached$aeme

  # data.frame with no ensemble id column
  expect_error(
    run_aeme_ensemble(aeme = aeme, model = model, path = cached$path,
                      param_sets = rpe_sets(model, 2)[[1]]),
    "ensemble"
  )
  # list element missing required columns
  bad <- rpe_sets(model, 2)
  bad[[1]]$value <- NULL
  expect_error(
    run_aeme_ensemble(aeme = aeme, model = model, path = cached$path,
                      param_sets = bad),
    "value|malformed"
  )
})
