# Tests for the deprecated run_aeme_ensemble() wrapper, which delegates
# supplied parameter sets (`param_sets`) to AEME::run_aeme_ensemble().
# AEME owns the detailed tests of that path.
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

test_that("deprecated run_aeme_ensemble delegates supplied sets to AEME", {
  skip_if_slow()
  skip_on_cran()
  model <- "gotm_wet"
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, run = FALSE)
  aeme <- cached$aeme
  path <- cached$path
  sets <- rpe_sets(model, n = 3)

  lifecycle::expect_deprecated(
    aeme2 <- run_aeme_ensemble(aeme = aeme, model = model, path = path,
                               param_sets = sets, parallel = FALSE),
    "AEME::run_aeme_ensemble"
  )
  outp <- AEME::output(aeme2)
  expect_equal(outp$n_members, 3)
  expect_true("Date" %in% names(outp$ens_001[[model]]))
  expect_true(ggplot2::is_ggplot(plot_ensemble(aeme2, model = model, depth = 0)))
})

test_that("deprecated run_aeme_ensemble accepts a long data.frame and parallel", {
  skip_if_slow()
  skip_on_cran()
  model <- "gotm_wet"
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, run = FALSE)
  sets <- rpe_sets(model, n = 4)
  long <- dplyr::bind_rows(stats::setNames(sets, paste0("m", 1:4)),
                           .id = "ensemble")

  suppressWarnings(
    aeme2 <- run_aeme_ensemble(aeme = cached$aeme, model = model,
                               path = cached$path, param_sets = long,
                               parallel = TRUE, ncore = 2)
  )
  expect_equal(AEME::output(aeme2)$n_members, 4)
  expect_equal(attr(AEME::output(aeme2), "realisation"), paste0("m", 1:4))
})

test_that("deprecated run_aeme_ensemble without param_sets points to the sampler", {
  skip_if_slow()
  skip_on_cran()
  model <- "gotm_wet"
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, run = FALSE)
  utils::data("aeme_parameters", package = "AEME", envir = environment())
  pp <- aeme_parameters[aeme_parameters$model == model, ]
  pp$min <- pp$value - 0.1 * abs(pp$value)
  pp$max <- pp$value + 0.1 * abs(pp$value)
  AEME::parameters(cached$aeme) <- pp

  lifecycle::expect_deprecated(
    aeme2 <- run_aeme_ensemble(aeme = cached$aeme, model = model, n = 2,
                               path = cached$path),
    "sample_aeme_ensemble"
  )
  expect_equal(AEME::output(aeme2)$n_members, 2)
})

test_that("a data.frame without an ensemble column aborts informatively", {
  skip_if_slow()
  skip_on_cran()
  model <- "gotm_wet"
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, run = FALSE)
  suppressWarnings(expect_error(
    run_aeme_ensemble(aeme = cached$aeme, model = model, path = cached$path,
                      param_sets = rpe_sets(model, 2)[[1]]),
    "ensemble"
  ))
})
