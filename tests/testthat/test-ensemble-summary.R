# Tests for ensemble_summary() / score_ensemble() and the ensemble_summary
# path through plot_ensemble(). gotm_wet only - it makes its own output
# directory, so a serial ensemble needs no PEST binaries and no
# make_temp_dir() output/ fix.

es_ensemble <- function(n = 6) {
  cached <- get_cached_aeme_run(model = "gotm_wet", ext_elev = 5, run = FALSE)
  utils::data("aeme_parameters", package = "AEME", envir = environment())
  pp <- aeme_parameters[aeme_parameters$model == "gotm_wet", ]
  pp$min <- pp$value - 0.1 * abs(pp$value)
  pp$max <- pp$value + 0.1 * abs(pp$value)
  AEME::parameters(cached$aeme) <- pp
  aeme <- run_aeme_ensemble(aeme = cached$aeme, model = "gotm_wet", n = n,
                            path = cached$path, parallel = FALSE)
  list(aeme = aeme, path = cached$path, n = n)
}

test_that("ensemble_summary summarises a depth-resolved variable", {
  skip_on_cran()
  e <- es_ensemble()

  s <- ensemble_summary(e$aeme, "gotm_wet", vars_sim = "HYD_temp",
                        depths = c(1, 5))

  expect_s3_class(s, "aeme_ensemble_summary")
  expect_setequal(unique(s$stats$depth), c(1, 5))
  expect_true(all(c("mean", "sd", "n", "q2.5", "q50", "q97.5") %in%
                    names(s$stats)))
  expect_true(all(s$stats$n <= e$n) && max(s$stats$n) == e$n)
  # bands are ordered
  expect_true(all(s$stats$q2.5 <= s$stats$q50 + 1e-8, na.rm = TRUE))
  expect_true(all(s$stats$q50 <= s$stats$q97.5 + 1e-8, na.rm = TRUE))
  # members retained, one block per realisation
  expect_setequal(unique(s$members$ens), seq_len(e$n))
  expect_setequal(unique(s$members$depth), c(1, 5))

  expect_output(print(s), "aeme_ensemble_summary")
  expect_identical(as.data.frame(s), as.data.frame(s$stats))
})

test_that("ensemble_summary handles a 1-D variable", {
  skip_on_cran()
  e <- es_ensemble()

  s <- ensemble_summary(e$aeme, "gotm_wet", vars_sim = "LKE_lvlwtr")
  expect_true(all(is.na(s$stats$depth)))
  expect_true(all(c("q2.5", "q50", "q97.5") %in% names(s$stats)))

  p <- plot_ensemble(s, var_sim = "LKE_lvlwtr")
  expect_true(ggplot2::is_ggplot(p))
})

test_that("plot_ensemble renders from a summary without re-extracting", {
  skip_on_cran()
  e <- es_ensemble()
  s <- ensemble_summary(e$aeme, "gotm_wet", vars_sim = "HYD_temp",
                        depths = c(1, 5))

  expect_true(ggplot2::is_ggplot(
    plot_ensemble(s, model = "gotm_wet", var_sim = "HYD_temp", depth = 5)))
  expect_true(ggplot2::is_ggplot(
    plot_ensemble(s, var_sim = "HYD_temp", depth = 1, type = "line")))
})

test_that("plot_ensemble(aeme) and plot_ensemble(summary) agree", {
  skip_on_cran()
  e <- es_ensemble()

  from_aeme <- plot_ensemble(e$aeme, model = "gotm_wet", var_sim = "HYD_temp",
                             depth = 2)
  s <- ensemble_summary(e$aeme, "gotm_wet", vars_sim = "HYD_temp", depths = 2,
                        probs = c(0.025, 0.5, 0.975))
  from_summary <- plot_ensemble(s, model = "gotm_wet", var_sim = "HYD_temp",
                                depth = 2)

  expect_equal(from_aeme$data$.mid, from_summary$data$.mid)
  expect_equal(from_aeme$data$.lower, from_summary$data$.lower)
})

test_that("a summary without the requested interval errors informatively", {
  skip_on_cran()
  e <- es_ensemble()
  s <- ensemble_summary(e$aeme, "gotm_wet", vars_sim = "HYD_temp",
                        depths = 3, probs = c(0.25, 0.75))

  expect_error(plot_ensemble(s, var_sim = "HYD_temp", depth = 3),
               "does not carry|matching")
})

test_that("keep_members = FALSE drops the frame and blocks type = line", {
  skip_on_cran()
  e <- es_ensemble()
  s <- ensemble_summary(e$aeme, "gotm_wet", vars_sim = "HYD_temp", depths = 4,
                        keep_members = FALSE)
  expect_null(s$members)
  expect_error(plot_ensemble(s, var_sim = "HYD_temp", depth = 4,
                             type = "line"), "per-member")
})

test_that("score_ensemble is a documented stub", {
  skip_on_cran()
  expect_error(score_ensemble(structure(list(), class = "aeme_ensemble_summary")),
               class = "aemetools_not_implemented")
})
