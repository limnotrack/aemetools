# Tests for pest_posterior_params() - turning a finished pestpp-ies run's
# parameter ensemble into runnable `param` sets. Synthetic run directory
# (make_pest_dir(), helper-pest.R), so no solver and no model run.

# A calib-like stub: parameter_metadata is the template, calibration_metadata
# points .pest_locate() at the synthetic run directory.
mk_calib <- function(s, extra_rows = NULL) {
  pm <- s$param
  if (!is.null(extra_rows)) pm <- rbind(pm, extra_rows[, names(pm)])
  list(
    parameter_metadata = transform(pm, sim_id = "sim_001"),
    calibration_metadata = data.frame(pest_dir = s$d, engine = "pest",
                                      c_method = "pest", na_value = 999)
  )
}

frozen_row <- data.frame(
  model = "glm_aed", file = "glm4.nml", group = "sediment", name = "n_zones",
  index = NA_integer_, value = 3, min = 3, max = 3, log = FALSE,
  stringsAsFactors = FALSE
)

test_that("pest_posterior_params returns one runnable param set per realisation", {
  skip_on_cran()
  s <- make_pest_dir()
  p <- pest_posterior_params(mk_calib(s))

  expect_s3_class(p, "aeme_param_sets")
  expect_length(p, 3)                       # real_0..real_2, base excluded
  expect_named(p, c("real_0", "real_1", "real_2"))

  for (set in p) {
    expect_s3_class(set, "data.frame")
    expect_identical(names(set),
                     c("model", "file", "name", "group", "index", "value",
                       "min", "max", "name_full"))
    expect_equal(nrow(set), nrow(s$param))
  }

  # Values track aeme.2.par.csv: p001 -> light/Kw[1], p002 -> light/ce.
  kw <- function(set) set$value[set$name_full == "light/Kw[1]"]
  ce <- function(set) set$value[set$name_full == "light/ce[NA]"]
  expect_equal(vapply(p, kw, numeric(1)),
               c(real_0 = 0.68, real_1 = 0.72, real_2 = 0.76))
  expect_equal(vapply(p, ce, numeric(1)),
               c(real_0 = 0.001, real_1 = 0.002, real_2 = 0.003))

  # min/max come from the template, not from the ensemble spread.
  expect_equal(p[[1]]$min, s$param$min)
  expect_equal(p[[1]]$max, s$param$max)
})

test_that("frozen parameters are carried through every set unchanged", {
  skip_on_cran()
  s <- make_pest_dir()
  p <- pest_posterior_params(mk_calib(s, extra_rows = frozen_row))

  for (set in p) {
    expect_true("sediment/n_zones[NA]" %in% set$name_full)
    fz <- set[set$name_full == "sediment/n_zones[NA]", ]
    expect_equal(fz$value, 3)      # untouched: absent from the ensemble
    expect_equal(c(fz$min, fz$max), c(3, 3))
  }
  # The adjustable rows still move.
  expect_false(p[["real_0"]]$value[p[["real_0"]]$name_full == "light/Kw[1]"] ==
                 p[["real_2"]]$value[p[["real_2"]]$name_full == "light/Kw[1]"])
})

test_that("include_base and iteration select the ensemble", {
  skip_on_cran()
  s <- make_pest_dir()

  with_base <- pest_posterior_params(mk_calib(s), include_base = TRUE)
  expect_length(with_base, 4)
  expect_true("base" %in% names(with_base))

  prior <- pest_posterior_params(mk_calib(s), iteration = 0)
  post  <- pest_posterior_params(mk_calib(s))
  kw <- function(x, r) x[[r]]$value[x[[r]]$name_full == "light/Kw[1]"]
  expect_equal(kw(prior, "real_0"), 0.2)     # aeme.0.par.csv
  expect_equal(kw(post,  "real_0"), 0.68)    # aeme.2.par.csv
  expect_equal(attr(prior, "iteration"), 0)
  expect_equal(attr(post,  "iteration"), 2)
})

test_that("n_max keeps the first realisations only", {
  skip_on_cran()
  s <- make_pest_dir()
  expect_length(pest_posterior_params(mk_calib(s), n_max = 2), 2)
})

test_that("a bare directory works when param is supplied", {
  skip_on_cran()
  s <- make_pest_dir()
  p <- pest_posterior_params(s$d, param = s$param)
  expect_s3_class(p, "aeme_param_sets")
  expect_length(p, 3)
})

test_that("missing template and mismatched param abort informatively", {
  skip_on_cran()
  s <- make_pest_dir()

  expect_error(pest_posterior_params(s$d), "template")

  wrong <- s$param
  wrong$name <- c("no_such", "also_no")
  wrong$group <- NA_character_
  wrong$index <- NA_integer_
  expect_error(pest_posterior_params(s$d, param = wrong),
               "do not match|None of the template")
})

test_that("print and as.data.frame give the two documented forms", {
  skip_on_cran()
  s <- make_pest_dir()
  p <- pest_posterior_params(mk_calib(s, extra_rows = frozen_row))

  expect_output(print(p), "sets")
  expect_output(print(p), "frozen")
  expect_invisible(print(p))

  df <- as.data.frame(p)
  expect_true("ensemble" %in% names(df))
  expect_equal(nrow(df), length(p) * nrow(p[[1]]))
  expect_setequal(unique(df$ensemble), names(p))
})
