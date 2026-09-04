# set_param_log(), freeze_param(), carry_param() and check_param_targets():
# the small helpers for assembling a `param` dataframe ahead of a staged
# calibration. The first three are pure table transforms; the last needs a
# built model to check names against.

demo_param <- function() {
  data.frame(
    model = "glm_aed", file = "glm4.nml",
    group = c("light", "light", "sediment"),
    name = c("Kw", "ce", "sed_temp_mean"),
    index = c(1L, NA_integer_, NA_integer_),
    value = c(0.5, 0.0013, 8),
    min = c(0.05, 5e-4, 4),
    max = c(5, 5e-3, 12),
    stringsAsFactors = FALSE
  )
}

# ---- set_param_log() -----------------------------------------------------

test_that("set_param_log() flags positive parameters that span the ratio", {
  p <- set_param_log(demo_param())
  expect_type(p$log, "logical")
  expect_equal(p$log, c(TRUE, TRUE, FALSE))          # 100x, 10x, 3x
})

test_that("set_param_log() honours the ratio and Inf disables it", {
  expect_equal(set_param_log(demo_param(), ratio = 100)$log,
               c(TRUE, FALSE, FALSE))
  expect_equal(set_param_log(demo_param(), ratio = Inf)$log,
               c(FALSE, FALSE, FALSE))
})

test_that("set_param_log() never flags a non-positive lower bound", {
  p <- demo_param()
  p$min[1] <- 0
  expect_false(set_param_log(p)$log[1])
})

test_that("set_param_log(overwrite = FALSE) keeps existing TRUE flags", {
  p <- demo_param()
  p$log <- c(FALSE, FALSE, TRUE)                     # ce..sed set by hand
  out <- set_param_log(p, overwrite = FALSE)
  expect_equal(out$log, c(TRUE, FALSE, TRUE))
  expect_equal(set_param_log(p, overwrite = TRUE)$log, c(TRUE, TRUE, FALSE))
})

test_that("set_param_log() validates its inputs", {
  expect_error(set_param_log(demo_param()[, c("name", "value")]), "min")
  expect_error(set_param_log(demo_param(), ratio = 0), "ratio")
})

# ---- freeze_param() ----------------------------------------------------

test_that("freeze_param() collapses min/max onto value", {
  p <- freeze_param(demo_param())
  expect_equal(p$min, p$value)
  expect_equal(p$max, p$value)
})

test_that("freeze_param(names =) freezes only the named rows", {
  p <- freeze_param(demo_param(), names = "Kw")
  expect_equal(p$min[1], p$value[1])
  expect_equal(p$max[1], p$value[1])
  expect_equal(p$min[2:3], demo_param()$min[2:3])    # untouched
})

test_that("freeze_param() rejects unknown names and NA values", {
  expect_error(freeze_param(demo_param(), names = "nope"), "nope")
  p <- demo_param(); p$value[2] <- NA
  expect_error(freeze_param(p, names = "ce"), "NA")
})

# ---- carry_param() ---------------------------------------------------

test_that("carry_param() freezes a get_best_params()-shaped frame", {
  best <- demo_param()
  best$fit_value <- c(0.2, 0.3, 0.4)                 # extra cols tolerated
  out <- carry_param(best)
  expect_equal(out$min, out$value)
  expect_equal(out$max, out$value)
  expect_true("name_full" %in% names(out))
  expect_equal(out$name_full[1], encode_param("light", "Kw", 1L))
})

test_that("carry_param() applies names / except and drops NA-value rows", {
  best <- demo_param()
  expect_equal(nrow(carry_param(best, names = c("Kw", "ce"))), 2L)
  expect_equal(carry_param(best, except = "Kw")$name, c("ce", "sed_temp_mean"))

  best$value[3] <- NA
  expect_warning(out <- carry_param(best), "no best value")
  expect_equal(nrow(out), 2L)
})

test_that("carry_param() errors on a frame missing param columns", {
  expect_error(carry_param(data.frame(a = 1)), "missing")
})

# ---- check_param_targets() -------------------------------------------

test_that(".config_keys() walks lists and table leaves", {
  cfg <- list(
    blockA = list(x = 1, y = 2),
    blockB = list(nested = list(z = 3)),
    tbl = data.frame(p_name = c("R_growth", "w_p"), green = 1:2)
  )
  keys <- aemetools:::.config_keys(cfg)
  expect_true(all(c("x", "y", "z", "R_growth", "w_p") %in% keys))
})

test_that("check_param_targets() finds names absent from the built model", {
  aeme <- get_cached_aeme_run("glm_aed", use_bgc = FALSE, run = FALSE)$aeme

  good <- data.frame(model = "glm_aed", file = "glm4.nml", group = NA,
                     name = "Kw", index = NA, value = 0.5, min = 0.1, max = 1,
                     stringsAsFactors = FALSE)
  bad <- good
  bad$name <- "Kw_not_a_real_parameter"

  expect_equal(nrow(check_param_targets(good, aeme)), 0L)
  expect_warning(out <- check_param_targets(bad, aeme), "no matching field")
  expect_equal(out$name, "Kw_not_a_real_parameter")
  expect_error(check_param_targets(bad, aeme, error = TRUE), "no matching field")
})
