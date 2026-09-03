# Tests for update_param() that need neither a model run nor a solver, by
# building a minimal `calib` object of the shape read_calib() returns.

make_fake_calib <- function(sim_id = "LID1_glmaed_C_001", na_value = 999) {

  param_meta <- data.frame(
    sim_id = sim_id,
    model = "glm_aed",
    file = c("glm4.nml", "glm4.nml"),
    name = c("Kw", "ce"),
    value = c(0.5, 0.0013),
    min = c(0.1, 0.0005),
    max = c(1.5, 0.005),
    group = c("light", "light"),
    index = c(1L, NA_integer_),
    stringsAsFactors = FALSE
  )

  pnames <- encode_param(param_meta$group, param_meta$name, param_meta$index)

  # Long format: one row per (run, parameter, fit_type). Two runs, the
  # second the better fit, so the "best" values are unambiguous.
  sim_data <- do.call(rbind, lapply(seq_len(2), function(r) {
    data.frame(
      sim_id = sim_id, gen = 1L, run = r,
      parameter_name = pnames,
      parameter_value = if (r == 1) c(0.4, 0.0011) else c(0.9, 0.0032),
      fit_type = "fit",
      fit_value = if (r == 1) 0.8 else 0.2,
      stringsAsFactors = FALSE
    )
  }))

  list(
    lake_metadata = data.frame(id = "LID1", name = "test"),
    simulation_metadata = data.frame(sim_id = sim_id, model = "glm_aed",
                                     stringsAsFactors = FALSE),
    parameter_metadata = param_meta,
    simulation_data = sim_data,
    calibration_metadata = data.frame(sim_id = sim_id, na_value = na_value)
  )
}

test_that("update_param works with a caller-supplied param dataframe", {
  # Regression: `name_full` was only derived inside the `missing(param)`
  # branch, but every join below keys on it, so the documented usage -
  # passing your own parameter dataframe - failed with "Join columns in `y`
  # must be present in the data". Every active caller in the package omits
  # `param`, and the only two that passed it were commented out, so the
  # path was dead rather than merely untested.
  calib <- make_fake_calib()

  param <- data.frame(
    model = "glm_aed",
    file = c("glm4.nml", "glm4.nml"),
    name = c("Kw", "ce"),
    value = c(0.5, 0.0013),
    min = c(0.1, 0.0005),
    max = c(1.5, 0.005),
    group = c("light", "light"),
    index = c(1L, NA_integer_),
    stringsAsFactors = FALSE
  )
  expect_false("name_full" %in% names(param))

  out <- expect_no_error(update_param(calib = calib, param = param))
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 2L)
  # Values come from the better-fitting run.
  expect_equal(out$value[out$name == "Kw"], 0.9)
  expect_equal(out$value[out$name == "ce"], 0.0032)
})

test_that("supplying param gives the same result as deriving it from calib", {
  calib <- make_fake_calib()

  from_calib <- update_param(calib = calib)
  supplied <- update_param(
    calib = calib,
    param = calib$parameter_metadata[, AEME::param_colnames(incl_opt = FALSE)]
  )
  expect_equal(supplied, from_calib)
})

test_that("an already-present name_full column is respected, not recomputed", {
  calib <- make_fake_calib()
  param <- calib$parameter_metadata[, AEME::param_colnames(incl_opt = FALSE)]
  param$name_full <- encode_param(param$group, param$name, param$index)

  expect_no_error(out <- update_param(calib = calib, param = param))
  expect_equal(nrow(out), 2L)
})
