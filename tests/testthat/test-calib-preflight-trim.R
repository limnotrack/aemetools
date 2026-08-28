# Pre-flight check and output trimming for calib_aeme() / sa_aeme().
# One glm_aed lake is built once for the file.

.pt_cache <- new.env(parent = emptyenv())

pt_fixture <- function() {
  if (is.null(.pt_cache$fx)) {
    aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
    mc <- AEME::set_vars_sim(AEME::get_model_controls(), vars_sim = "HYD_temp")
    path <- tempfile("pt_glm_")
    dir.create(path, recursive = TRUE)
    aeme <- AEME::build_aeme(aeme = aeme, model = "glm_aed", path = path,
                             model_controls = mc, ext_elev = 5,
                             use_bgc = FALSE)
    aeme <- AEME::run_aeme(aeme = aeme, model = "glm_aed", path = path)
    utils::data("aeme_parameters", package = "AEME", envir = environment())
    param <- aeme_parameters[aeme_parameters$model == "glm_aed" &
                               aeme_parameters$name != "outflow", ]
    .pt_cache$fx <- list(aeme = aeme, path = path, mc = mc, param = param)
  }
  .pt_cache$fx
}

pt_ctrl <- function(...) {
  create_calib_control(NP = NA, itermax = 4, c_method = "LHC",
                       parallel = FALSE, file_type = "csv",
                       file_dir = tempfile("pt_cs_"), ...)
}


test_that("apply_trim_output strips GLM point CSVs and the mass_balance block", {
  fx <- pt_fixture()
  md <- file.path(AEME::get_lake_dir(fx$aeme, path = fx$path), "glm_aed")

  aeme <- apply_trim_output(fx$aeme, "glm_aed", "HYD_temp", fx$path)

  nml <- AEME::read_nml(file.path(md, "glm4.nml"))
  expect_false(any(grepl("^csv_point", names(nml$output))))
  expect_null(nml$mass_balance)

  unlink(file.path(md, "output"), recursive = TRUE)
  dir.create(file.path(md, "output"))
  suppressMessages(AEME::run_aeme(aeme, model = "glm_aed", path = fx$path))
  files <- list.files(file.path(md, "output"))
  expect_false(any(grepl("mass_balance|^WQ_", files)))
  expect_true("output.nc" %in% files)
})

test_that("calib_preflight passes a working setup and returns the fit list", {
  fx <- pt_fixture()
  res <- calib_preflight(aeme = fx$aeme, param = fx$param, m = "glm_aed",
                         path = fx$path, vars_sim = "HYD_temp",
                         FUN_list = list(HYD_temp = mae),
                         weights = c(HYD_temp = 1), model_controls = fx$mc,
                         ctrl = list(na_value = 999, timeout = Inf),
                         include_wlev = FALSE, method = "calib")
  expect_type(res, "list")
  expect_true(is.finite(res$HYD_temp))
})

test_that("calib_preflight aborts on a setup that produces no usable fit", {
  fx <- pt_fixture()
  expect_error(
    suppressWarnings(calib_preflight(
      aeme = fx$aeme, param = fx$param, m = "glm_aed", path = fx$path,
      vars_sim = "HYD_temp", FUN_list = list(HYD_temp = mae),
      weights = c(HYD_temp = 1), model_controls = fx$mc,
      ctrl = list(na_value = 999, timeout = 1e-6),
      include_wlev = FALSE, method = "calib")),
    class = "aemetools_error_preflight"
  )
})

test_that("calib_aeme aborts at pre-flight instead of running a whole calibration", {
  fx <- pt_fixture()
  t0 <- Sys.time()
  expect_error(
    suppressWarnings(calib_aeme(
      aeme = fx$aeme, model = "glm_aed", param = fx$param, path = fx$path,
      vars_sim = "HYD_temp", FUN_list = list(HYD_temp = mae),
      weights = c(HYD_temp = 1),
      ctrl = pt_ctrl(timeout = 1e-6, trim_output = FALSE))),
    class = "aemetools_error_preflight"
  )
  # It must fail fast - one run, not the LHC budget.
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 60)
})

test_that("calib_aeme runs to completion with preflight + trim_output on", {
  fx <- pt_fixture()
  sid <- calib_aeme(aeme = fx$aeme, model = "glm_aed", param = fx$param,
                    path = fx$path, vars_sim = "HYD_temp",
                    FUN_list = list(HYD_temp = mae), weights = c(HYD_temp = 1),
                    ctrl = pt_ctrl())
  expect_type(sid, "character")
  expect_match(sid, "glmaed")

  md <- file.path(AEME::get_lake_dir(fx$aeme, path = fx$path), "glm_aed")
  nml <- AEME::read_nml(file.path(md, "glm4.nml"))
  expect_null(nml$mass_balance)   # trim_output took effect
})

test_that("preflight = FALSE skips the check", {
  fx <- pt_fixture()
  # A broken setup that would abort at pre-flight - with preflight off it must
  # get past that point (it then fails later or returns NA fits, which is not
  # our concern here).
  got_preflight_error <- tryCatch({
    suppressWarnings(calib_aeme(
      aeme = fx$aeme, model = "glm_aed", param = fx$param, path = fx$path,
      vars_sim = "HYD_temp", FUN_list = list(HYD_temp = mae),
      weights = c(HYD_temp = 1),
      ctrl = pt_ctrl(preflight = FALSE, trim_output = FALSE, timeout = 1e-6)))
    FALSE
  },
  aemetools_error_preflight = function(e) TRUE,
  error = function(e) FALSE)

  expect_false(got_preflight_error)
})
