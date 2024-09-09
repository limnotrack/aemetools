# File: tests/testthat/test-app-function.R
library(shinytest2)

test_that("run_aeme app runs", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  # unlink(tmpdir, recursive = TRUE)
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  model_controls <- model_controls |>
    dplyr::mutate(simulate = dplyr::case_when(
      var_aeme == "ZOO_zoo1" ~ TRUE,
      .default = simulate
    ))
  model <- c("glm_aed", "gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           ext_elev = 5, use_bgc = FALSE)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  shiny_app <- run_aeme_shiny(aeme = aeme, param = param, path = path,
                              model_controls = model_controls)
  # app <- AppDriver$new(shiny_app, name = "AEME_app", wait = TRUE)

  app <- AppDriver$new(shiny_app, name = "AEME_app", wait = TRUE)

  # Wait for the specific UI element to appear (e.g., an input field)
  app$wait_for_value(input = "param1")
  app$click("build")
  app$click("run")
  # Update output value
  # app$expect_screenshot()

  app$expect_values()

  # record_test(app = app, seed = 123, name = "AEME_app",
  #             test_file = "test-app-function.R", record_screen_size = FALSE,
  #             run_test = TRUE)

})
