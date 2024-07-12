test_that("can run an ensemble of AEME-GLM & GOTM in parallel", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed", "gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)

  utils::data("aeme_parameters", package = "AEME")

  AEME::parameters(aeme) <- aeme_parameters

  # Run ensemble
  aeme <- run_aeme_ensemble(aeme = aeme, model = model, n = 5, path = path,
                            parallel = TRUE)

  outp <- AEME::output(aeme)
  testthat::expect_true(outp$n_members == 5)

})

test_that("can run an ensemble of AEME-GLM in series", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model,
                         verbose = FALSE, path = path)
  # AEME::plot(aeme, model = model)
  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(all(file_chk))

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- create_control(method = "calib", NP = 10, itermax = 30, ncore = 2,
                         parallel = TRUE, file_type = "db",
                         file_name = "results.db")

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 10, "LKE_lvlwtr" = 1)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id, path = path)

  testthat::expect_true(is.list(calib))
  ctrl$parallel <- FALSE

  # Run ensemble
  aeme <- run_aeme_ensemble(aeme = aeme, sim_id = sim_id, param = param,
                            path = path, calib = calib, ctrl = ctrl)

  outp <- AEME::output(aeme)
  testthat::expect_true(outp$n_members == ctrl$NP)

})

test_that("can run an ensemble of AEME-GLM in parallel and plot", {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("gotm_wet")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)

  utils::data("aeme_parameters", package = "AEME")
  aeme_parameters <- aeme_parameters |>
    dplyr::mutate(min = value - 0.1 * value,
                  max = value + 0.1 * value)

  AEME::parameters(aeme) <- aeme_parameters

  # Run ensemble
  aeme <- run_aeme_ensemble(aeme = aeme, model = model, n = 5, path = path,
                            parallel = TRUE)

  outp <- AEME::output(aeme)
  testthat::expect_true(outp$n_members == 5)

  p <- plot_ensemble(aeme = aeme, model = model, depth = 0)

  testthat::expect_true(ggplot2::is.ggplot(p))

  p2 <- plot_ensemble(aeme = aeme, model = model, depth = 5, type = "line")
  testthat::expect_true(ggplot2::is.ggplot(p2))

})
