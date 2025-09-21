test_that("can execute sensitivity analysis for AEME-DYRESM in parallel", {

  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("dy_cd")
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
                                    model, "DYsim.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters |>
    dplyr::filter(file != "wdr")

  # Function to calculate fitness
  fit <- function(df) {
    mean(df$model)
  }

  FUN_list <- list(HYD_temp = fit)

  ctrl <- create_control(method = "sa", N = 2^1, ncore = 2, parallel = TRUE,
                         file_type = "db", file_name = "results.db",
                         vars_sim = list(
                           surf_temp = list(var = "HYD_temp",
                                            month = c(10:12, 1:3),
                                            depth_range = c(0, 2)
                           ),
                           bot_temp = list(var = "HYD_temp",
                                           month = c(10:12, 1:3),
                                           depth_range = c(10, 13)
                           )
                         )
  )

  # Run sensitivity analysis AEME model
  sim_id <- sa_aeme(aeme = aeme, path = path, param = param,
                    model = model, ctrl = ctrl, FUN_list = FUN_list)

  sa_res <- read_sa(ctrl = ctrl, sim_id = sim_id, boot = FALSE)

  testthat::expect_true(is.data.frame(sa_res[[1]]$df))
})

test_that("can execute sensitivity analysis for AEME-GLM in parallel", {

  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  inp <- AEME::input(aeme)
  model_controls <- AEME::get_model_controls()
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = FALSE)

  aeme <- AEME::run_aeme(aeme = aeme, model = model, verbose = FALSE,
                         path = path)

  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters |>
    dplyr::filter(file != "wdr")

  # Function to calculate fitness
  fit <- function(df) {
    mean(df$model)
  }
  fit2 <- function(df) {
    median(df$model, na.rm = TRUE)
  }
  bot_deps <- c(inp$init_depth - 2, inp$init_depth)
  FUN_list <- list(HYD_temp = fit, HYD_thmcln = fit2, LKE_lvlwtr = fit2)
  db_file <- "results.db"
  ctrl <- create_control(method = "sa", N = 2^2,
                         file_type = "db", file_name = db_file,
                         na_value = 1e20, ncore = 2,
                         vars_sim = list(
                           surf_temp = list(var = "HYD_temp",
                                            month = c(12, 1:2),
                                            depth_range = c(0, 2)
                           ),
                           bot_temp = list(var = "HYD_temp",
                                           month = c(12, 1:2),
                                           depth_range = bot_deps
                           ),
                           thm_cln = list(var = "HYD_thmcln",
                                          month = c(12, 1:2),
                                          depth_range = c(0, inp$init_depth)
                           ),
                           lke_lvl = list(var = "LKE_lvlwtr",
                                          month = c(12, 1:2),
                                          depth_range = c(0, inp$init_depth)
                           )
                         )
  )

  # ctrl <- create_control(method = "sa", N = 2^2, ncore = 2, parallel = TRUE,
  #                        file_type = "db", file_name = "results.db",
  #                        vars_sim = list(
  #                          surf_temp = list(var = "HYD_temp",
  #                                           month = c(10:12, 1:3),
  #                                           depth_range = c(0, 2)
  #                          ),
  #                          bot_temp = list(var = "HYD_temp",
  #                                          month = c(10:12, 1:3),
  #                                          depth_range = c(10, 13)
  #                          )
  #                        )
  # )

  # Run sensitivity analysis AEME model
  sim_id <- sa_aeme(aeme = aeme, path = path, param = param, model = model,
                    ctrl = ctrl, FUN_list = FUN_list)

  sa_res <- read_sa(ctrl = ctrl, sim_id = sim_id, R = 2^2)

  testthat::expect_true(is.data.frame(sa_res[[1]]$df))

  p1 <- plot_uncertainty(sa_res)
  testthat::expect_true(ggplot2::is_ggplot(p1))

  p2 <- plot_scatter(sa_res)
  testthat::expect_true(ggplot2::is_ggplot(p2))

  pl1 <- plot_multiscatter(sa_res)
  testthat::expect_true(is.list(pl1))
  testthat::expect_true(ggplot2::is_ggplot(pl1[[1]][[1]]))

  pl2 <- plot_sobol(sa = sa_res, add_errorbars = TRUE, use_dummy = TRUE)
  testthat::expect_true(ggplot2::is_ggplot(pl2))

})

test_that("can execute sensitivity analysis for AEME-GLM in parallel for just LKE_lvlwtr", {

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

  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters |>
    dplyr::filter(file != "wdr")

  # Function to calculate fitness
  fit <- function(df) {
    mean(df$model)
  }

  FUN_list <- list(LKE_lvlwtr = fit)

  ctrl <- create_control(method = "sa", N = 2^2, ncore = 2, parallel = TRUE,
                         file_type = "csv",
                         vars_sim = list(
                           lke_lvl = list(var = "LKE_lvlwtr",
                                          month = 1:12,
                                          depth_range = c(0, 2)
                           )
                         )
  )

  # Run sensitivity analysis AEME model
  sim_id <- sa_aeme(aeme = aeme, path = path, param = param,
                    model = model, ctrl = ctrl, FUN_list = FUN_list)

  sa_res <- read_sa(ctrl = ctrl, sim_id = sim_id, R = 2^2)

  testthat::expect_true(is.data.frame(sa_res[[1]]$df))

  p1 <- plot_uncertainty(sa_res)
  testthat::expect_true(ggplot2::is_ggplot(p1))

  p2 <- plot_scatter(sa_res)
  testthat::expect_true(ggplot2::is_ggplot(p2))

  pl1 <- plot_multiscatter(sa_res)
  testthat::expect_true(is.list(pl1))
  testthat::expect_true(ggplot2::is_ggplot(pl1[[1]][[1]]))

  pl2 <- plot_sobol(sa = sa_res, add_errorbars = TRUE, use_dummy = TRUE)
  testthat::expect_true(ggplot2::is_ggplot(pl2))

})

test_that("can execute sensitivity analysis for AEME-GOTM in parallel", {

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

  aeme <- AEME::run_aeme(aeme = aeme, model = model,
                         verbose = FALSE, path = path)

  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters |>
    dplyr::filter(file != "wdr")

  # Function to calculate fitness
  fit <- function(df) {
    mean(df$model)
  }
  fit2 <- function(df) {
    median(df$model, na.rm = TRUE)
  }

  FUN_list <- list(HYD_temp = fit, HYD_thmcln = fit2, LKE_lvlwtr = fit2)


  ctrl <- create_control(method = "sa", N = 2^2, ncore = 2, parallel = TRUE,
                         file_type = "db", file_name = "results.db",
                         vars_sim = list(
                           surf_temp = list(var = "HYD_temp",
                                            month = c(10:12, 1:3),
                                            depth_range = c(0, 2)
                           ),
                           bot_temp = list(var = "HYD_temp",
                                           month = c(10:12, 1:3),
                                           depth_range = c(10, 13)
                           ),
                           thm_cln = list(var = "HYD_thmcln",
                                          month = c(10:12, 1:3),
                                          depth_range = c(0, 13)
                           ),
                           lke_lvl = list(var = "LKE_lvlwtr",
                                          month = c(12, 1:2),
                                          depth_range = c(0, lke$depth)
                           )
                         )
  )

  # Run sensitivity analysis AEME model
  sim_id <- sa_aeme(aeme = aeme, path = path, param = param,
                    model = model, ctrl = ctrl, FUN_list = FUN_list)

  sa_res <- read_sa(ctrl = ctrl, sim_id = sim_id, R = 2^2)

  testthat::expect_true(is.data.frame(sa_res[[1]]$df))

  punc <- plot_uncertainty(sa_res)
  testthat::expect_true(ggplot2::is_ggplot(punc))

})

test_that("can execute sensitivity analysis for derived variables", {

  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  # Copy files from package into tempdir
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
  model_controls <- AEME::get_model_controls(use_bgc = TRUE)
  inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
  model <- c("glm_aed")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           inf_factor = inf_factor, ext_elev = 5,
                           use_bgc = TRUE)

  aeme <- AEME::run_aeme(aeme = aeme, model = model,
                         verbose = FALSE, path = path)

  lke <- AEME::lake(aeme)
  file_chk <- file.exists(file.path(path, paste0(lke$id, "_",
                                                 tolower(lke$name)),
                                    model, "output", "output.nc"))
  testthat::expect_true(file_chk)

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters |>
    dplyr::filter(file != "wdr")

  # Function to calculate fitness
  fit <- function(df) {
    mean(df$model)
  }
  sum_fit <- function(df) {
    sum(df$model)
  }

  FUN_list <- list(HYD_schstb = fit, HYD_thmcln = fit, CHM_oxynal = sum_fit)

  ctrl <- create_control(method = "sa", N = 2^2, ncore = 2, parallel = TRUE,
                         file_type = "db", file_name = "results.db",
                         vars_sim = list(
                           sch_stab = list(var = "HYD_schstb",
                                           month = c(10:12, 1:3),
                                           depth_range = c(0, 18)
                           ),
                           thermo_depth = list(var = "HYD_thmcln",
                                               month = c(10:12, 1:3),
                                               depth_range = c(0, 18)
                           ),
                           oxy_nal = list(var = "CHM_oxynal",
                                          month = c(10:12, 1:3),
                                          depth_range = c(0, 18)
                           )
                         )
  )

  # Run sensitivity analysis AEME model
  sim_id <- sa_aeme(aeme = aeme, path = path, param = param,
                    model = model, ctrl = ctrl, FUN_list = FUN_list)

  sa_res <- read_sa(ctrl = ctrl, sim_id = sim_id, boot = FALSE)

  testthat::expect_true(is.data.frame(sa_res[[1]]$df))

  punc <- plot_uncertainty(sa_res)
  testthat::expect_true(ggplot2::is_ggplot(punc))

})
