test_that("can run_and_fit sensitivity analysis for AEME-GLM", {

  model <- c("glm_aed")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
  inp <- AEME::input(aeme)

  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, file.exists)
  testthat::expect_true(all(file_chk))

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
  ctrl <- create_sa_control(N = 2^2,
                            file_type = "db", file_name = db_file,
                            na_value = 999, ncore = 2L,
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

  vars_sim <- sapply(ctrl$vars_sim, \(v) v$var) |>
    unique()

  out <- run_and_fit(aeme = aeme, path = path, param = param, method = "sa",
                     model = model, sa_ctrl = ctrl, FUN_list = FUN_list,
                     weights = weights, vars_sim = vars_sim)
  testthat::expect_true(is.list(out))
  na_chk <- sapply(out, function(x) !is.na(x)) |>
    all()
  testthat::expect_true(na_chk)
})

test_that("can execute sensitivity analysis for AEME-DYRESM in parallel", {

  model <- c("dy_cd")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
  # AEME::plot(aeme, model = model)
  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, file.exists)
  testthat::expect_true(all(file_chk))
  
  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters |>
    dplyr::filter(file != "wdr")
  
  # Function to calculate fitness
  fit <- function(df) {
    mean(df$model)
  }
  
  FUN_list <- list(HYD_temp = fit)
  
  ctrl <- create_sa_control(N = 2^1, ncore = 2L, parallel = TRUE,
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

test_that("can execute sensitivity analysis with old fun", {

  model <- c("glm_aed")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
  inp <- AEME::input(aeme)

  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, file.exists)
  testthat::expect_true(all(file_chk))

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
                         na_value = 999, ncore = 2L,
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
  
  # Run sensitivity analysis AEME model
  sim_id <- sa_aeme(aeme = aeme, path = path, param = param, model = model,
                    ctrl = ctrl, FUN_list = FUN_list)
  
  sim_meta <- read_simulation_meta(ctrl = ctrl)
  sim_meta2 <- read_simulation_meta(ctrl = ctrl, type = "sa")
  sa_res3 <- read_sa(file_name = ctrl$file_name, file_dir = ctrl$file_dir,
                     sim_id = sim_id, boot = FALSE)
  testthat::expect_true(all(sim_meta2$type == "sa"))
  testthat::expect_equal(sa_res3, read_sa(ctrl = ctrl, sim_id = sim_id,
                                          boot = FALSE))
  
})

test_that("can execute sensitivity analysis for AEME-GLM in parallel", {

  model <- c("glm_aed")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
  inp <- AEME::input(aeme)

  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, file.exists)
  testthat::expect_true(all(file_chk))
  
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
  ctrl <- create_sa_control(N = 2^2,
                            file_type = "db", file_name = db_file,
                            na_value = 999, ncore = 2L,
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
  
  # Run sensitivity analysis AEME model
  sim_id <- sa_aeme(aeme = aeme, path = path, param = param, model = model,
                    ctrl = ctrl, FUN_list = FUN_list)
  
  sim_meta <- read_simulation_meta(ctrl = ctrl)
  sim_meta2 <- read_simulation_meta(ctrl = ctrl, type = "sa")
  sa_res3 <- read_sa(file_name = ctrl$file_name, file_dir = ctrl$file_dir,
                     sim_id = sim_id, boot = FALSE)
  testthat::expect_true(all(sim_meta2$type == "sa"))
  testthat::expect_equal(sa_res3, read_sa(ctrl = ctrl, sim_id = sim_id,
                                          boot = FALSE))
  
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

  model <- c("glm_aed")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, file.exists)
  testthat::expect_true(all(file_chk))

  utils::data("aeme_parameters", package = "AEME")
  param <- aeme_parameters |>
    dplyr::filter(file != "wdr")

  # Function to calculate fitness
  fit <- function(df) {
    mean(df$model)
  }

  FUN_list <- list(LKE_lvlwtr = fit)
  
  ctrl <- create_sa_control(N = 2^2, ncore = 2L, parallel = TRUE,
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

  model <- c("gotm_wet")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, file.exists)
  testthat::expect_true(all(file_chk))

  # lke$depth is used below when setting up ctrl's depth_range.
  lke <- AEME::lake(aeme)

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
  
  
  ctrl <- create_sa_control(N = 2^2, ncore = 2L, parallel = TRUE,
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

  model <- c("glm_aed")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = TRUE,
                                run = TRUE)
  aeme <- cached$aeme
  path <- cached$path

  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, file.exists)
  testthat::expect_true(all(file_chk))
  
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
  
  ctrl <- create_sa_control(N = 2^2, ncore = 2L, parallel = TRUE,
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
