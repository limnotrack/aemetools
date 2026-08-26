test_that("can run AEME-GLM with parameters", {
  model <- c("glm_aed")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = FALSE)
  aeme <- cached$aeme
  path <- cached$path

  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  run_aeme_param(aeme = aeme, param = param, model = model, path = path)
  outfile <- AEME::get_model_outfile(aeme, path = path)
  file_chk <- sapply(outfile, file.exists)
  testthat::expect_true(all(file_chk))

  nc <- run_aeme_param(aeme = aeme, param = param,
                       model = model, path = path,
                       return_nc = TRUE)
  testthat::expect_true(is(nc, "ncdf4"))

  ncdf4::nc_close(nc)

})

test_that("can run funs return same fit", {

  vars_sim <- c("HYD_strat", "HYD_temp", "HYD_thmcln", "HYD_schstb",
                "CHM_oxycln", "CHM_oxynal",
                "NIT_tn", "PHS_tp", "PHY_tchla", "CAR_toc")
  model <- c("glm_aed")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = TRUE,
                                vars_sim = vars_sim, run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
  model_controls <- AEME::get_model_controls(use_bgc = TRUE)
  model_controls <- AEME::set_vars_sim(model_controls = model_controls,
                                 vars_sim = vars_sim)
  fit1 <- AEME::assess_model(aeme = aeme)

  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters
  # Update Sed params
  sed_param <- AEME::get_glm_sed_params(aeme = aeme) |>
    dplyr::mutate(
      min = dplyr::case_when(
        grepl("sed_temp_mean", name) ~ 8,
        .default = min
      ),
      max = dplyr::case_when(
        grepl("sed_temp_mean", name) ~ 22,
        .default = max
      )
    )

  param <- param |>
    dplyr::filter(!(name %in% sed_param$name)) |>
    dplyr::bind_rows(sed_param)

  AEME::parameters(aeme) <- param
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           ext_elev = 5,
                           use_bgc = TRUE) |> 
    AEME::run_aeme()
  fit2 <- AEME::assess_model(aeme = aeme, model = model, var_sim = "HYD_temp")


  aeme <- run_aeme_param(aeme = aeme, param = param, model = model, path = path,
                         return_aeme = TRUE)
  fit3 <- AEME::assess_model(aeme = aeme, model = model, var_sim = "HYD_temp")

  # MAE fun
  # Function to calculate fitness
  mae <- function(df) {
    O <- df$obs
    P <- df$model
    mean(abs(P - O), na.rm = TRUE)
  }

  vars_sim <- c("HYD_temp")
  weights <- c("HYD_temp" = 1)
  FUN_list <- list("HYD_temp" = mae)

  testthat::expect_equal(fit2$mae, fit3$mae)
  fit3_df <- AEME::get_var(aeme = aeme, model = model, var_sim = vars_sim,
                           use_obs = TRUE)

  lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)
  fit3_dir <- file.path(lake_dir, paste0(model, "_fit3"))
  dir.create(fit3_dir, showWarnings = FALSE, recursive = TRUE)
  file.copy(file.path(lake_dir, model), fit3_dir, recursive = TRUE)

  fit4 <- run_and_fit(aeme = aeme, param = param,
                      model = model, path = path, FUN_list = FUN_list,
                      vars_sim = vars_sim, weights = weights,
                      return_indices = FALSE,
                      include_wlev = FALSE,
                      fit = TRUE)
  testthat::expect_equal(round(fit3$mae, 2), round(fit4$HYD_temp, 2))

  test_vars <- c("HYD_thmcln", "CHM_oxynal", "LKE_tli4")
  v = test_vars[1]
  for (v in test_vars) {
    weights <- c(1)
    names(weights) <- c(v)
    FUN_list <- list()
    FUN_list[[v]] <- mae
    fit_tmp <- run_and_fit(aeme = aeme, param = param,
                           model = model, path = path, FUN_list = FUN_list,
                           vars_sim = c(v), weights = weights,
                           return_indices = FALSE,
                           include_wlev = TRUE,
                           fit = TRUE)
    testthat::expect_true(!is.na(fit_tmp[[v]]))
    testthat::expect_true(!is.na(fit_tmp$LKE_lvlwtr))
  }

  weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 1)
  FUN_list$HYD_temp <- mae
  FUN_list$LKE_lvlwtr <- mae

  fit5 <- run_and_fit(aeme = aeme, param = param,
                      model = model, path = path, FUN_list = FUN_list,
                      vars_sim = vars_sim, weights = weights,
                      return_indices = FALSE,
                      include_wlev = TRUE,
                      fit = TRUE)
  testthat::expect_equal(round(fit3$mae, 2), round(fit5$HYD_temp, 2))
})

test_that("can calibrate with backwards compatibility", {
  model <- c("glm_aed")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = FALSE)
  aeme <- cached$aeme
  path <- cached$path

  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters
  ctrl <- create_control(method = "calib", NP = 10, itermax = 20,
                         ncore = getOption("ncore"),
                         parallel = TRUE, file_type = "db",
                         file_name = "results.db")

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model, ctrl = ctrl,
                       vars_sim = vars_sim)

  # calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  calib <- read_calib(file_name = ctrl$file_name, file_dir = ctrl$file_dir,
                      file_type = "db", type = "calib", sim_id = sim_id)
  testthat::expect_true("time_elapsed" %in% names(calib$calibration_metadata))
  testthat::expect_true(is.list(calib))

  psum <- plot_calib_summary(calib = calib)
  testthat::expect_true(ggplot2::is_ggplot(psum))

  plist <- plot_calib(calib = calib, fit_col = "LKE_lvlwtr")

  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))

})

test_that("can calibrate temperature for AEME-GLM in series with DB output", {
  model <- c("glm_aed")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = FALSE)
  aeme <- cached$aeme
  path <- cached$path

  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters
  ctrl <- create_calib_control(NP = 10, itermax = 20, ncore = getOption("ncore"),
                               parallel = FALSE, file_type = "db",
                               file_name = "results.db")

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model, ctrl = ctrl,
                       vars_sim = vars_sim)

  # calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  calib <- read_calib(file_name = ctrl$file_name, file_dir = ctrl$file_dir,
                      file_type = "db", type = "calib", sim_id = sim_id)
  testthat::expect_true("time_elapsed" %in% names(calib$calibration_metadata))
  testthat::expect_true(is.list(calib))

  psum <- plot_calib_summary(calib = calib)
  testthat::expect_true(ggplot2::is_ggplot(psum))

  plist <- plot_calib(calib = calib, fit_col = "LKE_lvlwtr")

  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))

})

test_that("can calibrate lake level only for AEME-GLM in parallel", {
  model <- c("glm_aed")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
  # AEME::plot(aeme, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, file.exists)
  testthat::expect_true(all(file_chk))

  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- create_calib_control(VTR = -Inf, NP = 10, itermax = 20,
                               reltol = -Inf, cutoff = 0.25, mutate = 0.1,
                               parallel = TRUE, file_type = "csv",
                               na_value = 999, ncore = getOption("ncore"))

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  sim_times <- get_simulation_time(aeme = aeme, model = model, path = path,
                                   param = param, FUN_list = FUN_list,
                                   vars_sim = vars_sim, weights = weights)

  ctrl$timeout <- 0.01
  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  testthat::expect_true(is.list(calib))
  testthat::expect_true(all(is.na(calib$simulation_data$fit_value)))

  # Calibrate AEME model
  ctrl$timeout <- 2
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)

  testthat::expect_true(is.list(calib))

  plist <- plot_calib(calib = calib)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))

})

test_that("can calibrate sediment parameters only for AEME-GLM", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- AEME::get_model_controls()
  model <- c("glm_aed")
  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters |>
    dplyr::filter(module != "sediment")
  sed_param <- AEME::glm_sed_params(n_zones = 3, zone_heights = c(5, 10, 14),
                                    sed_temp_mean = c(12, 14, 18))
  param <- dplyr::bind_rows(param, sed_param)

  aeme <- AEME::add_param(aeme = aeme, param = param)

  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           ext_elev = 5, use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model, path = path)
  aeme <- AEME::run_aeme(aeme = aeme)
  AEME::plot_output(aeme)
  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, file.exists)
  testthat::expect_true(all(file_chk))


  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- create_calib_control(VTR = -Inf, NP = 10, itermax = 20,
                               reltol = 0.07, cutoff = 0.25, mutate = 0.1,
                               parallel = TRUE, file_type = "csv",
                               na_value = 999, ncore = getOption("ncore"))

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 1)

  # remove n_zones and zone_heights from calibration
  param <- param |>
    dplyr::filter(!grepl("n_zones|benthic_mode|zone_heights", name))
  tst_param <- param |>
    dplyr::mutate(
      value = dplyr::case_when(
        name == "sediment/sed_temp_mean" ~ 10,
        TRUE ~ value)
    )
  tst <- run_aeme_param(aeme = aeme, param = param, model = model, path = path)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path, param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)

  testthat::expect_true(is.list(calib))

  plist <- plot_calib(calib = calib)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))

})

test_that("can calibrate lake level w/ scaling outflow and level from wbal only for AEME-GLM in parallel", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  obs <- AEME::observations(aeme)
  obs$lake <- NULL
  obs$level <- NULL
  AEME::observations(aeme) <- obs
  model_controls <- AEME::get_model_controls()
  model <- c("glm_aed")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           ext_elev = 5, use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model,
                         verbose = FALSE, path = path)
  # AEME::plot(aeme, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, file.exists)
  testthat::expect_true(all(file_chk))

  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  param <- aeme_parameters[aeme_parameters$name == "outflow", ]

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    mean(abs(O - P))
    # -1 * (cor(x = O, y = P, method = "pearson") -
    #         (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- create_calib_control(VTR = -Inf, NP = 10, itermax = 20,
                               reltol = 0.07, cutoff = 0.25, mutate = 0.1,
                               parallel = TRUE, file_type = "csv",
                               na_value = 999, ncore = getOption("ncore"))

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  fit <- run_and_fit(aeme = aeme, param = param,
                     model = model, path = path, FUN_list = FUN_list,
                     vars_sim = vars_sim, weights = weights,
                     return_indices = FALSE,
                     include_wlev = TRUE,
                     fit = TRUE)

  testthat::expect_true(fit$LKE_lvlwtr < 0.25)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)

  testthat::expect_true(is.list(calib))

  plist <- plot_calib(calib = calib)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))

})

test_that("can calibrate lake level w/ scaling outflow and level from wbal (filtered params) only for AEME-GLM in parallel", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  obs <- AEME::observations(aeme)
  obs$lake <- NULL
  obs$level <- NULL
  AEME::observations(aeme) <- obs
  model_controls <- AEME::get_model_controls()
  model <- c("glm_aed")
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           ext_elev = 5, use_bgc = FALSE)
  aeme <- AEME::run_aeme(aeme = aeme, model = model,
                         verbose = FALSE, path = path)
  # AEME::plot(aeme, model = model, path = path, plot = "calib",
  #            obs = "temp", save = FALSE, show = FALSE)
  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, file.exists)
  testthat::expect_true(all(file_chk))

  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters |>
    dplyr::filter(model %in% model & grepl("wdr|inf", file)) |>
    dplyr::mutate(min = 0, max = 2.5)

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    mean(abs(O - P))
    # -1 * (cor(x = O, y = P, method = "pearson") -
    #         (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- create_calib_control(VTR = -Inf, NP = 10, itermax = 20,
                               reltol = 0.07, cutoff = 0.25, mutate = 0.1,
                               parallel = TRUE, file_type = "csv",
                               na_value = 999, ncore = getOption("ncore"))

  vars_sim <- c("LKE_lvlwtr")
  weights <- c("LKE_lvlwtr" = 1)

  fit <- run_and_fit(aeme = aeme, param = param,
                     model = model, path = path, FUN_list = FUN_list,
                     vars_sim = vars_sim, weights = weights,
                     return_indices = FALSE,
                     include_wlev = TRUE,
                     fit = TRUE)

  testthat::expect_true(fit$LKE_lvlwtr < 0.25)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)

  get_param(calib = calib, best = TRUE)

  testthat::expect_true(is.list(calib))

  plist <- plot_calib(calib = calib)
  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))

})

test_that("can calibrate temperature with LHC for AEME-GLM in series with DB output", {
  model <- c("glm_aed")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = FALSE)
  aeme <- cached$aeme
  path <- cached$path

  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  mae <- function(df) {
    mean(abs(df$obs - df$model))
  }

  FUN_list <- list(HYD_temp = mae, LKE_lvlwtr = fit)

  ctrl <- create_calib_control(NP = 10, itermax = 20, ncore = getOption("ncore"),
                               parallel = FALSE, file_type = "db",
                               file_name = "results.db", c_method = "LHC")

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 0.5)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  testthat::expect_true(is.list(calib))

  calib_meta <- read_calib_meta(file = ctrl$file_name, file_dir = ctrl$file_dir)
  testthat::expect_true(is.data.frame(calib_meta))


  psum <- plot_calib_summary(calib = calib)
  testthat::expect_true(ggplot2::is_ggplot(psum))

  plist <- plot_calib(calib = calib, fit_col = "LKE_lvlwtr")

  testthat::expect_true(is.list(plist))

  testthat::expect_true(all(sapply(plist, ggplot2::is_ggplot)))

})

test_that("can update bgc parameters for GLM-AED", {
  vars_sim <- c("PHY_tchla")
  model <- c("glm_aed")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = TRUE,
                                vars_sim = vars_sim, run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
  model_controls <- AEME::get_model_controls(use_bgc = TRUE) # fails if F, need to inspect further
  model_controls <- AEME::set_vars_sim(model_controls, vars_sim = vars_sim)

  # Get parameters for calibration
  data("aeme_parameters", package = "AEME")
  phy_param <- AEME::get_aeme_parameters(model = model,
                                         file = c(#"fabm.yaml",
                                           "aed_phyto_pars.csv"),
                                         module = "phytoplankton") |>
    dplyr::filter(grepl("p_initial|p0|Xcc|R_growth|theta_growth|T_std|T_opt|
                        T_max|R_resp|theta_resp|k_fres", name))

  sed_param <- AEME::get_aeme_parameters(model = model,
                                         file = c("aed.nml"),
                                         module = "sed_const2d")
  param <- dplyr::bind_rows(aeme_parameters, phy_param)

  # Update Sed params
  sed_param <- AEME::get_glm_sed_params(aeme = aeme) |>
    dplyr::mutate(
      min = dplyr::case_when(
        grepl("sed_temp_mean", name) ~ 8,
        .default = min
      ),
      max = dplyr::case_when(
        grepl("sed_temp_mean", name) ~ 22,
        .default = max
      )
    )

  param <- param |>
    dplyr::filter(!(name %in% sed_param$name)) |>
    dplyr::bind_rows(sed_param)

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    mean(abs(P - O))
  }
  FUN_list <- list(PHY_tchla = fit)

  ctrl <- create_calib_control(NP = 10, itermax = 20, ncore = getOption("ncore"),
                               parallel = F, file_type = "db", na_value = 999,
                               file_name = "results.db", c_method = "LHC",
                               timeout = 5)

  run_and_fit(aeme = aeme, param = param, model = model, vars_sim = vars_sim,
              path = path, model_controls = model_controls, FUN_list = FUN_list)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim)

  calib <- read_calib(sim_id = sim_id, ctrl = ctrl)

  testthat::expect_true(is.list(calib))

  plist <- plot_calib(calib = calib, fit_col = "PHY_tchla")

  testthat::expect_true(is.list(plist))

  best_pars <- get_param(calib = calib, best = TRUE)
  best_fit <- best_pars |>
    dplyr::group_by(model) |>
    dplyr::summarise(fit = min(fit_value), .groups = "drop")

  aeme <- update_param(calib = calib, aeme = aeme)
  upd_param <- AEME::parameters(aeme)
  upd_param$param_name <- paste0(upd_param$model, "/", upd_param$group, "/", upd_param$name)
  upd_param2 <- update_param(calib = calib) |>
    dplyr::mutate(param_name = encode_param(group, name, index)) |>
    dplyr::arrange(match(upd_param$param_name, param_name))
  # upd_param2$param_name <- paste0(upd_param2$model, "/", upd_param2$group, "/", upd_param2$name)
  # upd_param2 <- upd_param2[match(upd_param$param_name, upd_param2$param_name), ]
  testthat::expect_true(all(upd_param$value %in% upd_param2$value))
  # testthat::expect_true(all(upd_param$min == upd_param2$min))
  # testthat::expect_true(all(upd_param$max == upd_param2$max))

  aeme <- AEME::add_param(aeme = aeme, param = upd_param2)
  aeme <- AEME::build_aeme(path = path, aeme = aeme,
                           model = model, model_controls = model_controls,
                           ext_elev = 5, use_bgc = TRUE)
  aeme <- AEME::run_aeme(aeme = aeme, path = path, model = model)

  aeme <- AEME::remove_param(aeme)
  aeme <- run_aeme_param(aeme = aeme, param = upd_param2, model = model,
                         path = path, return_aeme = TRUE, parallel = TRUE)
  # df <- AEME::get_var(aeme = aeme, model = model, var_sim = vars_sim,
  #                     return_df = TRUE, use_obs = TRUE)
  # AEME::plot_output(aeme, var_sim = vars_sim)
  mod_fit <- AEME::assess_model(aeme = aeme, model = model, var_sim = vars_sim)

  testthat::expect_true(all(best_pars$parameter_value %in% upd_param$value))
})

test_that("can write csv output to database", {
  model <- c("glm_aed")
  cached <- get_cached_aeme_run(model = model, ext_elev = 5, use_bgc = FALSE,
                                run = TRUE)
  aeme <- cached$aeme
  path <- cached$path
  # AEME::plot(aeme, model = model)
  outfile <- AEME::get_model_outfile(aeme, model)
  file_chk <- sapply(outfile, file.exists)
  testthat::expect_true(all(file_chk))

  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters

  # Function to calculate fitness
  fit <- function(df) {
    O <- df$obs
    P <- df$model
    -1 * (cor(x = O, y = P, method = "pearson") -
            (mean(abs(O - P)) / (max(O) - min(O))))
  }
  FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

  ctrl <- create_calib_control(NP = 10, itermax = 20, ncore = getOption("ncore"),
                               parallel = TRUE, file_type = "csv")

  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 10, "LKE_lvlwtr" = 1)

  # Calibrate AEME model
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)

  testthat::expect_true(is.list(calib))

  db_file <- write_csv_to_db(file_dir = ctrl$file_dir,
                             file_name = "csv2db.db")

  testthat::expect_true(file.exists(db_file))

})
