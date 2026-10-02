# --- Config ---
options(ncore = 2L)

test_that("can calibrate with param_var_matrix for AEME-GLM in parallel", {
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- tempdir()
  model_controls <- AEME::get_model_controls(use_bgc = TRUE)
  model <- c("glm_aed")
  path <- "aeme"
  sed_param <- AEME::glm_sed_params(n_zones = 2, zone_heights = c(8, 15))
  aeme <- aeme |> 
    AEME::add_param(param = sed_param) |> 
    AEME::build_aeme(path = path, 
                     model = model, model_controls = model_controls,
                     ext_elev = 5, use_bgc = TRUE)
  cfg <- AEME::configuration(aeme)
  cfg$glm_aed$hydrodynamic$sediment$n_zones
  AEME::get_glm_sed_zones(aeme, path)
  AEME::set_aed_sed_const2d(aeme = aeme, path = path)
  
  
  aeme <- AEME::run_aeme(aeme = aeme)
  AEME::plot_output(aeme, model = model, var_sim = "CHM_oxy")
  AEME::plot_output(aeme, model = model, var_sim = "PHY_tchla")
  
  glm_sed <- AEME::get_aed_sed_const2d_param(aeme, path) |> 
    dplyr::filter(
      !grepl("n_zones|active_zone", name)
    )
  
  # Get parameters for calibration
  sed_param_cal <- sed_param |> 
    dplyr::filter(grepl("sed_temp_mean|sed_temp_peak_doy|sed_temp_amplitude", name))
  
  data("aeme_parameters", package = "AEME")
  data("aeme_parameters_bgc", package = "AEME")
  param <- aeme_parameters |> 
    dplyr::bind_rows(aeme_parameters_bgc) |> 
    dplyr::filter(
      !grepl("sediment", name)
    ) |> 
    dplyr::filter(model %in% c("glm_aed"), !grepl("zone_heights|sed_roughness|aed_sed_const2d|n_zones|inflow|benthic_mode|sed_heat_Ksoil|sed_temp_depth|sed_reflectivity", name),
                  !duplicated(name)) |>
    dplyr::bind_rows(sed_param_cal) |>
    dplyr::bind_rows(glm_sed) |>
    # dplyr::select(dplyr::all_of(AEME::param_colnames(incl_opt = FALSE))) |> 
    as.data.frame()
  
  vars_sim <- c("HYD_temp", "HYD_thmcln", "LKE_lvlwtr", "CHM_oxy", "PHY_tchla")
  FUN_list <- list(HYD_temp = kge_loss, HYD_thmcln = kge_loss,
                   LKE_lvlwtr = kge_loss, CHM_oxy = kge_loss,
                   PHY_tchla = kge_loss)
  
  ctrl <- create_calib_control(NP = 40, itermax = 200,
                               ncore = 5,
                               parallel = TRUE, file_type = "db",
                               na_value = 999,
                               cutoff = 0.5, cutoff_final = 0.15,
                               mutate = 0.05, mutate_final = 0.2,
                               c_method = "MOEDA",
                               file_name = "results.db")
  
  weights <- set_weights(vars_sim = vars_sim)
  
  data("param_var_matrix", package = "aemetools")
  param_var_matrix_err <- param_var_matrix |> 
    dplyr::mutate(HYD_temp = FALSE)
  testthat::expect_error({
    # Calibrate AEME model
    sim_id <- calib_aeme(aeme = aeme, path = path,
                         param = param, model = model,
                         FUN_list = FUN_list, ctrl = ctrl,
                         vars_sim = vars_sim, weights = weights,
                         param_var_matrix = param_var_matrix_err)
  })
  # param_var_matrix <- edit_param_var_matrix(param_var_matrix)
  
  # Calibrate AEME model
  sim_id_pvm <- calib_aeme(aeme = aeme, path = path,
                           param = param, model = model,
                           FUN_list = FUN_list, ctrl = ctrl,
                           vars_sim = vars_sim, weights = weights,
                           param_var_matrix = param_var_matrix)
  
  # c_method = "MOEDA" requires param_var_matrix, so this comparison
  # (calibrating the same problem *without* param_var_matrix) needs its own
  # control object reverted to the default method.
  ctrl_default <- ctrl
  ctrl_default$c_method <- "CMAES"
  sim_id <- calib_aeme(aeme = aeme, path = path,
                       param = param, model = model,
                       FUN_list = FUN_list, ctrl = ctrl_default,
                       vars_sim = vars_sim, weights = weights)

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id_pvm)
  plist <- plot_calib(calib = calib)
  plist$dotty
  p <- plot_pareto_generations(calib)
  testthat::expect_true(ggplot2::is_ggplot(p))
  
  testthat::expect_true(is.list(calib))
  
  param_wide <- calib$simulation_data |>
    dplyr::filter(fit_type == "fit") |>
    tidyr::pivot_wider(id_cols = c("gen", "run"), names_from = parameter_name,
                       values_from = parameter_value)
  
  testthat::expect_true(is.data.frame(param_wide))
  testthat::expect_true(all(param_wide$`NA/aed_sed_const2d/fsed_oxy[1]` <= param_wide$`NA/aed_sed_const2d/fsed_oxy[2]`))
  testthat::expect_true(all(param_wide$`NA/sediment/sed_temp_mean[1]` <= param_wide$`NA/sediment/sed_temp_mean[2]`))
  ptemp <- plot_calib(calib = calib, 
                      fit_col = "HYD_temp")
  poxy <- plot_calib(calib = calib, 
                     fit_col = "CHM_oxy")
  pstrat <- plot_calib(calib = calib, 
                       fit_col = "HYD_thmcln")
  pfit <- plot_calib(calib = calib)
  
  pfit$convergence
  
  
  plot_calib_summary(calib = calib)
  # pstrat$dotty
  # ptemp$dotty
  # poxy$dotty
  pfit$dotty
  
  testthat::expect_true(is.list(ptemp))
  
  best_params <- get_param(calib, fit_col = "fit", 
                           best = TRUE)
  best_params |> 
    print()
  
  aeme <- run_aeme_param(aeme = aeme, path = path,
                         param = best_params, model = model,
                         return_aeme = TRUE)
  AEME::plot_output(aeme)
  AEME::plot_output(aeme, var_sim = "CHM_oxy")
  AEME::plot_output(aeme, var_sim = "HYD_thmcln")
  AEME::plot_output(aeme, var_sim = "PHY_tchla")
  AEME::assess_model(aeme = aeme)
  
  
})
