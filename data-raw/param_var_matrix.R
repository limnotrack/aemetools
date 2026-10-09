tmpdir <- tempdir()
aeme_dir <- system.file("extdata/lake/", package = "AEME")
# Copy files from package into tempdir
file.copy(aeme_dir, tmpdir, recursive = TRUE)
path <- file.path(tmpdir, "lake")
aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
model_controls <- AEME::get_model_controls()
model <- c("glm_aed")
path <- "aeme"
sed_param <- AEME::glm_sed_params(n_zones = 2, zone_heights = c(8, 15))
aeme <- AEME::add_param(aeme = aeme, param = sed_param)

aeme <- AEME::build_aeme(path = path, aeme = aeme,
                         model = model, model_controls = model_controls,
                         ext_elev = 5, use_bgc = TRUE)
# aeme <- AEME::run_aeme(aeme = aeme, path = path, model = model)
# AEME::plot_output(aeme, model = model, var_sim = "CHM_oxy")

glm_sed <- AEME::get_aed_sed_const2d_param(aeme, path) |> 
  dplyr::filter(
    !grepl("n_zones|active_zone", name)
  )

data("aeme_parameters", package = "AEME")
data("aeme_parameters_bgc", package = "AEME")

sed_param_cal <- sed_param |> 
  dplyr::filter(grepl("sed_temp_mean|sed_temp_peak_doy|sed_temp_amplitude", name))
param <- aeme_parameters |> 
  dplyr::bind_rows(aeme_parameters_bgc) |> 
  dplyr::filter(
    !grepl("sediment", name)
  ) |> 
  dplyr::filter(model == "glm_aed", 
                !grepl("zone_heights|sed_roughness|aed_sed_const2d|n_zones|inflow|benthic_mode|sed_heat_Ksoil|sed_temp_depth|sed_reflectivity", name),
                !duplicated(name)) |>
  dplyr::bind_rows(sed_param_cal) |>
  dplyr::bind_rows(glm_sed) |>
  # dplyr::select(dplyr::all_of(AEME::param_colnames(incl_opt = FALSE))) |> 
  as.data.frame()

vars_sim <- c("HYD_temp", "HYD_thmcln", "LKE_lvlwtr", "CHM_oxy", "PHY_tchla")

param_var_matrix <- create_param_var_matrix(param = param,
                                            vars_sim = vars_sim)

param_var_matrix <- edit_param_var_matrix(param_var_matrix)

usethis::use_data(param_var_matrix, overwrite = TRUE)
