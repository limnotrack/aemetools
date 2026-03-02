# Calibrate AEME model parameters using observations

`calib_model()` runs the model and compares it against observations
provided. It can run in parallel by using multiple cores availlable on
your computer to run quicker.

## Usage

``` r
calib_aeme(
  aeme,
  model,
  param,
  path,
  vars_sim = "HYD_temp",
  FUN_list,
  weights,
  model_controls = NULL,
  ctrl = NULL,
  param_var_matrix = NULL,
  param_df = NULL
)
```

## Arguments

- aeme:

  aeme; object.

- model:

  vector; of models to be used. Can be \`dy_cd\`, \`glm_aed\`,
  \`gotm_wet\`.

- param:

  dataframe; of parameters read in from a csv file. Requires the columns
  c("model", "file", "name", "value", "min", "max", "log")

- path:

  filepath; where input files are located relative to the current
  working directory.

- vars_sim:

  vector; of variables names to be used in the calculation of model fit.

- FUN_list:

  list of functions; named according to the variables in the `vars_sim`.
  Funtions are of the form `function(df)` which will be used to
  calculate model fit. If nor provided, uses mean absolute error (MAE).

- weights:

  a named vector; of weights for each variable in vars_sim. If not
  provided, defaults to 1 for each variable.

- model_controls:

  dataframe; of configuration loaded from "model_controls.csv".

- ctrl:

  list; of controls for sensitivity analysis function created using the
  [`create_control`](create_control.md) function. See
  [create_control](create_control.md) for more details.

- param_var_matrix:

  list of dataframes; with parameters as rows and response variables as
  columns. Created using
  [`create_param_var_matrix`](create_param_var_matrix.md). This is used
  to specify which parameters are associated with which response
  variables, and therefore which parameters are updated in each
  generation of the calibration.

- param_df:

  dataframe; of parameters to be used in the calibration. Requires the
  columns c("model", "file", "name", "value", "min", "max"). This is
  used to restart from a previous calibration.

## Value

string of simulation id to be used to read the simulation output.

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
model_controls <- AEME::get_model_controls()
model <- c("glm_aed", "gotm_wet")
path <- "aeme"
aeme <- AEME::build_aeme(aeme = aeme, model = model, path = path,
                         model_controls = model_controls, ext_elev = 5) |>
  AEME::run_aeme()
#> Created missing directory: D:\a\aemetools\aemetools\docs\reference\aeme
#> ℹ Using observed water level
#> ℹ No missing values in observed water level. Using observed water level
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED for lake wainamu
#> ℹ Copied in GLM nml file
#> ℹ Copied in AED nml file and supporting files
#> ℹ Building GOTM-WET model for lake wainamu
#> ℹ Copied in GOTM configuration files
#> ✔ GOTM YAML validation completed - no issues detected.
#> ✔ GLM nml validation completed - no issues detected.
#> ℹ Running models... (Have you tried parallelizing?) [2026-03-02 21:48:18]
#> → GLM-AED running... [2026-03-02 21:48:18]
#> ✔ GLM-AED run successful! [2026-03-02 21:48:18]
#> → GOTM-WET running... [2026-03-02 21:48:18]
#> ✔ GOTM-WET run successful! [2026-03-02 21:48:19]
#> ✔ Model run complete! [2026-03-02 21:48:19]
#> ! The following variables are not available in model gotm_wet: RAD_extc
#> ! The following variables are not available in model gotm_wet: RAD_extc

data("aeme_parameters", package = "AEME")
param <- aeme_parameters

# Function to calculate fitness
nse <- function(df) {
# Calculate Nash-Sutcliffe Efficiency
  nse <- 1 - (sum((df$obs - df$model)^2) / sum((df$obs - mean(df$obs))^2))
  -1 * nse
}
FUN_list <- list(HYD_temp = nse, LKE_lvlwtr = nse)

ctrl <- create_control(method = "calib", NP = 10, itermax = 20, ncore = 2,
                       parallel = TRUE, file_type = "db",
                       file_name = "results.db")
#> Warning: `create_control()` was deprecated in aemetools 0.2.0.
#> ℹ Please use `create_calibration_control()` instead.
#> ℹ Use `create_calibration_control()` when method = 'calib'. Use
#>   `create_sa_control()` when method = 'sa'.

vars_sim <- c("HYD_temp", "LKE_lvlwtr")
weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 1)

# Calibrate AEME model
sim_id <- calib_aeme(aeme = aeme, model = model, path = path,
                     param = param, FUN_list = FUN_list, ctrl = ctrl,
                     vars_sim = vars_sim, weights = weights)
#> ℹ Variables not found: `LKE_lvlwtr`.
#> Adding them to model_controls.
#> ! The following parameters have the same value, min, 
#> and max and will not be updated during calibration: "sediment/n_zones"
#> Warning: No parameters in 'param' for gotm_wet.
#> ℹ Extracting indices for "glm_aed" modelled variables [2026-03-02 21:48:21]
#> ✔ Indices extracted for "glm_aed" modelled variables [2026-03-02 21:48:22]
#> ℹ Using 2 cores for parallel calibration for "glm_aed".
#> → Starting generation 1/2, 10 members. [2026-03-02 21:48:22]
#> Parameter summary for generation 1:
#>        light/Kw MET_wndspd MET_radswd mixing/coef_mix_conv
#> mean      2.744     1.0070     1.0050              0.15040
#> median    2.707     0.9956     1.0190              0.15260
#> sd        1.646     0.1818     0.1966              0.02886
#>        mixing/coef_wind_stir mixing/coef_mix_shear mixing/coef_mix_turb
#> mean                 0.24870                0.1510               0.4499
#> median               0.24740                0.1534               0.4631
#> sd                   0.03003                0.0300               0.1528
#>        mixing/coef_mix_hyp sediment/sed_temp_mean[1]
#> mean                0.5977                    12.090
#> median              0.6030                    11.960
#> sd                  0.1238                     3.616
#>        sediment/sed_temp_peak_doy[1] outflow inflow
#> mean                           45.08  1.4830 1.5210
#> median                         42.96  1.5030 1.5950
#> sd                             25.63  0.6226 0.5908
#> ✔ Completed generation 1/2 
#> for "glm_aed". [2026-03-02 21:48:38]
#> Best fit: 13.3 (sd: 3727.9) Parameters: [ 2.28, 0.985, 0.762, 0.162, 0.208,
#> 0.113, 0.279, 0.662, 9.4, 21.1, 1.25, and 1.23 ]
#> Writing output for generation 1 to results.db with sim ID: "45819_glmaed_C_001"
#> [2026-03-02 21:48:38]
#> ℹ Survival rate: 0.8
#> → Starting generation 2/2, 10 members. [2026-03-02 21:48:38]
#> Parameter summary for generation 2:
#>        light/Kw MET_wndspd MET_radswd mixing/coef_mix_conv
#> mean     2.8650    0.92930     1.0370             0.170000
#> median   2.8940    0.90430     1.0940             0.174100
#> sd       0.5826    0.04647     0.1229             0.009649
#>        mixing/coef_wind_stir mixing/coef_mix_shear mixing/coef_mix_turb
#> mean                 0.23580               0.14240              0.42110
#> median               0.24040               0.15090              0.45300
#> sd                   0.01907               0.02086              0.06646
#>        mixing/coef_mix_hyp sediment/sed_temp_mean[1]
#> mean               0.68520                    13.670
#> median             0.70420                    15.320
#> sd                 0.03347                     3.314
#>        sediment/sed_temp_peak_doy[1] outflow inflow
#> mean                           34.07  1.0340 0.9169
#> median                         39.53  0.9796 0.7954
#> sd                             10.15  0.1765 0.2444
#> Writing output for generation 2 to results.db with sim ID: "45819_glmaed_C_001"
#> [2026-03-02 21:48:44]
#> ✔ Completed generation 2/2 
#> for "glm_aed". [2026-03-02 21:48:45]
#> Best fit: 13.3 (sd: 1882.4)
#> ℹ Survival rate: 1
#> ℹ Extracting indices for "gotm_wet" modelled variables [2026-03-02 21:48:45]
#> ✔ Indices extracted for "gotm_wet" modelled variables [2026-03-02 21:48:46]
#> ℹ Using 2 cores for parallel calibration for "gotm_wet".
#> → Starting generation 1/2, 10 members. [2026-03-02 21:48:47]
#> Parameter summary for generation 1:
#>        turbulence/turb_param/k_min light_extinction/A/constant_value
#> mean                     4.954e-06                           0.52640
#> median                   5.230e-06                           0.52680
#> sd                       3.112e-06                           0.08243
#>        light_extinction/g1/constant_value light_extinction/g2/constant_value
#> mean                               0.5915                             1.3660
#> median                             0.5979                             1.4150
#> sd                                 0.0864                             0.8143
#>        MET_wndspd MET_radswd outflow inflow
#> mean       0.9928     1.0060  1.4810 1.4850
#> median     0.9830     1.0250  1.4900 1.4670
#> sd         0.1771     0.1824  0.6328 0.6032
#> ✔ Completed generation 1/2 
#> for "gotm_wet". [2026-03-02 21:49:05]
#> Best fit: 9.6 (sd: 17934) Parameters: [ 6.29e-06, 0.479, 0.467, 0.179, 1.19,
#> 1.16, 1.15, and 1.17 ]
#> Writing output for generation 1 to results.db with sim ID:
#> "45819_gotmwet_C_001" [2026-03-02 21:49:05]
#> ℹ Survival rate: 0.7
#> → Starting generation 2/2, 10 members. [2026-03-02 21:49:05]
#> Parameter summary for generation 2:
#>        turbulence/turb_param/k_min light_extinction/A/constant_value
#> mean                     4.604e-06                            0.4790
#> median                   4.744e-06                            0.4721
#> sd                       1.576e-06                            0.0490
#>        light_extinction/g1/constant_value light_extinction/g2/constant_value
#> mean                              0.51470                             0.4948
#> median                            0.50830                             0.4553
#> sd                                0.04436                             0.3026
#>        MET_wndspd MET_radswd outflow inflow
#> mean       1.0970    1.17700  1.3910 1.4510
#> median     1.1000    1.16900  1.3640 1.3650
#> sd         0.1017    0.03663  0.2247 0.2984
#> Writing output for generation 2 to results.db with sim ID:
#> "45819_gotmwet_C_001" [2026-03-02 21:49:15]
#> ✔ Completed generation 2/2 
#> for "gotm_wet". [2026-03-02 21:49:15]
#> Best fit: 9.49 (sd: 2496.4)
#> ℹ Survival rate: 1
```
