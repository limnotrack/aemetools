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

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`. Defaults to all models if not found in
  `aeme`.

- param:

  dataframe; of parameters read in from a csv file. Requires the columns
  c("model", "file", "name", "value", "min", "max", "log")

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

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

  data.frame; model configuration, typically loaded via
  [`get_model_controls()`](https://limnotrack.com/reference/get_model_controls.html).

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
#> ✔ Created missing directory: D:\a\aemetools\aemetools\docs\reference\aeme
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows — this may be a placeholder
#>   value.
#> ℹ Check raw data or unit conversion for this variable.
#> 
#> ── Calculating water balance ──
#> 
#> Resolving water level
#>   ℹ Using observed water level
#> ! Missing values in observed water level
#> ℹ Estimating surface water temperature
#> ✔ Estimating surface water temperature [9ms]
#> 
#> Estimating lake water levels for glm_aed
#>   ℹ Optimizing parameters for water balance
#>   ✔ Optimization Complete: C = 0.3343, h_inv = 23.4915, Final RMSE = 0.1431
#> Estimating lake water levels for gotm_wet
#>   ℹ Optimizing parameters for water balance
#>   ✔ Optimization Complete: C = 0.334, h_inv = 23.4829, Final RMSE = 0.1472
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> 
#> ── Building GLM-AED for lake wainamu ──
#> 
#> ℹ Copied in GLM nml file
#> ℹ Copied in AED nml file and supporting files
#> ℹ Copied in GLM plots nml file
#> ℹ Building GOTM-WET model for lake wainamu
#> ℹ Copied in GOTM configuration files
#> ✔ GOTM YAML validation completed - no issues detected.
#> ✔ GLM nml validation completed - no issues detected.
#> ℹ Running models... (Have you tried parallelizing?) [2026-06-15 21:05:56]
#> → GLM-AED running... [2026-06-15 21:05:56]
#> ✔ GLM-AED run successful! [2026-06-15 21:05:56]
#> → GOTM-WET running... [2026-06-15 21:05:56]
#> ✔ GOTM-WET run successful! [2026-06-15 21:05:56]
#> ✔ Model run complete! [2026-06-15 21:05:56]
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
#> ℹ Please use `create_calib_control()` instead.
#> ℹ Use `create_calib_control()` when method = 'calib'. Use `create_sa_control()`
#>   when method = 'sa'.

vars_sim <- c("HYD_temp", "LKE_lvlwtr")
weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 1)

# Calibrate AEME model
sim_id <- calib_aeme(aeme = aeme, model = model, path = path,
                     param = param, FUN_list = FUN_list, ctrl = ctrl,
                     vars_sim = vars_sim, weights = weights)
#> ℹ Variables not found: `LKE_lvlwtr`.
#> Adding them to model_controls.
#> ℹ Extracting indices for "glm_aed" modelled variables [2026-06-15 21:05:57]
#> ✔ Indices extracted for "glm_aed" modelled variables [2026-06-15 21:05:58]
#> ℹ Using 2 cores for parallel calibration for "glm_aed".
#> → Starting generation 1/2, 10 members. [2026-06-15 21:05:59]
#> Parameter summary for generation 1:
#>        light/Kw MET_wndspd MET_radswd mixing/coef_mix_conv
#> mean      2.744     1.0070     1.0050              0.15040
#> median    2.707     0.9956     1.0190              0.15260
#> sd        1.646     0.1818     0.1966              0.02886
#>        mixing/coef_wind_stir mixing/coef_mix_shear mixing/coef_mix_turb
#> mean                 0.24870                0.1510               0.4499
#> median               0.24740                0.1534               0.4631
#> sd                   0.03003                0.0300               0.1528
#>        mixing/coef_mix_hyp outflow inflow
#> mean                0.5977  1.5150 1.4910
#> median              0.6030  1.4940 1.4430
#> sd                  0.1238  0.6027 0.5759
#> ✔ Completed generation 1/2 
#> for "glm_aed". [2026-06-15 21:06:16]
#> Best fit: 66 (sd: 2769.8) Parameters: [ 1.58, 1.14, 1.17, 0.12, 0.274, 0.185,
#> 0.215, 0.411, 0.629, and 0.679 ]
#> Writing output for generation 1 to results.db with sim ID: "45819_glmaed_C_001"
#> [2026-06-15 21:06:16]
#> ℹ Survival rate: 0.8
#> → Starting generation 2/2, 10 members. [2026-06-15 21:06:17]
#> Parameter summary for generation 2:
#>        light/Kw MET_wndspd MET_radswd mixing/coef_mix_conv
#> mean      2.604     1.0040      1.118              0.14740
#> median    2.937     0.9956      1.092              0.15920
#> sd        1.473     0.2387      0.117              0.03781
#>        mixing/coef_wind_stir mixing/coef_mix_shear mixing/coef_mix_turb
#> mean                 0.25290              0.190000               0.3296
#> median               0.24650              0.192300               0.3033
#> sd                   0.02539              0.007898               0.1356
#>        mixing/coef_mix_hyp outflow inflow
#> mean                0.4602  0.9744  1.001
#> median              0.4330  1.0910  1.048
#> sd                  0.1118  0.4034  0.350
#> Writing output for generation 2 to results.db with sim ID: "45819_glmaed_C_001"
#> [2026-06-15 21:06:25]
#> ✔ Completed generation 2/2 
#> for "glm_aed". [2026-06-15 21:06:25]
#> Best fit: 4.95 (sd: 4292.1)
#> ℹ Survival rate: 1
#> ℹ Extracting indices for "gotm_wet" modelled variables [2026-06-15 21:06:26]
#> ✔ Indices extracted for "gotm_wet" modelled variables [2026-06-15 21:06:27]
#> ℹ Using 2 cores for parallel calibration for "gotm_wet".
#> → Starting generation 1/2, 10 members. [2026-06-15 21:06:28]
#> Parameter summary for generation 1:
#>        turbulence/turb_param/k_min light_extinction/A/constant_value
#> mean                     5.090e-06                           0.52800
#> median                   5.019e-06                           0.52890
#> sd                       3.147e-06                           0.07763
#>        light_extinction/g1/constant_value light_extinction/g2/constant_value
#> mean                              0.59160                              1.329
#> median                            0.59210                              1.301
#> sd                                0.09194                              0.851
#>        MET_wndspd MET_radswd outflow inflow
#> mean       1.0030     1.0090  1.4860 1.4910
#> median     1.0190     1.0020  1.4740 1.4890
#> sd         0.1873     0.1836  0.5946 0.6027
#> ✔ Completed generation 1/2 
#> for "gotm_wet". [2026-06-15 21:06:46]
#> Best fit: 14 (sd: 13950) Parameters: [ 4.27e-06, 0.493, 0.601, 0.644, 0.711,
#> 1.23, 1.21, and 1.24 ]
#> Writing output for generation 1 to results.db with sim ID:
#> "45819_gotmwet_C_001" [2026-06-15 21:06:46]
#> ℹ Survival rate: 0.8
#> → Starting generation 2/2, 10 members. [2026-06-15 21:06:46]
#> Parameter summary for generation 2:
#>        turbulence/turb_param/k_min light_extinction/A/constant_value
#> mean                     2.313e-06                            0.5309
#> median                   2.338e-06                            0.5352
#> sd                       2.025e-06                            0.0619
#>        light_extinction/g1/constant_value light_extinction/g2/constant_value
#> mean                              0.58710                             1.2480
#> median                            0.58820                             1.1920
#> sd                                0.01494                             0.6213
#>        MET_wndspd MET_radswd outflow inflow
#> mean       0.9246     1.0130  0.9679 0.9958
#> median     0.9552     1.0490  0.9201 0.9602
#> sd         0.1575     0.1882  0.4447 0.4133
#> Writing output for generation 2 to results.db with sim ID:
#> "45819_gotmwet_C_001" [2026-06-15 21:06:56]
#> ✔ Completed generation 2/2 
#> for "gotm_wet". [2026-06-15 21:06:56]
#> Best fit: 14 (sd: 5418.6)
#> ℹ Survival rate: 0.9
```
