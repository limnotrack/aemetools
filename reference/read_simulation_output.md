# Read calibration output

Read calibration output

## Usage

``` r
read_simulation_output(
  ctrl = NULL,
  file_name,
  file_dir,
  file_type = "db",
  sim_id = NULL,
  type
)

read_calib(
  ctrl = NULL,
  file_name,
  file_dir,
  file_type = "db",
  sim_id = NULL,
  type
)
```

## Arguments

- ctrl:

  list; of controls for sensitivity analysis function created using the
  [`create_control`](create_control.md) function. See
  [create_control](create_control.md) for more details.

- file_name:

  The name of the output file. If `ctrl` is provided, this argument is
  ignored.

- file_dir:

  The directory of the output file. If `ctrl` is provided, this argument
  is ignored.

- file_type:

  string; file type to write the output to. Options are
  `c("csv", "db")`. Defaults to "db".

- sim_id:

  A vector of simulation IDs to read. If NULL, all simulations are read.

- type:

  A character string indicating the type of simulation. One of "calib",
  "sa", or "all". If missing, the type is inferred from the `ctrl`
  argument. If type is provided it overrides the `ctrl$method` value.

## Value

A list with the metadata and simulation data frames.

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
#> ✔ Estimating surface water temperature [7ms]
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
#> ℹ Building GOTM-WET model for lake wainamu
#> ✔ GOTM YAML validation completed - no issues detected.
#> ✔ GLM nml validation completed - no issues detected.
#> ℹ Running models... (Have you tried parallelizing?) [2026-06-16 05:27:12]
#> → GLM-AED running... [2026-06-16 05:27:12]
#> ✔ GLM-AED run successful! [2026-06-16 05:27:13]
#> → GOTM-WET running... [2026-06-16 05:27:13]
#> ✔ GOTM-WET run successful! [2026-06-16 05:27:13]
#> ✔ Model run complete! [2026-06-16 05:27:13]
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

vars_sim <- c("HYD_temp", "LKE_lvlwtr")
weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 1)

# Calibrate AEME model
sim_id <- calib_aeme(aeme = aeme, model = model, path = path,
                     param = param, FUN_list = FUN_list, ctrl = ctrl,
                     vars_sim = vars_sim, weights = weights)
#> ℹ Variables not found: `LKE_lvlwtr`.
#> Adding them to model_controls.
#> ℹ Extracting indices for "glm_aed" modelled variables [2026-06-16 05:27:14]
#> ✔ Indices extracted for "glm_aed" modelled variables [2026-06-16 05:27:15]
#> ℹ Using 2 cores for parallel calibration for "glm_aed".
#> → Starting generation 1/2, 10 members. [2026-06-16 05:27:16]
#> Parameter summary for generation 1:
#>        light/Kw MET_wndspd MET_radswd mixing/coef_mix_conv
#> mean      2.771     1.0040     0.9956              0.14960
#> median    2.563     0.9955     0.9888              0.14690
#> sd        1.655     0.1795     0.1833              0.03246
#>        mixing/coef_wind_stir mixing/coef_mix_shear mixing/coef_mix_turb
#> mean                 0.25030               0.14970               0.4417
#> median               0.24840               0.15350               0.4362
#> sd                   0.02996               0.03067               0.1525
#>        mixing/coef_mix_hyp outflow inflow
#> mean                0.6005  1.4710 1.4690
#> median              0.5977  1.5100 1.5050
#> sd                  0.1182  0.5938 0.6087
#> ✔ Completed generation 1/2 
#> for "glm_aed". [2026-06-16 05:27:31]
#> Best fit: 27.5 (sd: 3299.1) Parameters: [ 3.66, 1.08, 0.711, 0.101, 0.242,
#> 0.113, 0.561, 0.459, 0.584, and 0.583 ]
#> Writing output for generation 1 to results.db with sim ID: "45819_glmaed_C_003"
#> [2026-06-16 05:27:31]
#> ℹ Survival rate: 0.8
#> → Starting generation 2/2, 10 members. [2026-06-16 05:27:31]
#> Parameter summary for generation 2:
#>        light/Kw MET_wndspd MET_radswd mixing/coef_mix_conv
#> mean     3.3510     1.0310     0.9118              0.11560
#> median   3.6040     1.0720     0.8383              0.10810
#> sd       0.7859     0.0841     0.2244              0.01729
#>        mixing/coef_wind_stir mixing/coef_mix_shear mixing/coef_mix_turb
#> mean                  0.2321               0.13690               0.4558
#> median                0.2396               0.11760               0.5304
#> sd                    0.0154               0.03464               0.1640
#>        mixing/coef_mix_hyp outflow inflow
#> mean                0.5487  0.8769 0.9280
#> median              0.4797  0.6845 0.6614
#> sd                  0.1382  0.3899 0.4792
#> Writing output for generation 2 to results.db with sim ID: "45819_glmaed_C_003"
#> [2026-06-16 05:27:39]
#> ✔ Completed generation 2/2 
#> for "glm_aed". [2026-06-16 05:27:39]
#> Best fit: 6.4 (sd: 863.39)
#> ℹ Survival rate: 1
#> ℹ Extracting indices for "gotm_wet" modelled variables [2026-06-16 05:27:39]
#> ✔ Indices extracted for "gotm_wet" modelled variables [2026-06-16 05:27:41]
#> ℹ Using 2 cores for parallel calibration for "gotm_wet".
#> → Starting generation 1/2, 10 members. [2026-06-16 05:27:41]
#> Parameter summary for generation 1:
#>        turbulence/turb_param/k_min light_extinction/A/constant_value
#> mean                     4.939e-06                           0.52250
#> median                   4.785e-06                           0.52220
#> sd                       3.091e-06                           0.08191
#>        light_extinction/g1/constant_value light_extinction/g2/constant_value
#> mean                              0.59090                             1.3690
#> median                            0.59820                             1.4530
#> sd                                0.08894                             0.8391
#>        MET_wndspd MET_radswd outflow inflow
#> mean       0.9938     1.0010  1.4950 1.5010
#> median     0.9737     0.9877  1.5180 1.4610
#> sd         0.1830     0.1722  0.6088 0.5899
#> ✔ Completed generation 1/2 
#> for "gotm_wet". [2026-06-16 05:27:58]
#> Best fit: 937 (sd: 20559) Parameters: [ 4.04e-06, 0.613, 0.543, 0.588, 0.83,
#> 0.866, 2.21, and 2.08 ]
#> Writing output for generation 1 to results.db with sim ID:
#> "45819_gotmwet_C_003" [2026-06-16 05:27:58]
#> ℹ Survival rate: 0.7
#> → Starting generation 2/2, 10 members. [2026-06-16 05:27:58]
#> Parameter summary for generation 2:
#>        turbulence/turb_param/k_min light_extinction/A/constant_value
#> mean                     5.378e-06                           0.54500
#> median                   5.879e-06                           0.51500
#> sd                       2.225e-06                           0.07573
#>        light_extinction/g1/constant_value light_extinction/g2/constant_value
#> mean                              0.58260                             0.4119
#> median                            0.60160                             0.2657
#> sd                                0.04975                             0.3112
#>        MET_wndspd MET_radswd outflow inflow
#> mean       0.9382     1.0340  1.7830 1.9600
#> median     0.9609     1.1460  1.5950 1.8500
#> sd         0.1717     0.2115  0.4603 0.3688
#> Writing output for generation 2 to results.db with sim ID:
#> "45819_gotmwet_C_003" [2026-06-16 05:28:08]
#> ✔ Completed generation 2/2 
#> for "gotm_wet". [2026-06-16 05:28:08]
#> Best fit: 55.4 (sd: 6753.9)
#> ℹ Survival rate: 1
                     
# Read calibration output                      
calib <- read_calib(sim_id = sim_id, ctrl = ctrl)
```
