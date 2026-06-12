# Plot calibration results

Plot calibration results

## Usage

``` r
plot_calib(
  calib,
  fit_col = "fit",
  nrow = 2,
  base_size = 8,
  return_pars = FALSE,
  log_y = TRUE,
  na_value = deprecated()
)
```

## Arguments

- calib:

  A list with the calibration results loaded using
  [`read_calib`](read_simulation_output.md).

- fit_col:

  character; name of column containing fit values. Default is `"fit"`.

- nrow:

  integer; number of rows in plot

- base_size:

  numeric; base size for theme

- return_pars:

  logical; return parameter values

- log_y:

  logical; use log scale on y-axis. Default is `TRUE`.

- na_value:

  **\[deprecated\]** Numeric. Penalty value substituted for `NA` fit
  values, this is no longer needed as NA values are now written to
  simulation_data in output of calib_aeme() and sa_aeme(). The argument
  will be removed in a future version.

## Value

list of plots

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
#> ℹ Running models... (Have you tried parallelizing?) [2026-06-12 04:28:11]
#> → GLM-AED running... [2026-06-12 04:28:11]
#> ✔ GLM-AED run successful! [2026-06-12 04:28:11]
#> → GOTM-WET running... [2026-06-12 04:28:11]
#> ✔ GOTM-WET run successful! [2026-06-12 04:28:12]
#> ✔ Model run complete! [2026-06-12 04:28:12]
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
#> ℹ Extracting indices for "glm_aed" modelled variables [2026-06-12 04:28:13]
#> ✔ Indices extracted for "glm_aed" modelled variables [2026-06-12 04:28:14]
#> ℹ Using 2 cores for parallel calibration for "glm_aed".
#> → Starting generation 1/2, 10 members. [2026-06-12 04:28:15]
#> Parameter summary for generation 1:
#>        light/Kw MET_wndspd MET_radswd mixing/coef_mix_conv
#> mean      2.807     1.0000     0.9927              0.15040
#> median    2.845     0.9934     1.0150              0.15200
#> sd        1.531     0.1745     0.1880              0.03004
#>        mixing/coef_wind_stir mixing/coef_mix_shear mixing/coef_mix_turb
#> mean                 0.24990               0.14960               0.4450
#> median               0.24840               0.14840               0.4710
#> sd                   0.02969               0.03118               0.1532
#>        mixing/coef_mix_hyp outflow inflow
#> mean                0.6024  1.5160 1.5050
#> median              0.6122  1.5570 1.5060
#> sd                  0.1230  0.5903 0.6052
#> ✔ Completed generation 1/2 
#> for "glm_aed". [2026-06-12 04:28:32]
#> Best fit: 1420 (sd: 2675.9) Parameters: [ 2.7, 0.859, 0.886, 0.123, 0.218,
#> 0.112, 0.515, 0.637, 1.19, and 0.61 ]
#> Writing output for generation 1 to results.db with sim ID: "45819_glmaed_C_002"
#> [2026-06-12 04:28:32]
#> ℹ Survival rate: 0.6
#> → Starting generation 2/2, 10 members. [2026-06-12 04:28:32]
#> Parameter summary for generation 2:
#>        light/Kw MET_wndspd MET_radswd mixing/coef_mix_conv
#> mean     4.3280      1.208     1.1730               0.1496
#> median   4.4090      1.259     1.2460               0.1506
#> sd       0.7157      0.139     0.1884               0.0182
#>        mixing/coef_wind_stir mixing/coef_mix_shear mixing/coef_mix_turb
#> mean                  0.2386               0.12760              0.43260
#> median                0.2359               0.12720              0.45850
#> sd                    0.0266               0.02061              0.08927
#>        mixing/coef_mix_hyp outflow inflow
#> mean               0.71740  1.9810 2.0160
#> median             0.72050  2.0770 2.2670
#> sd                 0.03507  0.3487 0.6639
#> Writing output for generation 2 to results.db with sim ID: "45819_glmaed_C_002"
#> [2026-06-12 04:28:41]
#> ✔ Completed generation 2/2 
#> for "glm_aed". [2026-06-12 04:28:41]
#> Best fit: 28 (sd: 739.72)
#> ℹ Survival rate: 0.8
#> ℹ Extracting indices for "gotm_wet" modelled variables [2026-06-12 04:28:41]
#> ✔ Indices extracted for "gotm_wet" modelled variables [2026-06-12 04:28:42]
#> ℹ Using 2 cores for parallel calibration for "gotm_wet".
#> → Starting generation 1/2, 10 members. [2026-06-12 04:28:43]
#> Parameter summary for generation 1:
#>        turbulence/turb_param/k_min light_extinction/A/constant_value
#> mean                     5.038e-06                           0.52570
#> median                   5.175e-06                           0.52820
#> sd                       2.989e-06                           0.08054
#>        light_extinction/g1/constant_value light_extinction/g2/constant_value
#> mean                              0.59190                              1.393
#> median                            0.58270                              1.370
#> sd                                0.09116                              0.828
#>        MET_wndspd MET_radswd outflow inflow
#> mean       1.0030     1.0000  1.4720 1.5090
#> median     1.0070     0.9877  1.4180 1.5870
#> sd         0.1778     0.1822  0.6385 0.5825
#> ✔ Completed generation 1/2 
#> for "gotm_wet". [2026-06-12 04:29:02]
#> Best fit: 124 (sd: 19366) Parameters: [ 4.43e-06, 0.439, 0.48, 1.74, 1.04,
#> 0.947, 1.88, and 1.84 ]
#> Writing output for generation 1 to results.db with sim ID:
#> "45819_gotmwet_C_002" [2026-06-12 04:29:02]
#> ℹ Survival rate: 0.6
#> → Starting generation 2/2, 10 members. [2026-06-12 04:29:02]
#> Parameter summary for generation 2:
#>        turbulence/turb_param/k_min light_extinction/A/constant_value
#> mean                     4.646e-06                           0.48530
#> median                   4.370e-06                           0.47040
#> sd                       1.142e-06                           0.07295
#>        light_extinction/g1/constant_value light_extinction/g2/constant_value
#> mean                              0.50930                             1.5440
#> median                            0.49050                             1.5980
#> sd                                0.06984                             0.8177
#>        MET_wndspd MET_radswd outflow inflow
#> mean      1.05100     0.9768  1.7820 1.7300
#> median    1.03800     0.9811  1.8290 1.8270
#> sd        0.06014     0.1248  0.3004 0.3341
#> Writing output for generation 2 to results.db with sim ID:
#> "45819_gotmwet_C_002" [2026-06-12 04:29:12]
#> ✔ Completed generation 2/2 
#> for "gotm_wet". [2026-06-12 04:29:12]
#> Best fit: 22.9 (sd: 2731)
#> ℹ Survival rate: 0.9
                     
# Read calibration output                      
calib <- read_calib(sim_id = sim_id, ctrl = ctrl)
plist <- plot_calib(calib = calib)

# Dotty plot
plist$dotty
#> Warning: Removed 60 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 40 rows containing missing values or values outside the scale range
#> (`geom_point()`).


# Convergence plot
plist$convergence


# Histogram plot
plist$histogram
```
