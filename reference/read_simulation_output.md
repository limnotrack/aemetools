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
#> ℹ Using observed water level
#> ℹ No missing values in observed water level. Using observed water level
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED for lake wainamu
#> ℹ Building GOTM-WET model for lake wainamu
#> ✔ GOTM YAML validation completed - no issues detected.
#> ✔ GLM nml validation completed - no issues detected.
#> ℹ Running models... (Have you tried parallelizing?) [2026-03-02 21:51:30]
#> → GLM-AED running... [2026-03-02 21:51:30]
#> ✔ GLM-AED run successful! [2026-03-02 21:51:30]
#> → GOTM-WET running... [2026-03-02 21:51:31]
#> ✔ GOTM-WET run successful! [2026-03-02 21:51:31]
#> ✔ Model run complete! [2026-03-02 21:51:31]
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
#> ℹ Extracting indices for "glm_aed" modelled variables [2026-03-02 21:51:33]
#> ✔ Indices extracted for "glm_aed" modelled variables [2026-03-02 21:51:34]
#> ℹ Using 2 cores for parallel calibration for "glm_aed".
#> → Starting generation 1/2, 10 members. [2026-03-02 21:51:34]
#> Parameter summary for generation 1:
#>        light/Kw MET_wndspd MET_radswd mixing/coef_mix_conv
#> mean      2.811     0.9939     1.0100              0.14980
#> median    2.812     0.9948     1.0090              0.15080
#> sd        1.708     0.1892     0.1857              0.02925
#>        mixing/coef_wind_stir mixing/coef_mix_shear mixing/coef_mix_turb
#> mean                 0.24980               0.15150               0.4483
#> median               0.24810               0.15030               0.4597
#> sd                   0.03053               0.02964               0.1508
#>        mixing/coef_mix_hyp sediment/sed_temp_mean[1]
#> mean                0.6041                    11.880
#> median              0.6167                    11.560
#> sd                  0.1221                     3.561
#>        sediment/sed_temp_peak_doy[1] outflow inflow
#> mean                           44.83  1.4830  1.508
#> median                         44.32  1.4720  1.570
#> sd                             28.01  0.6213  0.594
#> ✔ Completed generation 1/2 
#> for "glm_aed". [2026-03-02 21:51:48]
#> Best fit: 1680 (sd: 2659.3) Parameters: [ 2.59, 1.15, 1.17, 0.166, 0.278,
#> 0.105, 0.611, 0.633, 7.87, 36.6, 1.74, and 1.25 ]
#> Writing output for generation 1 to results.db with sim ID: "45819_glmaed_C_003"
#> [2026-03-02 21:51:49]
#> ℹ Survival rate: 0.6
#> → Starting generation 2/2, 10 members. [2026-03-02 21:51:49]
#> Parameter summary for generation 2:
#>        light/Kw MET_wndspd MET_radswd mixing/coef_mix_conv
#> mean     1.8740     0.8177     0.9825              0.17250
#> median   1.8260     0.7774     0.9867              0.17330
#> sd       0.4185     0.1440     0.1043              0.02735
#>        mixing/coef_wind_stir mixing/coef_mix_shear mixing/coef_mix_turb
#> mean                 0.27430               0.12280               0.5729
#> median               0.27180               0.11840               0.5915
#> sd                   0.01883               0.02032               0.1455
#>        mixing/coef_mix_hyp sediment/sed_temp_mean[1]
#> mean               0.72530                    10.460
#> median             0.71330                    10.400
#> sd                 0.05915                     1.968
#>        sediment/sed_temp_peak_doy[1] outflow inflow
#> mean                           37.80   1.487 1.5540
#> median                         40.10   1.379 1.4460
#> sd                             13.01   0.394 0.3305
#> Writing output for generation 2 to results.db with sim ID: "45819_glmaed_C_003"
#> [2026-03-02 21:51:54]
#> ✔ Completed generation 2/2 
#> for "glm_aed". [2026-03-02 21:51:54]
#> Best fit: 7.61 (sd: 2295.4)
#> ℹ Survival rate: 0.8
#> ℹ Extracting indices for "gotm_wet" modelled variables [2026-03-02 21:51:55]
#> ✔ Indices extracted for "gotm_wet" modelled variables [2026-03-02 21:51:56]
#> ℹ Using 2 cores for parallel calibration for "gotm_wet".
#> → Starting generation 1/2, 10 members. [2026-03-02 21:51:57]
#> Parameter summary for generation 1:
#>        turbulence/turb_param/k_min light_extinction/A/constant_value
#> mean                     5.039e-06                           0.52650
#> median                   5.141e-06                           0.52760
#> sd                       2.981e-06                           0.07809
#>        light_extinction/g1/constant_value light_extinction/g2/constant_value
#> mean                              0.59090                             1.3910
#> median                            0.59260                             1.3850
#> sd                                0.09164                             0.7801
#>        MET_wndspd MET_radswd outflow inflow
#> mean       0.9996     1.0020  1.5020  1.507
#> median     1.0010     0.9856  1.5310  1.518
#> sd         0.1770     0.1813  0.6031  0.613
#> ✔ Completed generation 1/2 
#> for "gotm_wet". [2026-03-02 21:52:20]
#> Best fit: 5.15 (sd: 7108) Parameters: [ 1.2e-06, 0.43, 0.479, 0.296, 1.29,
#> 1.29, 0.534, and 0.545 ]
#> Writing output for generation 1 to results.db with sim ID:
#> "45819_gotmwet_C_003" [2026-03-02 21:52:20]
#> ℹ Survival rate: 0.9
#> → Starting generation 2/2, 10 members. [2026-03-02 21:52:21]
#> Parameter summary for generation 2:
#>        turbulence/turb_param/k_min light_extinction/A/constant_value
#> mean                     2.509e-06                           0.46200
#> median                   1.901e-06                           0.44730
#> sd                       2.476e-06                           0.06009
#>        light_extinction/g1/constant_value light_extinction/g2/constant_value
#> mean                               0.5745                             0.6831
#> median                             0.5314                             0.4753
#> sd                                 0.1049                             0.6117
#>        MET_wndspd MET_radswd outflow inflow
#> mean       1.1640     1.0890  0.9794 0.8695
#> median     1.2040     1.1310  0.7133 0.7077
#> sd         0.1789     0.2192  0.5855 0.5154
#> Writing output for generation 2 to results.db with sim ID:
#> "45819_gotmwet_C_003" [2026-03-02 21:52:32]
#> ✔ Completed generation 2/2 
#> for "gotm_wet". [2026-03-02 21:52:32]
#> Best fit: 4.52 (sd: 1043.8)
#> ℹ Survival rate: 0.9
                     
# Read calibration output                      
calib <- read_calib(sim_id = sim_id, ctrl = ctrl)
```
