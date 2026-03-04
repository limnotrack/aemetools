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
#> ℹ Running models... (Have you tried parallelizing?) [2026-03-04 03:04:03]
#> → GLM-AED running... [2026-03-04 03:04:03]
#> ✔ GLM-AED run successful! [2026-03-04 03:04:03]
#> → GOTM-WET running... [2026-03-04 03:04:03]
#> ✔ GOTM-WET run successful! [2026-03-04 03:04:04]
#> ✔ Model run complete! [2026-03-04 03:04:04]
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
#> ℹ Extracting indices for "glm_aed" modelled variables [2026-03-04 03:04:05]
#> ✔ Indices extracted for "glm_aed" modelled variables [2026-03-04 03:04:06]
#> ℹ Using 2 cores for parallel calibration for "glm_aed".
#> → Starting generation 1/2, 10 members. [2026-03-04 03:04:07]
#> Parameter summary for generation 1:
#>        light/Kw MET_wndspd MET_radswd mixing/coef_mix_conv
#> mean      2.861     0.9918     1.0000              0.15010
#> median    2.764     0.9815     0.9993              0.15030
#> sd        1.612     0.1869     0.1926              0.02925
#>        mixing/coef_wind_stir mixing/coef_mix_shear mixing/coef_mix_turb
#> mean                 0.24920               0.14830               0.4551
#> median               0.24780               0.15090               0.4594
#> sd                   0.02897               0.03033               0.1553
#>        mixing/coef_mix_hyp outflow inflow
#> mean                0.5934  1.4660 1.5450
#> median              0.5891  1.4040 1.5750
#> sd                  0.1240  0.6184 0.6141
#> ✔ Completed generation 1/2 
#> for "glm_aed". [2026-03-04 03:04:21]
#> Best fit: 263 (sd: 4032.3) Parameters: [ 3.05, 1.07, 1.27, 0.106, 0.253, 0.196,
#> 0.217, 0.646, 1.3, and 1.25 ]
#> Writing output for generation 1 to results.db with sim ID: "45819_glmaed_C_003"
#> [2026-03-04 03:04:21]
#> ℹ Survival rate: 0.7
#> → Starting generation 2/2, 10 members. [2026-03-04 03:04:22]
#> Parameter summary for generation 2:
#>        light/Kw MET_wndspd MET_radswd mixing/coef_mix_conv
#> mean      1.787     0.9291     1.1160              0.15580
#> median    1.554     0.9214     1.1440              0.15350
#> sd        1.422     0.1569     0.1525              0.03803
#>        mixing/coef_wind_stir mixing/coef_mix_shear mixing/coef_mix_turb
#> mean                0.260400              0.186300               0.4510
#> median              0.258500              0.186800               0.4445
#> sd                  0.009843              0.008991               0.1460
#>        mixing/coef_mix_hyp outflow inflow
#> mean               0.65600  1.4890 1.6860
#> median             0.68410  1.5320 1.7260
#> sd                 0.09622  0.3176 0.5123
#> Writing output for generation 2 to results.db with sim ID: "45819_glmaed_C_003"
#> [2026-03-04 03:04:30]
#> ✔ Completed generation 2/2 
#> for "glm_aed". [2026-03-04 03:04:30]
#> Best fit: 18.6 (sd: 2756.6)
#> ℹ Survival rate: 1
#> ℹ Extracting indices for "gotm_wet" modelled variables [2026-03-04 03:04:30]
#> ✔ Indices extracted for "gotm_wet" modelled variables [2026-03-04 03:04:31]
#> ℹ Using 2 cores for parallel calibration for "gotm_wet".
#> → Starting generation 1/2, 10 members. [2026-03-04 03:04:32]
#> Parameter summary for generation 1:
#>        turbulence/turb_param/k_min light_extinction/A/constant_value
#> mean                     4.940e-06                           0.52720
#> median                   4.905e-06                           0.52790
#> sd                       3.167e-06                           0.08187
#>        light_extinction/g1/constant_value light_extinction/g2/constant_value
#> mean                              0.59090                             1.3460
#> median                            0.60150                             1.4500
#> sd                                0.08696                             0.8097
#>        MET_wndspd MET_radswd outflow inflow
#> mean       1.0030     0.9975  1.4950 1.5140
#> median     0.9910     0.9961  1.4720 1.4320
#> sd         0.1803     0.1845  0.6073 0.6017
#> ✔ Completed generation 1/2 
#> for "gotm_wet". [2026-03-04 03:04:50]
#> Best fit: 108 (sd: 9490.8) Parameters: [ 1.52e-06, 0.448, 0.483, 0.854, 0.829,
#> 0.97, 2.21, and 2.28 ]
#> Writing output for generation 1 to results.db with sim ID:
#> "45819_gotmwet_C_003" [2026-03-04 03:04:50]
#> ℹ Survival rate: 0.8
#> → Starting generation 2/2, 10 members. [2026-03-04 03:04:50]
#> Parameter summary for generation 2:
#>        turbulence/turb_param/k_min light_extinction/A/constant_value
#> mean                     1.881e-06                           0.48090
#> median                   1.743e-06                           0.47080
#> sd                       4.958e-07                           0.06674
#>        light_extinction/g1/constant_value light_extinction/g2/constant_value
#> mean                              0.50360                             1.1670
#> median                            0.49510                             1.0440
#> sd                                0.04614                             0.7084
#>        MET_wndspd MET_radswd outflow inflow
#> mean      0.83980     0.9083  1.8010 1.8990
#> median    0.84240     0.9112  1.8290 2.0430
#> sd        0.05328     0.1094  0.6452 0.6382
#> Writing output for generation 2 to results.db with sim ID:
#> "45819_gotmwet_C_003" [2026-03-04 03:05:00]
#> ✔ Completed generation 2/2 
#> for "gotm_wet". [2026-03-04 03:05:00]
#> Best fit: 61.6 (sd: 1935.1)
#> ℹ Survival rate: 1
                     
# Read calibration output                      
calib <- read_calib(sim_id = sim_id, ctrl = ctrl)
```
