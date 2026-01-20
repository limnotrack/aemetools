# Get simulation time for each model

Get simulation time for each model

## Usage

``` r
get_simulation_time(aeme, model, path, param, FUN_list, vars_sim, weights)
```

## Arguments

- aeme:

  aeme; object.

- model:

  vector; of models to be used. Can be \`dy_cd\`, \`glm_aed\`,
  \`gotm_wet\`.

- path:

  filepath; where input files are located relative to the current
  working directory.

- param:

  dataframe; of parameters read in from a csv file. Requires the columns
  c("model", "file", "name", "value", "min", "max", "log")

- FUN_list:

  list of functions; named according to the variables in the `vars_sim`.
  Funtions are of the form `function(df)` which will be used to
  calculate model fit. If nor provided, uses mean absolute error (MAE).

- vars_sim:

  vector; of variables names to be used in the calculation of model fit.

- weights:

  vector; of weights for each variable in vars_sim. If not provided,
  defaults to 1 for each variable.

## Value

numeric vector of simulation time for each model.

## Examples

``` r
tmpdir <- tempdir()
aeme_dir <- system.file("extdata/lake/", package = "AEME")
# Copy files from package into tempdir
file.copy(aeme_dir, tmpdir, recursive = TRUE)
#> [1] TRUE
path <- file.path(tmpdir, "lake")
aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
model_controls <- AEME::get_model_controls()
model <- c("glm_aed", "gotm_wet")
aeme <- AEME::build_aeme(path = path, aeme = aeme,
model = model, model_controls = model_controls,
ext_elev = 5, use_bgc = FALSE)
#> ℹ Using observed water level
#> ℹ No missing values in observed water level. Using observed water level
#> Parameters: C = 0.5 , h_inv = 22.9904 

#> Parameters: C = 0.5 , h_inv = 22.9904 
#> Parameters: C = 0.501 , h_inv = 22.9904 
#> Parameters: C = 0.499 , h_inv = 22.9904 
#> Parameters: C = 0.5 , h_inv = 22.9914 
#> Parameters: C = 0.5 , h_inv = 22.9894 
#> Parameters: C = 0.2661 , h_inv = 23.6504 
#> Parameters: C = 0.2671 , h_inv = 23.6504 
#> Parameters: C = 0.2651 , h_inv = 23.6504 
#> Parameters: C = 0.2661 , h_inv = 23.6504 
#> Parameters: C = 0.2661 , h_inv = 23.6494 
#> Parameters: C = 0.3939 , h_inv = 23.3644 
#> Parameters: C = 0.3949 , h_inv = 23.3644 
#> Parameters: C = 0.3929 , h_inv = 23.3644 
#> Parameters: C = 0.3939 , h_inv = 23.3654 
#> Parameters: C = 0.3939 , h_inv = 23.3634 
#> Parameters: C = 0.3311 , h_inv = 23.5049 
#> Parameters: C = 0.3321 , h_inv = 23.5049 
#> Parameters: C = 0.3301 , h_inv = 23.5049 
#> Parameters: C = 0.3311 , h_inv = 23.5059 
#> Parameters: C = 0.3311 , h_inv = 23.5039 
#> Parameters: C = 0.3395 , h_inv = 23.4794 
#> Parameters: C = 0.3405 , h_inv = 23.4794 
#> Parameters: C = 0.3385 , h_inv = 23.4794 
#> Parameters: C = 0.3395 , h_inv = 23.4804 
#> Parameters: C = 0.3395 , h_inv = 23.4784 
#> Parameters: C = 0.3355 , h_inv = 23.4916 
#> Parameters: C = 0.3365 , h_inv = 23.4916 
#> Parameters: C = 0.3345 , h_inv = 23.4916 
#> Parameters: C = 0.3355 , h_inv = 23.4926 
#> Parameters: C = 0.3355 , h_inv = 23.4906 
#> Optimization Complete:
#>   Best C: 0.3355
#>   Best h_inv: 23.4916
#>   Final RMSE: 0.1397

#> Parameters: C = 0.5 , h_inv = 22.9904 

#> Parameters: C = 0.5 , h_inv = 22.9904 
#> Parameters: C = 0.501 , h_inv = 22.9904 
#> Parameters: C = 0.499 , h_inv = 22.9904 
#> Parameters: C = 0.5 , h_inv = 22.9914 
#> Parameters: C = 0.5 , h_inv = 22.9894 
#> Parameters: C = 0.2632 , h_inv = 23.6504 
#> Parameters: C = 0.2642 , h_inv = 23.6504 
#> Parameters: C = 0.2622 , h_inv = 23.6504 
#> Parameters: C = 0.2632 , h_inv = 23.6504 
#> Parameters: C = 0.2632 , h_inv = 23.6494 
#> Parameters: C = 0.391 , h_inv = 23.3679 
#> Parameters: C = 0.392 , h_inv = 23.3679 
#> Parameters: C = 0.39 , h_inv = 23.3679 
#> Parameters: C = 0.391 , h_inv = 23.3689 
#> Parameters: C = 0.391 , h_inv = 23.3669 
#> Parameters: C = 0.3271 , h_inv = 23.509 
#> Parameters: C = 0.3281 , h_inv = 23.509 
#> Parameters: C = 0.3261 , h_inv = 23.509 
#> Parameters: C = 0.3271 , h_inv = 23.51 
#> Parameters: C = 0.3271 , h_inv = 23.508 
#> Parameters: C = 0.3335 , h_inv = 23.4857 
#> Parameters: C = 0.3345 , h_inv = 23.4857 
#> Parameters: C = 0.3325 , h_inv = 23.4857 
#> Parameters: C = 0.3335 , h_inv = 23.4867 
#> Parameters: C = 0.3335 , h_inv = 23.4847 
#> Optimization Complete:
#>   Best C: 0.3335
#>   Best h_inv: 23.4857
#>   Final RMSE: 0.1429

#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED2 for lake wainamu
#> ℹ Copied in GLM nml file
#> ℹ Building GOTM-WET model for lake wainamu
#> ℹ Copied in GOTM configuration files
#> ✔ GOTM YAML validation completed - no issues detected.
#> ✔ GLM nml validation completed - no issues detected.
aeme <- AEME::run_aeme(aeme = aeme, model = model, path = path)
#> ℹ Running models... (Have you tried parallelizing?) [2026-01-20 21:51:29]
#> → GLM-AED running... [2026-01-20 21:51:29]
#> ✔ GLM-AED run successful! [2026-01-20 21:51:30]
#> → GOTM-WET running... [2026-01-20 21:51:30]
#> ✔ GOTM-WET run successful! [2026-01-20 21:51:30]
#> ✔ Model run complete! [2026-01-20 21:51:30]
#> ! The following variables are not available in model gotm_wet: RAD_extc
#> ! The following variables are not available in model gotm_wet: RAD_extc
data("aeme_parameters", package = "AEME")
param <- aeme_parameters
# Function to calculate fitness
fit <- function(df) {
mean(abs(df$obs - df$model))
}
FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)
ctrl <- create_control(method = "calib", NP = 10, itermax = 30, ncore = 2,
parallel = TRUE, file_type = "db", file_name = "results.db")
vars_sim <- c("HYD_temp", "LKE_lvlwtr")
weights <- c("HYD_temp" = 10, "LKE_lvlwtr" = 1)
sim_times <- get_simulation_time(aeme = aeme, model = model, path = path,
param = param, FUN_list = FUN_list, vars_sim = vars_sim, weights = weights)
```
