# Get simulation time for each model

Get simulation time for each model

## Usage

``` r
get_simulation_time(aeme, model, path, param, FUN_list, vars_sim, weights)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`. Defaults to all models if not found in
  `aeme`.

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

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

  a named vector; of weights for each variable in vars_sim. If not
  provided, defaults to 1 for each variable.

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
#> ℹ Copied in GLM nml file
#> ℹ Copied in AED nml file and supporting files
#> ℹ Copied in GLM plots nml file
#> ℹ Building GOTM-WET model for lake wainamu
#> ℹ Copied in GOTM configuration files
#> ✔ GOTM YAML validation completed - no issues detected.
#> ✔ GLM nml validation completed - no issues detected.
aeme <- AEME::run_aeme(aeme = aeme, model = model, path = path)
#> ℹ Running models... (Have you tried parallelizing?) [2026-06-16 05:25:28]
#> → GLM-AED running... [2026-06-16 05:25:28]
#> ✔ GLM-AED run successful! [2026-06-16 05:25:28]
#> → GOTM-WET running... [2026-06-16 05:25:28]
#> ✔ GOTM-WET run successful! [2026-06-16 05:25:29]
#> ✔ Model run complete! [2026-06-16 05:25:29]
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
