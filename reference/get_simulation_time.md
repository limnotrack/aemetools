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
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED2 for lake wainamu
#> ℹ Copied in GLM nml file
#> ℹ Copied in AED nml file and supporting files
#> ℹ Building GOTM-WET model for lake wainamu
#> ℹ Copied in GOTM configuration files
#> ✔ GOTM YAML validation completed - no issues detected.
#> ✔ GLM nml validation completed - no issues detected.
aeme <- AEME::run_aeme(aeme = aeme, model = model, path = path)
#> ℹ Running models... (Have you tried parallelizing?) [2026-02-13 02:04:22]
#> → GLM-AED running... [2026-02-13 02:04:22]
#> ✔ GLM-AED run successful! [2026-02-13 02:04:23]
#> → GOTM-WET running... [2026-02-13 02:04:23]
#> ✔ GOTM-WET run successful! [2026-02-13 02:04:23]
#> ✔ Model run complete! [2026-02-13 02:04:23]
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
