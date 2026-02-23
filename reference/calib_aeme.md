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
  vars_sim = "HYD_temp",
  FUN_list,
  weights,
  path = ".",
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

- vars_sim:

  vector; of variables names to be used in the calculation of model fit.

- FUN_list:

  list of functions; named according to the variables in the `vars_sim`.
  Funtions are of the form `function(df)` which will be used to
  calculate model fit. If nor provided, uses mean absolute error (MAE).

- weights:

  a named vector; of weights for each variable in vars_sim. If not
  provided, defaults to 1 for each variable.

- path:

  filepath; where input files are located relative to the current
  working directory.

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
aeme <- AEME::build_aeme(aeme = aeme, model = model, path = path, 
                         model_controls = model_controls,
                         ext_elev = 5, use_bgc = FALSE)
#> Error: object 'path' not found
aeme <- AEME::run_aeme(aeme = aeme, model = model, path = path)
#> Error: object 'path' not found

data("aeme_parameters", package = "AEME")
param <- aeme_parameters

# Function to calculate fitness
fit <- function(df) {
  mean(abs(df$obs - df$model))
}
FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)

ctrl <- create_control(method = "calib", NP = 10, itermax = 20, ncore = 2,
                       parallel = TRUE, file_type = "db",
                       file_name = "results.db")

vars_sim <- c("HYD_temp", "LKE_lvlwtr")
weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 1)

# Calibrate AEME model
sim_id <- calib_aeme(aeme = aeme, model = model, path = path,
                     param = param, FUN_list = FUN_list, ctrl = ctrl,
                     vars_sim = vars_sim, weights = weights)
#> Error in AEME::set_vars_sim(model_controls = model_controls, vars_sim = vars_sim,     simulate = TRUE, exclusive = TRUE): "var_aeme" %in% names(model_controls) is not TRUE
```
