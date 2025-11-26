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

  vector; of weights for each variable in vars_sim. If not provided,
  defaults to 1 for each variable.

- path:

  filepath; where input files are located relative to the current
  working directory.

- model_controls:

  dataframe; of configuration loaded from "model_controls.csv".

- ctrl:

  list; of controls for sensitivity analysis function created using the
  [`create_control`](https://limnotrack.github.io/aemetools/reference/create_control.md)
  function. See
  [create_control](https://limnotrack.github.io/aemetools/reference/create_control.md)
  for more details.

- param_df:

  dataframe; of parameters to be used in the calibration. Requires the
  columns c("model", "file", "name", "value", "min", "max"). This is
  used to restart from a previous calibration.

## Value

string of simulation id to be used to read the simulation output.
