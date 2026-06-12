# Run a model and calculate model fit.

Run a model and calculate model fit.

## Usage

``` r
run_and_fit(
  aeme,
  param,
  model,
  vars_sim,
  path,
  model_controls = NULL,
  FUN_list = NULL,
  weights,
  na_value = 999,
  var_indices = NULL,
  return_indices = FALSE,
  include_wlev = FALSE,
  return_df = FALSE,
  method = "calib",
  sa_ctrl = NULL,
  fit = TRUE,
  timeout = Inf
)
```

## Arguments

- aeme:

  Aeme object.

- param:

  dataframe; of parameters read in from a csv file. Requires the columns
  c("model", "file", "name", "value", "min", "max", "log")

- model:

  string; for which model. Options are c("dy_cd", "glm_aed" and
  "gotm_wet")

- vars_sim:

  vector; of variables names to be used in the calculation of model fit.
  Currently only supports using one variable.

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

- model_controls:

  data.frame; model configuration, typically loaded via
  [`get_model_controls()`](https://limnotrack.com/reference/get_model_controls.html).

- FUN_list:

  function; of the form `function(O, P)` which will be used in to
  calculate model fit. If NULL, uses mean absolute error (MAE).

- weights:

  vector; of weights to be used in the calculation of model fit.

- na_value:

  numeric; value to be returned if model fails to run.

- var_indices:

  list; generated from running `run_and_fit()` with
  `return indices = TRUE` on the first simulation.

- return_indices:

  boolean; return the indices (depths, time and dates) of each variable.
  Used when running calibration and the time period does not change
  between simulations.

- include_wlev:

  boolean; include water level in the calculation of model fit.

- return_df:

  boolean; return dataframe of modelled and observed.

- method:

  string; of the method of the model run. Options are c("sa", "calib").

- sa_ctrl:

  list; of control parameters for the sensitivity analysis. Only
  required if `method = "sa"`.

- fit:

  boolean; fit model or not. If FALSE, only return netCDF file
  connection.

- timeout:

  numeric; time in seconds to run each simulation. Default is Inf.

## Value

A single value of model fit, calculated by `FUN_list`.
