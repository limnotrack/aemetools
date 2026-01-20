# Run AEME with parameter dataframe

Run AEME with parameter dataframe

## Usage

``` r
run_aeme_param(
  aeme,
  param,
  model,
  path = ".",
  model_controls = NULL,
  na_value = 999,
  return_nc = FALSE,
  return_aeme = FALSE,
  parallel = FALSE,
  verbose = FALSE,
  timeout = Inf
)
```

## Arguments

- aeme:

  aeme; object.

- param:

  dataframe; of parameters read in from a csv file. Requires the columns
  c("model", "file", "name", "value", "min", "max", "log")

- model:

  vector; of models to be used. Can be \`dy_cd\`, \`glm_aed\`,
  \`gotm_wet\`.

- path:

  filepath; where input files are located relative to the current
  working directory.

- model_controls:

  dataframe; of configuration loaded from "model_controls.csv".

- na_value:

  numeric; value to return if model run is unsuccessful

- return_nc:

  boolean; return netCDF file connection

- return_aeme:

  boolean; return AEME object

- parallel:

  logical; run models in parallel. Defaults to FALSE.

- verbose:

  logical; print model output to console. Defaults to FALSE.

- timeout:

  Timeout for the process, in seconds, or as a `difftime` object. If it
  is not finished before this, it will be killed.

## Value

`na_value` if model run is unsuccessful
