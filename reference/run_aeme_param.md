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

  Aeme object.

- param:

  dataframe; of parameters read in from a csv file. Requires the columns
  c("model", "file", "name", "value", "min", "max", "log")

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`. Defaults to all models if not found in
  `aeme`.

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

- model_controls:

  data.frame; model configuration, typically loaded via
  [`get_model_controls()`](https://limnotrack.com/reference/get_model_controls.html).

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
