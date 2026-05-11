# Update parameter values in param based on best_pars

Update parameter values in param based on best_pars

## Usage

``` r
update_param(
  calib,
  param,
  aeme,
  replace = FALSE,
  fit_col = "fit",
  best_pars,
  quantile = 0.1,
  na_value = NULL
)
```

## Arguments

- calib:

  A list with the calibration results loaded using
  [`read_calib`](read_simulation_output.md).

- param:

  A data frame with parameters to update. Defaults to NULL. When NULL,
  the parameter values are extracted from `calib$parameter_metadata`.

- aeme:

  aeme; object. Defaults to NULL. When NULL, a dataframe of the updated
  parameter values is returned. When provided, the updated parameter
  values are added to the aeme object and the aeme object is returned.

- replace:

  Logical. If TRUE, the parameter values in the aeme object are replaced
  with the updated values. Defaults to FALSE. Only used when aeme is
  provided.

- fit_col:

  character; name of column containing fit values. Default is `"fit"`.

- best_pars:

  A data frame with the best parameters from
  [`get_param`](get_param.md). Defaults to NULL. When NULL,
  [`get_param`](get_param.md) is called to get the best parameters.

- quantile:

  **\[deprecated\]** The quantile to use for the top quantile of the
  fit_value. Defaults to 0.1. This is no longer needed and will be
  removed in a future version.

- na_value:

  **\[deprecated\]** Numeric. Penalty value substituted for `NA` fit
  values, this is no longer needed as NA values are now written to
  simulation_data in output of calib_aeme() and sa_aeme(). The argument
  will be removed in a future version.

## Value

data frame with updated parameter values for running the model with
[`run_aeme_param`](run_aeme_param.md)
