# Update parameter values in param based on best_pars

Update parameter values in param based on best_pars

## Usage

``` r
update_param(
  calib,
  param,
  na_value,
  aeme,
  replace = FALSE,
  quantile = 0.1,
  fit_col = "fit",
  best_pars
)
```

## Arguments

- calib:

  dataframe; output from [`read_calib`](read_simulation_output.md)

- param:

  A data frame with parameters to update. Defaults to NULL. When NULL,
  the parameter values are extracted from `calib$parameter_metadata`.

- na_value:

  A numeric value which corresponds to the NA value used in the
  calibration.

- aeme:

  aeme; object. Defaults to NULL. When NULL, a dataframe of the updated
  parameter values is returned. When provided, the updated parameter
  values are added to the aeme object and the aeme object is returned.

- replace:

  Logical. If TRUE, the parameter values in the aeme object are replaced
  with the updated values. Defaults to FALSE. Only used when aeme is
  provided.

- quantile:

  The quantile to use for the top quantile of the fit_value. Defaults to
  0.1.

- fit_col:

  character; name of column containing fit values. Default is `"fit"`.

- best_pars:

  A data frame with the best parameters from
  [`get_param`](get_param.md). Defaults to NULL. When NULL,
  [`get_param`](get_param.md) is called to get the best parameters.

## Value

data frame with updated parameter values for running the model with
[`run_aeme_param`](run_aeme_param.md)
