# Get best parameter values from calibration results

Get best parameter values from calibration results

## Usage

``` r
get_best_params(calib, fit_col = "fit", na_value = NULL)
```

## Arguments

- calib:

  A list with the calibration results loaded using
  [`read_calib`](read_simulation_output.md).

- fit_col:

  character; name of column containing fit values. Default is `"fit"`.

- na_value:

  **\[deprecated\]** Numeric. Penalty value substituted for `NA` fit
  values, this is no longer needed as NA values are now written to
  simulation_data in output of calib_aeme() and sa_aeme(). The argument
  will be removed in a future version.

## Value

A data frame with the parameter values.
