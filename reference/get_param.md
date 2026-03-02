# Get parameter values from calibration results

Get parameter values from calibration results

## Usage

``` r
get_param(calib, na_value, fit_col = "fit", best = FALSE, quantile = 0.1)
```

## Arguments

- calib:

  A list with the calibration results loaded using
  [`read_calib`](read_simulation_output.md).

- na_value:

  A numeric value which corresponds to the NA value used in the
  calibration.

- fit_col:

  character; name of column containing fit values. Default is `"fit"`.

- best:

  A logical value indicating whether to return the best parameter values
  or all parameter values.

- quantile:

  The quantile to use for the top quantile of the fit_value. Defaults to
  0.1.

## Value

A data frame with the parameter values.
