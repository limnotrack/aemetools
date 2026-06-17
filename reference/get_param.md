# Get parameter values from calibration results

**\[deprecated\]**

`get_param()` is deprecated. Please use either get_sim_params() or
get_best_params() instead, depending on whether you want to retrieve all
parameter values or just the best parameter values based on a specified
fit column and quantile threshold.

## Usage

``` r
get_param(calib, na_value, fit_col = "fit", best = FALSE, quantile = 0.1)
```

## Arguments

- calib:

  A list with the calibration results loaded using
  [`read_calib`](read_simulation_output.md).

- na_value:

  **\[deprecated\]** Numeric. Penalty value substituted for `NA` fit
  values, this is no longer needed as NA values are now written to
  simulation_data in output of calib_aeme() and sa_aeme(). The argument
  will be removed in a future version.

- fit_col:

  character; name of column containing fit values. Default is `"fit"`.

- best:

  A logical value indicating whether to return the best parameter values
  or all parameter values.

- quantile:

  **\[deprecated\]** No longer used, replaced by `quantile_threshold`.

## Value

A data frame with the parameter values.
