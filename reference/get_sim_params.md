# Get parameter values from calibration results

Retrieves parameter values from calibration results, with optional
filtering based on fit values. This function is useful for extracting
all parameter sets used in the calibration process, and can be filtered
to include only those with fit values below a specified quantile
threshold.

## Usage

``` r
get_sim_params(calib, fit_col = "fit", na_value = NULL, quantile_threshold = 1)
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

- quantile_threshold:

  Numeric. A value between 0 and 1 specifying the quantile threshold for
  selecting the best parameter values based on their fit values. For
  example, a value of 0.1 will select parameter values with fit values
  in the lowest 10% of the distribution. Default is 1 (no filtering).

## Value

A data frame with all the parameter values.
