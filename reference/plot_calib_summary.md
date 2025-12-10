# Plot calibration results

Plot calibration results

## Usage

``` r
plot_calib_summary(calib, fit_col, nrow = 2, base_size = 8, log_y = TRUE)
```

## Arguments

- calib:

  dataframe; output from [`read_calib`](read_simulation_output.md)

- fit_col:

  character; name of column containing fit values. If missing, all fit
  types in `calib$simulation_data$fit_type` are used.

- nrow:

  integer; number of rows in plot

- base_size:

  numeric; base size for theme

- log_y:

  logical; use log scale on y-axis. Default is `TRUE`.

## Value

list of plots
