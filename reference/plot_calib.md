# Plot calibration results

Plot calibration results

## Usage

``` r
plot_calib(
  calib,
  na_value,
  fit_col = "fit",
  nrow = 2,
  base_size = 8,
  return_pars = FALSE,
  log_y = TRUE
)
```

## Arguments

- calib:

  dataframe; output from
  [`read_calib`](https://limnotrack.github.io/aemetools/reference/read_simulation_output.md)

- na_value:

  numeric; value to replace NA values with

- fit_col:

  character; name of column containing fit values. Default is `"fit"`.

- nrow:

  integer; number of rows in plot

- base_size:

  numeric; base size for theme

- return_pars:

  logical; return parameter values

- log_y:

  logical; use log scale on y-axis. Default is `TRUE`.

## Value

list of plots
