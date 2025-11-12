# Visualization of first, total, second, third and fourth-order Sobol' indices

Visualization of first, total, second, third and fourth-order Sobol'
indices

## Usage

``` r
plot_sobol(sa, order = "first", add_errorbars = TRUE, use_dummy = TRUE)
```

## Arguments

- sa:

  list; of sensitivity analysis results read in with
  [`read_sa`](https://limnotrack.github.io/aemetools/reference/read_sa.md)

- order:

  A character vector specifying the order of the Sobol' indices to plot.
  The only current option is "first".

- add_errorbars:

  A logical value indicating whether to add error bars to the plot.
  Default is `TRUE`.

- use_dummy:

  A logical value indicating whether to use a dummy variable for the
  x-axis. Default is `TRUE`.

## Value

list of `ggplot` objects for each variable
