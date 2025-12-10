# Plot the scatter of the output of a sensitivity analysis

Plot the scatter of the output of a sensitivity analysis

## Usage

``` r
plot_scatter(sa, cutoff = NA)
```

## Arguments

- sa:

  list; of sensitivity analysis results read in with
  [`read_sa`](read_sa.md)

- cutoff:

  numeric. The maximum value of the fit to include in the plot. This can
  be useful to remove outliers.

## Value

`ggplot` object
