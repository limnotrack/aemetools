# Plot the uncertainty of the output of a sensitivity analysis

Plot the uncertainty of the output of a sensitivity analysis

## Usage

``` r
plot_uncertainty(sa, na.rm = TRUE, bins = 30)
```

## Arguments

- sa:

  list; of sensitivity analysis results read in with
  [`read_sa`](https://limnotrack.github.io/aemetools/reference/read_sa.md)

- na.rm:

  a logical evaluating to `TRUE` or `FALSE` indicating whether `NA`
  values should be stripped before the computation proceeds.

- bins:

  integer; number of bins for the histogram. Default is 30.

## Value

`ggplot` object
