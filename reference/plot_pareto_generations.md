# Plot Pareto front evolution across generations

This function visualizes the evolution of the Pareto front across
generations in a multi-objective optimization context. It creates
pairwise scatter plots of the objective values, highlighting the points
on the Pareto front for each generation.

## Usage

``` r
plot_pareto_generations(calib, generations = NULL)
```

## Arguments

- calib:

  dataframe; output from [`read_calib`](read_simulation_output.md)

- generations:

  Optional vector of generations to include in the plot. If NULL, all
  generations are included.

## Value

A ggplot object showing the evolution of the Pareto front across
generations. Each point represents a run in a generation, with points on
the Pareto front highlighted and colored by generation. The best
parameter set (with the lowest 'fit' value) is highlighted in red. The
axes are on a log scale to better visualize the distribution of
objective values.
