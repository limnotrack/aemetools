# Plot AEME ensemble output

Plot AEME ensemble output

## Usage

``` r
plot_ensemble(
  aeme,
  model,
  var_sim = "HYD_temp",
  depth = NULL,
  conf_int = 0.95,
  type = "ribbon",
  remove_spin_up = TRUE,
  add_obs = TRUE,
  var_lims = NULL
)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; model(s) to plot. If missing, all models in the Aeme
  object will be plotted.

- var_sim:

  string; of variable to plot

- depth:

  numeric; depth of the variable to extract. Default is NULL. If NULL,
  the variable profiles are extracted.

- conf_int:

  numeric; confidence interval to plot when `type = "ribbon"`. Default
  is 0.95.

- type:

  character; type of plot to create. Can be `"ribbon"`, where it plots a
  geom_ribbon to represent the confidence intervals specified in
  `conf_int` or `"line`, where it plots all the ensemble members as
  lines. Default is "ribbon".

- remove_spin_up:

  logical; remove spin-up period from plot. Default is TRUE.

- add_obs:

  logical; add observations to plot

- var_lims:

  numeric vector of length 2; limits for the variable. Defaults to NULL
  and will generate common limits for all variables.

## Value

ggplot object
