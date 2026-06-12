# Run AEME in a Shiny app

Run AEME in a Shiny app

## Usage

``` r
run_aeme_shiny(aeme, param, path = ".", model_controls = NULL)
```

## Arguments

- aeme:

  Aeme object.

- param:

  dataframe; of parameters read in from a csv file. Requires the columns
  c("model", "file", "name", "value", "min", "max", "log")

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

- model_controls:

  data.frame; model configuration, typically loaded via
  [`get_model_controls()`](https://limnotrack.com/reference/get_model_controls.html).

## Value

Launches shiny app
