# Run AEME in a Shiny app

Run AEME in a Shiny app

## Usage

``` r
run_aeme_shiny(aeme, param, path = ".", model_controls = NULL)
```

## Arguments

- aeme:

  aeme; object.

- param:

  dataframe; of parameters read in from a csv file. Requires the columns
  c("model", "file", "name", "value", "min", "max", "log")

- path:

  filepath; where input files are located relative to the current
  working directory.

- model_controls:

  dataframe; of configuration loaded from "model_controls.csv".

## Value

Launches shiny app
