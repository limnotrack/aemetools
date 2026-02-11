# Edit model parameters using a Shiny gadget

This function launches a Shiny gadget that allows users to edit model
parameters in an interactive table. The parameters are filtered by model
and file, and users can edit the values, minimums, maximums, and groups
of the parameters. The edited parameters are returned as a data frame
when the user clicks "Done". If the user clicks "Cancel", the original
parameters are returned.

## Usage

``` r
edit_parameters_shiny(param)
```

## Arguments

- param:

  dataframe; of parameters read in from a csv file. Requires the columns
  c("model", "file", "name", "value", "min", "max", "log")

## Value

A data frame with the edited parameters. The structure of the data frame
is the same as the input, with columns for model, file, name, value,
min, max, group and index. If the user cancels the editing, the original
input is returned.

## Examples

``` r
if (FALSE) { # \dontrun{
data("aeme_parameters", package = "AEME")
param <- aeme_parameters
edited_param <- edit_parameters_shiny(param)
} # }
```
