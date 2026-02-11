# Create a parameter-variable matrix for each model

Create a parameter-variable matrix for each model

## Usage

``` r
create_param_var_matrix(param, vars_sim)
```

## Arguments

- param:

  dataframe; of parameters read in from a csv file. Requires the columns
  c("model", "file", "name", "value", "min", "max", "log")

- vars_sim:

  vector; of variables names to be used in the calculation of model fit.

## Value

A data frame with columns for model, file, name_full, and one column for
each variable in vars_sim. The variable columns contain TRUE/FALSE
values indicating whether the parameter is associated with the variable.

## Examples

``` r
data(aeme_parameters, package = "AEME")
param <- aeme_parameters
vars_sim <- c("HYD_temp", "CHM_oxy", "PHY_tchla")
param_var_matrix <- create_param_var_matrix(param, vars_sim)
```
