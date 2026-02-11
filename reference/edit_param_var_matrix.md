# Edit parameter–response matrix in a Shiny gadget

Edit parameter–response matrix in a Shiny gadget

## Usage

``` r
edit_param_var_matrix(param_var_matrix)
```

## Arguments

- param_var_matrix:

  a data frame with columns 'model', 'file', 'name_full', and variable
  names as columns. The 'name_full' column should be encoded as
  'group/name\[index\]'. The variable columns should contain TRUE/FALSE
  values indicating whether the parameter is associated with the
  variable or not. This is the output of `get_param_var_matrix()`.

## Value

a named list of edited matrices, with the same structure as the input.
If the user cancels the editing, the original input is returned.

## Examples

``` r
if (FALSE) { # \dontrun{
param <- aeme_parameters
vars_sim <- c("HYD_temp", "CHM_oxy", "PHY_tchla")
param_var_matrix <- create_param_var_matrix(param, vars_sim)
edited_matrix <- edit_param_var_matrix(param_var_matrix)
} # }
```
