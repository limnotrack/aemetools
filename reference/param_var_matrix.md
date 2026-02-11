# AEME parameters-response matrix

A data frame that contains the parameters and their associated variables
for the AEME model. It contains a TRUE/FALSE value for each parameter
and variable combination, indicating whether the parameter is associated
with the variable or not. It can be used within calibration to determine
which parameters are associated with which variables.

## Usage

``` r
param_var_matrix
```

## Format

### `param_var_matrix`

A data frame with 32 rows and 8 columns:#'

- model:

  The model that the parameter belongs to.

- file:

  The file that the parameter belongs to.

- parameter_name:

  The name of the parameter encoded as "group/name\[index\]".

- HYD_temp:

  TRUE if the parameter is associated with the HYD_temp variable, FALSE
  otherwise.

- HYD_thmcln:

  TRUE if the parameter is associated with the HYD_thmcln variable,
  FALSE otherwise.

- LKE_lvlwtr:

  TRUE if the parameter is associated with the LKE_lvlwtr variable,
  FALSE otherwise.

- CHM_oxy:

  TRUE if the parameter is associated with the CHM_oxy variable, FALSE
  otherwise.

- PHY_tchla:

  TRUE if the parameter is associated with the PHY_tchla variable, FALSE
  otherwise.

## Source

Package development.
