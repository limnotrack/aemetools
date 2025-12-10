# Set weights for simulated variables

Set weights for simulated variables

## Usage

``` r
set_weights(vars_sim, weights = 1)
```

## Arguments

- vars_sim:

  vector; of variables names to be used in the calculation of model fit
  in [`calib_aeme()`](calib_aeme.md) or [`sa_aeme()`](sa_aeme.md).

- weights:

  numeric vector; of weights for each variable in `vars_sim`. If a
  single value is provided, it is recycled for all variables. Default is
  1.

## Value

named numeric vector; of weights for each variable in `vars_sim`.

## Examples

``` r
vars_sim <- c("HYD_temp", "HYD_thmcln", "HYD_strat")
# Set same weight for all variables
weights1 <- set_weights(vars_sim)
print(weights1)
#>   HYD_temp HYD_thmcln  HYD_strat 
#>          1          1          1 
# Set different weights for each variable
weights2 <- set_weights(vars_sim, weights = c(1, 2, 0.5))
print(weights2)
#>   HYD_temp HYD_thmcln  HYD_strat 
#>        1.0        2.0        0.5 
```
