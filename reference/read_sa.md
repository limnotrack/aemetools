# Read sensitivity analysis output

Read sensitivity analysis output

## Usage

``` r
read_sa(ctrl = NULL, file_name, file_dir, sim_id, R = NULL, boot = TRUE)
```

## Arguments

- ctrl:

  list; of controls for sensitivity analysis function created using the
  [`create_control`](create_control.md) function. See
  [create_control](create_control.md) for more details.

- file_name:

  The name of the output file. If `ctrl` is provided, this argument is
  ignored.

- file_dir:

  The directory of the output file. If `ctrl` is provided, this argument
  is ignored.

- sim_id:

  A vector of simulation IDs to read. If NULL, all simulations are read.

- R:

  Positive integer, number of bootstrap replicas. Default is NULL.

- boot:

  Logical. If TRUE, the function bootstraps the Sobol' indices. If
  FALSE, it provides point estimates. Default is `boot = FALSE`.

## Value

A list with thedata frame with the sensitivity analysis results and the
sobol indices for each variable.
