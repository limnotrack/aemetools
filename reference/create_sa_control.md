# Create control list for sensitivity analysis

Create control list for sensitivity analysis

## Usage

``` r
create_sa_control(
  file_type = "db",
  file_name = NULL,
  file_dir = "calib_sa",
  na_value = 999,
  parallel = TRUE,
  ncore = parallel::detectCores() - 1,
  timeout = Inf,
  N,
  vars_sim,
  ...
)
```

## Arguments

- file_type:

  Character. Output type: `"csv"` or `"db"`. Default `"db"`.

- file_name:

  Character. Output file name. Defaults to `"results.db"` (db) or
  `"simulation_metadata.csv"` (csv).

- file_dir:

  Character. Output directory. Default `"calib_sa"`.

- na_value:

  Numeric. Penalty value substituted for `NA` fit values during
  optimisation to discourage parameter sets that produce invalid model
  output. Default `999`.

- parallel:

  Logical. Run in parallel? Default `TRUE`.

- ncore:

  Integer. Number of cores if `parallel = TRUE`. Default
  `parallel::detectCores() - 1`.

- timeout:

  Numeric. Max runtime in seconds. Default `Inf`.

- N:

  Integer. Base sample size.

- vars_sim:

  Named list describing output variables.

- ...:

  Must be empty. Additional arguments are not allowed.

## Value

A control list.
