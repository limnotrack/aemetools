# Create control list for calibration

Create control list for calibration

## Usage

``` r
create_calib_control(
  file_type = "db",
  file_name = NULL,
  file_dir = "calib_sa",
  na_value = 999,
  parallel = TRUE,
  ncore = parallel::detectCores() - 1,
  timeout = Inf,
  VTR = -Inf,
  NP = NA,
  itermax = 200,
  reltol = 0.07,
  cutoff = 0.25,
  mutate = 0.1,
  c_method = "CMAES",
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

  Numeric. Replacement for `NA`. Default `999`.

- parallel:

  Logical. Run in parallel? Default `TRUE`.

- ncore:

  Integer. Number of cores if `parallel = TRUE`. Default
  `parallel::detectCores() - 1`.

- timeout:

  Numeric. Max runtime in seconds. Default `Inf`.

- VTR:

  Numeric. Target objective value. Default `-Inf`.

- NP:

  Integer. Population size. Default `NA`.

- itermax:

  Integer. Maximum iterations. Default `200`.

- reltol:

  Numeric. Relative convergence tolerance. Default `0.07`.

- cutoff:

  Numeric. Quantile cutoff (0–1).

- mutate:

  Numeric. Fraction of population to mutate (0–1).

- c_method:

  Character. `"CMAES"` or `"LHC"`. Default `"CMAES"`.

- ...:

  Must be empty. Additional arguments are not allowed.

## Value

A control list.
