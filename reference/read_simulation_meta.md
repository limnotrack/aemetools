# Read the simulation metadata

This function reads the simulation metadata from the output file. The
output file can be either a DuckDB database or a CSV file. The function
reads the metadata and returns it as a data frame.

## Usage

``` r
read_simulation_meta(ctrl = NULL, file_name, file_dir, type)
```

## Arguments

- ctrl:

  A list with the control parameters used in
  [`calib_aeme()`](calib_aeme.md) or [`sa_aeme()`](sa_aeme.md). If
  `ctrl` is provided, the `file_name` and `file_dir` arguments are
  ignored.

- file_name:

  The name of the output file. If `ctrl` is provided, this argument is
  ignored.

- file_dir:

  The directory of the output file. If `ctrl` is provided, this argument
  is ignored.

- type:

  Optional character vector to filter by simulation type. Possible
  values are "sa" for sensitivity analysis simulations and "calib" for
  calibration simulations. If not provided, all simulations are
  returned.

## Value

A data frame with the simulation metadata
