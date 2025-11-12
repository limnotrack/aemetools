# Read the calibration metadata

This function reads the calibration metadata from the output file. The
output file can be either a DuckDB database or a CSV file. The function
reads the metadata and returns it as a data frame.

## Usage

``` r
read_calib_meta(file, file_dir = "calib_sa")
```

## Arguments

- file:

  The path to the output file. It can either be a DuckDB database or a
  CSV file.

- file_dir:

  The directory where the output file is located. Defaults to
  "calib_sa".

## Value

A data frame with the calibration metadata
