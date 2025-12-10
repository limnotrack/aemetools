# Read calibration output

Read calibration output

## Usage

``` r
read_simulation_output(
  ctrl = NULL,
  file_name,
  file_dir,
  file_type = "db",
  sim_id = NULL,
  type
)

read_calib(
  ctrl = NULL,
  file_name,
  file_dir,
  file_type = "db",
  sim_id = NULL,
  type
)
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

- type:

  A character string indicating the type of simulation. One of "calib",
  "sa", or "all". If missing, the type is inferred from the `ctrl`
  argument. If type is provided it overrides the `ctrl$method` value.

## Value

A list with the metadata and simulation data frames.
