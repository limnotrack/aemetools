# Make inputs for the GR models

Make inputs for the GR models

## Usage

``` r
make_GR_inputs(
  id,
  reaches,
  lake,
  catchments,
  obs_flow = NULL,
  met,
  lat = NULL,
  FUN_MOD = airGR::RunModel_GR6J,
  plot = FALSE
)
```

## Arguments

- id:

  numeric; Reach ID

- reaches:

  sf; object with reaches as linestrings.

- lake:

  sf; polygon of lake shore.

- catchments:

  sf; polygon of catchmentss.

- obs_flow:

  dataframe; containing Date and flow in m3/s.

- met:

  dataframe; containing Date, air temperature and precipitation

- lat:

  numeric; latitude. If NULL, uses the latitude from the centre of the
  lake.

- FUN_MOD:

  function from the `airGR` package to be used. Defaults to
  [`airGR::RunModel_GR6J`](https://rdrr.io/pkg/airGR/man/RunModel_GR6J.html)

- plot:

  logical; plot the reaches, lake and catchment? Defaults to FALSE.

## Value

list of inputs for [`run_GR()`](run_GR.md).
