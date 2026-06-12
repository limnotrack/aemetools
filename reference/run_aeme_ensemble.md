# Run AEME ensemble

Run a parameter ensemble

## Usage

``` r
run_aeme_ensemble(
  aeme,
  model,
  n = 10,
  dist = c("norm", "unif"),
  path = ".",
  parallel = FALSE,
  ncore = NULL,
  param = NULL,
  na_value = 999
)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`. Defaults to all models if not found in
  `aeme`.

- n:

  numeric; number of ensemble members to generate.

- dist:

  character; distribution to sample from. Default is "norm". Other
  options are "unif".

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

- parallel:

  logical; whether to run in parallel. Default is FALSE.

- ncore:

  numeric; number of cores to use for parallel processing. Default is
  NULL. If NULL, the function will use the number of cores minus 1.

- param:

  data.frame; parameter values to use. Default is NULL. If NULL, the
  function will use the parameters from the aeme object.

- na_value:

  numeric; value to use for NA values. Default is 999.

## Value

an `aeme` object with model output loaded.
