# Run AEME ensemble

Run a parameter ensemble

## Usage

``` r
run_aeme_ensemble(
  aeme,
  model,
  n = 10,
  dist = "norm",
  path = ".",
  parallel = FALSE,
  ncore = NULL,
  param = NULL,
  na_value = 999
)
```

## Arguments

- aeme:

  aeme; object.

- model:

  vector; of models to be used. Can be \`dy_cd\`, \`glm_aed\`,
  \`gotm_wet\`.

- n:

  numeric; number of ensemble members to generate.

- dist:

  character; distribution to sample from. Default is "norm". Other
  options are "unif".

- path:

  filepath; where input files are located relative to the current
  working directory.

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

an \`aeme\` object with model output loaded.
