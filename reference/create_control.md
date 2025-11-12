# Create control list for calibration or sensitivity analysis

Create control list for calibration or sensitivity analysis

## Usage

``` r
create_control(method, ...)
```

## Arguments

- method:

  The method to be used. It can be either "calib" for calibration or
  "sa" for sensitivity analysis.

- ...:

  Additional arguments to be passed to the function `create_control`.
  The arguments are different for calibration and sensitivity analysis.
  There are arguments which are common to both methods:

  - `file_type` string; file type to write the output to. Options are
    c("csv", "db"). Defaults to "db".

  - `file_name` string; file name to write the output to. Defaults to
    "results.db" if `file_type` is "db" and "simulation_metadata.csv" if
    `file_type` is "csv".

  - `file_dir` string; directory to write the output to. Defaults to the
    directory "calib_sa" in the current working directory. If the
    directory does not exist, it will be created.

  - `na_value` value to replace NA values with in observations. Defaults
    to 999.

  - `parallel` boolean; run calibration in parallel. Default to TRUE

  - `ncore`: The number of cores to use for the calibration. This is
    only used if `parallel = TRUE`. Default to
    `parallel::detectCores() - 1`.

  - `timeout`: The maximum time in seconds to run the calibration.
    Default to Inf. If the calibration takes longer than the timeout,
    the calibration will stop and return the best parameter set found so
    far.

  For calibration, the arguments are:

  - `VTR` Value to be reached. The optimization process will stop if
    either the maximum number of iterations itermax is reached or the
    best parameter vector bestmem has found a value fn(bestmem) \<= VTR.
    Default to -Inf.

  - `NP` number of population members. Defaults to NA; if the user does
    not change the value of NP from NA it is reset as
    `10 * sum(param$model == model)`. For many problems it is best to
    set NP to be at least 10 times the length of the parameter vector.

  - `itermax` the maximum iteration (population generation) allowed.
    Default is 200.

  - `reltol` relative convergence tolerance. The algorithm stops if it
    is unable to reduce the value by a factor of
    `reltol * (abs(val) + reltol)`. Default = 0.07

  - `cutoff`: The quantile cutoff used to select the parents for the
    next generation. For example, if `cutoff = 0.25`, the best 25% of
    the population will be used as parents for the next generation.

  - `mutate` fraction of population to undergo mutation (0-1).

  - `c_method` character; the method to use for calibration. Options are
    "CMAES" and "LHC". Defaults to "CMAES".

  For sensitivity analysis, the arguments are:

  - `N`: The initial sample size of the base sample matrix.

  - `vars_sim`: A named list of output variables for sensitivity
    analysis. The name is user defined but each list must contain:

    - `var`: The variable name to use for the sensitivity analysis.

    - `month`: A vector of months to use for the sensitivity analysis.

    - `depth_range`: A vector of length 2 with the minimum and maximum
      depth range to use for the sensitivity analysis.

## Value

list with the control parameters

## Examples

``` r
calib_ctrl <- create_control("calib", VTR = -Inf, NP = 10, itermax = 30,
                              reltol = 0.07, cutoff = 0.25, mutate = 0.1,
                              parallel = TRUE, file_type = "db",
                              file_name = "results.db", na_value = 999,
                              ncore = 3)

sa_ctrl <- create_control(method = "sa", N = 2^2, ncore = 2L, na_value = 999,
                          parallel = TRUE, file_type = "db",
                          file_name = "results.db",
                          vars_sim = list(surf_temp = list(var = "HYD_temp",
                                                           month = c(10:12, 1:3),
                                                           depth_range = c(0, 2)
                                                           ),
                                          bot_temp = list(var = "HYD_temp",
                                                          month = c(10:12, 1:3),
                                                          depth_range = c(10, 13)
                                                          )
                                          )
                        )

```
