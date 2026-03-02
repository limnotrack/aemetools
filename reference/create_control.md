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

  Additional arguments passed to `create_control()`.

  **Common arguments (both methods):**

  - `file_type` Character. Output type: `"csv"` or `"db"`. Default
    `"db"`.

  - `file_name` Character. Output file name. Defaults to `"results.db"`
    (db) or `"simulation_metadata.csv"` (csv).

  - `file_dir` Character. Output directory. Defaults to `"calib_sa"` in
    the working directory (created if needed).

  - `na_value` Numeric. Replacement for `NA` in observations. Default
    `999`.

  - `parallel` Logical. Run in parallel? Default `TRUE`.

  - `ncore` Integer. Number of cores (used if `parallel = TRUE`).
    Default `parallel::detectCores() - 1`.

  - `timeout` Numeric. Maximum runtime in seconds. Default `Inf`.

  **Calibration-only arguments (`method = "calib"`):**

  - `VTR` Numeric. Target objective value. Default `-Inf`.

  - `NP` Integer. Population size. Default `NA` (internally reset).

  - `itermax` Integer. Maximum iterations. Default `200`.

  - `reltol` Numeric. Relative convergence tolerance. Default `0.07`.

  - `cutoff` Numeric. Quantile cutoff for parent selection (0–1).

  - `mutate` Numeric. Fraction of population to mutate (0–1).

  - `c_method` Character. Calibration method: `"CMAES"` or `"LHC"`.
    Default `"CMAES"`.

  **Sensitivity-analysis-only arguments (`method = "sa"`):**

  - `N` Integer. Base sample size.

  - `vars_sim` Named list describing output variables. Each element must
    contain:

    - `var` Character. Variable name.

    - `month` Integer vector. Months to include.

    - `depth_range` Numeric vector (length 2). Min/max depth.

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
