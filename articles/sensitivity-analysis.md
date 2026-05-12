# Sensitivity Analysis

## Setup

First, we will load the `AEME` and `aemetools` package:

``` r

library(AEME)
library(aemetools)
```

Create a folder for running the example calibration setup.

``` r


tmpdir <- "sa-test"
dir.create(tmpdir, showWarnings = FALSE)
aeme_dir <- system.file("extdata/lake/", package = "AEME")
# Copy files from package into tempdir
file.copy(aeme_dir, tmpdir, recursive = TRUE)
#> [1] TRUE
path <- file.path(tmpdir, "lake")

list.files(path, recursive = TRUE)
#> [1] "aeme.yaml"            "data/hypsograph.csv"  "data/inflow_FWMT.csv"
#> [4] "data/lake_obs.csv"    "data/meteo.csv"       "data/outflow.csv"    
#> [7] "data/water_level.csv" "model_controls.csv"
```

## Build AEME ensemble

Using the `AEME` functions, we will build the AEME model setup. For this
example, we will use the `glm_aed` model. The `build_aeme` function will

``` r


aeme <- yaml_to_aeme(path = path, "aeme.yaml")
model_controls <- AEME::get_model_controls()
inf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
outf_factor = c("dy_cd" = 1, "glm_aed" = 1, "gotm_wet" = 1)
model <- c("gotm_wet")
aeme <- build_aeme(path = path, aeme = aeme,
                   model = model, model_controls = model_controls,
                   inf_factor = inf_factor, ext_elev = 5,
                   use_bgc = TRUE)
```

## Description of Sensitivity Analysis method

The sensitivity analysis method used here is based on the Sobol method
and uses the `sensobol` package.

This package provides several functions to conduct variance-based
uncertainty and sensitivity analysis, from the estimation of sensitivity
indices to the visual representation of the results. It implements
several state-of-the-art first and total-order estimators and allows the
computation of up to fourth-order effects, as well as of the
approximation error, in a swift and user-friendly way.

For more information on the method, see the [sensobol package
vignette](https://cran.r-project.org/web/packages/sensobol/vignettes/sensobol.html).

## Load parameters to be used for the sensitivity analysis

Parameters are loaded from the `aemetools` package within the
`aeme_parameters` dataframe. The parameters are stored in a data frame
with the following columns:

- `model`: The model name

- `file`: The file name of the model parameter file

- `name`: The parameter name

- `value`: The parameter value

- `min`: The minimum value of the parameter

- `max`: The maximum value of the parameter

Parameters to be used for the calibration. (man)

``` r

utils::data("aeme_parameters", package = "AEME")
param <- aeme_parameters |>
  dplyr::filter(file != "wdr")
param
```

| model | file | name | value | min | max | group | index | module | var_sim |
|:---|:---|:---|---:|---:|---:|:---|:---|:---|:---|
| glm_aed | glm3.nml | light/Kw | 5.8e-01 | 0.100 | 5.52e+00 | NA | NA | hydrodynamic | HYD_temp\|HYD_thmcln |
| glm_aed | met | MET_wndspd | 1.0e+00 | 0.700 | 1.30e+00 | NA | NA | hydrodynamic | HYD_temp\|HYD_thmcln |
| glm_aed | met | MET_radswd | 1.0e+00 | 0.700 | 1.30e+00 | NA | NA | hydrodynamic | HYD_temp |
| glm_aed | glm3.nml | mixing/coef_mix_conv | 1.4e-01 | 0.100 | 2.00e-01 | NA | NA | hydrodynamic | HYD_thmcln |
| glm_aed | glm3.nml | mixing/coef_wind_stir | 2.1e-01 | 0.200 | 3.00e-01 | NA | NA | hydrodynamic | HYD_thmcln |
| glm_aed | glm3.nml | mixing/coef_mix_shear | 1.4e-01 | 0.100 | 2.00e-01 | NA | NA | hydrodynamic | HYD_thmcln |
| glm_aed | glm3.nml | mixing/coef_mix_turb | 5.6e-01 | 0.200 | 7.00e-01 | NA | NA | hydrodynamic | HYD_thmcln |
| glm_aed | glm3.nml | mixing/coef_mix_hyp | 7.4e-01 | 0.400 | 8.00e-01 | NA | NA | hydrodynamic | HYD_thmcln |
| glm_aed | inf | inflow | 1.0e+00 | 0.500 | 2.50e+00 | NA | NA | hydrodynamic | LKE_lvlwtr |
| gotm_wet | gotm.yaml | turbulence/turb_param/k_min | 6.0e-07 | 0.000 | 1.00e-05 | NA | NA | hydrodynamic | HYD_thmcln |
| gotm_wet | gotm.yaml | light_extinction/A/constant_value | 5.5e-01 | 0.395 | 6.59e-01 | NA | NA | hydrodynamic | HYD_temp\|HYD_thmcln |
| gotm_wet | gotm.yaml | light_extinction/g1/constant_value | 5.9e-01 | 0.440 | 7.40e-01 | NA | NA | hydrodynamic | HYD_temp\|HYD_thmcln |
| gotm_wet | gotm.yaml | light_extinction/g2/constant_value | 2.0e-01 | 0.050 | 2.70e+00 | NA | NA | hydrodynamic | HYD_temp\|HYD_thmcln |
| gotm_wet | met | MET_wndspd | 1.0e+00 | 0.700 | 1.30e+00 | NA | NA | hydrodynamic | HYD_temp\|HYD_thmcln |
| gotm_wet | met | MET_radswd | 1.0e+00 | 0.700 | 1.30e+00 | NA | NA | hydrodynamic | HYD_temp |
| gotm_wet | inf | inflow | 1.0e+00 | 0.500 | 2.50e+00 | NA | NA | hydrodynamic | LKE_lvlwtr |
| dy_cd | cfg | light_extinction_coefficient/7 | 9.0e-01 | 0.100 | 1.40e+00 | NA | NA | hydrodynamic | HYD_temp\|HYD_thmcln |
| dy_cd | dyresm3p1.par | vert_mix_coeff/15 | 2.0e+02 | 50.000 | 7.50e+02 | NA | NA | hydrodynamic | HYD_thmcln |
| dy_cd | met | MET_wndspd | 1.0e+00 | 0.700 | 1.30e+00 | NA | NA | hydrodynamic | HYD_temp\|HYD_thmcln |
| dy_cd | met | MET_radswd | 1.0e+00 | 0.700 | 1.30e+00 | NA | NA | hydrodynamic | HYD_temp |
| dy_cd | inf | inflow | 1.0e+00 | 0.500 | 2.50e+00 | NA | NA | hydrodynamic | LKE_lvlwtr |

## Sensitivity analysis setup

### Define fitness function

First, we will define a function for the sensitivity analysis function
to use to calculate the sensitivity of the model. This function takes a
dataframe as an argument. The dataframe contains the observed data
(`obs`) and the modelled data (`model`). The function should return a
single value.

Here we use the model mean.

``` r

# Function to calculate mean model output
fit <- function(df) {
  mean(df$model)
}
```

Different functions can be applied to different variables. For example,
we can use the mean for water temperature and median for chloophyll-a.

``` r

# Function to calculate median model output
fit2 <- function(df) {
  median(df$model)
}
```

Then these would be combined into a named list of functions which will
be passed to the `sa_aeme` function. They are named according to the
target variable.

``` r


# Create list of functions
FUN_list <- list(HYD_temp = fit, PHY_tchla = fit2)
```

### Define control parameters

Next, we will define the control parameters for the sensitivity
analysis. The control parameters are generated using `create_control`
and are then passed to the `sa_aeme` function. The control parameters
for the sensitivity analysis are as follows:

``` r

?create_sa_control
```

|                   |                 |
|-------------------|----------------:|
| create_sa_control | R Documentation |

## Create control list for sensitivity analysis

### Arguments

|  |  |
|----|----|
| `file_type` | Character. Output type: `"csv"` or `"db"`. Default `"db"`. |
| `file_name` | Character. Output file name. Defaults to `"results.db"` (db) or `"simulation_metadata.csv"` (csv). |
| `file_dir` | Character. Output directory. Default `"calib_sa"`. |
| `na_value` | Numeric. Penalty value substituted for `NA` fit values during optimisation to discourage parameter sets that produce invalid model output. Default `999`. |
| `parallel` | Logical. Run in parallel? Default `TRUE`. |
| `ncore` | Integer. Number of cores if `parallel = TRUE`. Default `parallel::detectCores() - 1`. |
| `timeout` | Numeric. Max runtime in seconds. Default `Inf`. |
| `N` | Integer. Base sample size. |
| `vars_sim` | Named list describing output variables. |
| `...` | Must be empty. Additional arguments are not allowed. |

Here is an example for examining surface temperature (surf_temp) in the
months December to February, bottom temperature (bot_temp), (10 - 13 m)
and also total chlorophyll-a (PHY_tchla) at the surface (0 - 2 m) during
the summer period.

``` r

ctrl <- create_sa_control(N = 2^4, ncore = 2, na_value = 999,
                          parallel = TRUE, file_name = "results.db",
                          vars_sim = list(
                            surf_temp = list(var = "HYD_temp",
                                             month = c(12, 1:2),
                                             depth_range = c(0, 2) 
                            ),
                            bot_temp = list(var = "HYD_temp",
                                            month = c(12, 1:2),
                                            depth_range = c(10, 13)
                            ),
                            surf_chla = list(var = "PHY_tchla",
                                             month = c(12, 1:2),
                                             depth_range = c(0, 2)
                            )
                          )
)
```

## Run sensitivity analysis

Once we have defined the fitness function, control parameters and
variables, we can run the sensitivity analysis. The `sa_aeme` function
takes the following arguments:

``` r

?sa_aeme
```

|         |                 |
|---------|----------------:|
| sa_aeme | R Documentation |

## Run sensitivity analysis on AEME model parameters

### Arguments

|  |  |
|----|----|
| `aeme` | aeme; object. |
| `model` | vector; of models to be used. Can be 'dy_cd', 'glm_aed', 'gotm_wet'. |
| `param` | dataframe; of parameters read in from a csv file. Requires the columns c("model", "file", "name", "value", "min", "max", "log") |
| `FUN_list` | list of functions; named according to the variables in the `vars_sim`. Funtions are of the form `⁠function(df)⁠` which will be used to calculate model fit. If NULL, uses mean absolute error (MAE). |
| `path` | filepath; where input files are located relative to the current working directory. |
| `model_controls` | dataframe; of configuration loaded from "model_controls.csv". |
| `ctrl` | list; of controls for sensitivity analysis function created using the `create_control` function. See create_control for more details. |
| `param_df` | dataframe; of parameters to be used in the calibration. Requires the columns c("model", "file", "name", "value", "min", "max"). This is used to restart from a previous calibration. |

The `sa_aeme` function writes the results to the file specified. The
`sa_aeme` function returns the `sim_id` of the run.

``` r

# Run sensitivity analysis AEME model
sim_id <- sa_aeme(aeme = aeme, path = path, param = param,
                  model = model, ctrl = ctrl, FUN_list = FUN_list)
#> ℹ Extracting variable indices for "gotm_wet" modelled 
#> variables "HYD_temp" and "PHY_tchla". [2026-05-12 03:44:34]
#> ✔ Variable indices extracted for "gotm_wet". 
#> [2026-05-12 03:44:39]
#> ℹ Starting parallel sensitivity analysis for 
#> "gotm_wet" using 2 cores with 
#> 144 parameter sets. 
#> [2026-05-12 03:44:39]
#>        turbulence/turb_param/k_min light_extinction/A/constant_value
#> mean                     4.851e-06                           0.52760
#> median                   5.000e-06                           0.52700
#> sd                       2.799e-06                           0.06984
#>        light_extinction/g1/constant_value light_extinction/g2/constant_value
#> mean                              0.59460                             1.3590
#> median                            0.59000                             1.2920
#> sd                                0.08189                             0.6979
#>        MET_wndspd MET_radswd inflow
#> mean       0.9965     0.9983 1.4930
#> median     1.0000     1.0000 1.5000
#> sd         0.1619     0.1606 0.5311
#> ✔ Parallel sensitivity analysis for 
#> "gotm_wet" completed. 
#> [2026-05-12 03:52:52]
#> Writing output for generation 1 to results.db with sim ID:
#> "LID45819_gotmwet_S_001" [2026-05-12 03:52:52]
```

## Reading sensitivity analysis results

The sensitivity results can be read in using the `read_sa` function.
This function takes the following arguments:

- `ctrl`: The control parameters used for the sensitivity analysis.
- `model`: The model used for the sensitivity analysis.
- `path`: The path to the directory where the model is configuration is.

``` r

# Read in sensitivity analysis results
sa_res <- read_sa(ctrl = ctrl, sim_id = sim_id, R = 10^3)
names(sa_res)
#> [1] "LID45819_gotmwet_S_001"
```

The `read_sa` function returns a list for each simulation id provided.
This list contains the following elements:

- `df`: dataframe of the sensitivity analysis results. The dataframe
  contains the model, generation, index (model run), parameter name,
  parameter value, fitness value and the median fitness value for each
  generation.

``` r

head(sa_res[[1]]$df)
```

| sim_id | model | run | gen | parameter_name | parameter_value | fit_type | fit_value | label |
|:---|:---|---:|---:|:---|---:|:---|---:|:---|
| LID45819_gotmwet_S_001 | gotm_wet | 1 | 1 | NA/turbulence/turb_param/k_min | 0.000005 | surf_temp | 22.0189 | k_min |
| LID45819_gotmwet_S_001 | gotm_wet | 1 | 1 | NA/turbulence/turb_param/k_min | 0.000005 | bot_temp | 20.4503 | k_min |
| LID45819_gotmwet_S_001 | gotm_wet | 1 | 1 | NA/turbulence/turb_param/k_min | 0.000005 | surf_chla | 6.3296 | k_min |
| LID45819_gotmwet_S_001 | gotm_wet | 1 | 1 | NA/light_extinction/A/constant_value | 0.527000 | surf_temp | 22.0189 | A |
| LID45819_gotmwet_S_001 | gotm_wet | 1 | 1 | NA/light_extinction/A/constant_value | 0.527000 | bot_temp | 20.4503 | A |
| LID45819_gotmwet_S_001 | gotm_wet | 1 | 1 | NA/light_extinction/A/constant_value | 0.527000 | surf_chla | 6.3296 | A |

- `sobol_indices`: list of the Sobol indices for each variable an it’s
  senstivity to the parameters.

``` r

sa_res[[1]]$sobol_indices
#> $surf_temp
#> 
#> First-order estimator: saltelli | Total-order estimator: jansen 
#> 
#> Total number of model runs: 144 
#> 
#> Sum of first order indices: 0.4635491 
#>        original         bias  std.error        low.ci     high.ci sensitivity
#>           <num>        <num>      <num>         <num>       <num>      <char>
#>  1:  0.71576426 -0.034470747 4.87805193  -8.810571079 10.31104110          Si
#>  2:  0.20051875  0.084402891 1.26682798  -2.366821365  2.59905308          Si
#>  3: -0.03513634 -0.128522490 3.95953187  -7.667153708  7.85392601          Si
#>  4:  0.27946626 -0.236376669 6.41641022 -12.060090014 13.09177587          Si
#>  5: -0.44228483 -0.047906816 5.52683236 -11.226770385 10.43801436          Si
#>  6: -1.39490641 -0.183565092 6.05016751 -13.069451728 10.64676910          Si
#>  7:  1.14012737 -0.098203953 7.01817006 -12.517029227 14.99369186          Si
#>  8:  0.47184592  0.033826567 0.22857451  -0.009978452  0.88601716          Ti
#>  9:  0.03228854  0.002527557 0.01203157   0.006179531  0.05334243          Ti
#> 10:  0.30847843  0.026377035 0.15260705  -0.017002932  0.58120573          Ti
#> 11:  0.74495089  0.051032587 0.23934917   0.224802555  1.16303405          Ti
#> 12:  0.51532721  0.047539602 0.17949190   0.115989955  0.81958526          Ti
#> 13:  0.63022194  0.056467379 0.20589659   0.170204659  0.97730445          Ti
#> 14:  0.87734815  0.058235306 0.31069533   0.210161180  1.42806450          Ti
#>     parameters
#>         <char>
#>  1:      k_min
#>  2:          A
#>  3:         g1
#>  4:         g2
#>  5:     wndspd
#>  6:     radswd
#>  7:     inflow
#>  8:      k_min
#>  9:          A
#> 10:         g1
#> 11:         g2
#> 12:     wndspd
#> 13:     radswd
#> 14:     inflow
#> 
#> $bot_temp
#> 
#> First-order estimator: saltelli | Total-order estimator: jansen 
#> 
#> Total number of model runs: 144 
#> 
#> Sum of first order indices: 9.109506 
#>        original         bias std.error      low.ci    high.ci sensitivity
#>           <num>        <num>     <num>       <num>      <num>      <char>
#>  1:  0.37317571  0.791535505 4.2034626 -8.65699509  7.8202755          Si
#>  2: -0.03223781  0.616004751 2.8173412 -6.17012984  4.8736447          Si
#>  3:  0.34710276  0.542605292 3.7832436 -7.61052366  7.2195186          Si
#>  4:  4.57169255  0.168471249 4.9192624 -5.23835574 14.0447983          Si
#>  5:  0.44533168  0.290526711 4.0883425 -7.85819905  8.1678090          Si
#>  6:  1.48541819  0.219908823 4.0594496 -6.69086565  9.2218844          Si
#>  7:  1.91902266  0.344223363 4.8046519 -7.84214546 10.9917441          Si
#>  8:  0.54991949  0.029333970 0.2463440  0.03776008  1.0034109          Ti
#>  9:  0.29899064 -0.003993716 0.1775138 -0.04493622  0.6509049          Ti
#> 10:  0.37860346  0.051748277 0.1817229 -0.02931518  0.6830256          Ti
#> 11:  0.84054820  0.056094114 0.3206319  0.15602707  1.4128811          Ti
#> 12:  0.45498732  0.056711601 0.1734129  0.05839270  0.7381587          Ti
#> 13:  0.38230716  0.076837547 0.2153503 -0.11660920  0.7275484          Ti
#> 14:  0.57930726  0.091285568 0.2412933  0.01509556  0.9609478          Ti
#>     parameters
#>         <char>
#>  1:      k_min
#>  2:          A
#>  3:         g1
#>  4:         g2
#>  5:     wndspd
#>  6:     radswd
#>  7:     inflow
#>  8:      k_min
#>  9:          A
#> 10:         g1
#> 11:         g2
#> 12:     wndspd
#> 13:     radswd
#> 14:     inflow
#> 
#> $surf_chla
#> 
#> First-order estimator: saltelli | Total-order estimator: jansen 
#> 
#> Total number of model runs: 144 
#> 
#> Sum of first order indices: 3.231496 
#>         original          bias std.error      low.ci   high.ci sensitivity
#>            <num>         <num>     <num>       <num>     <num>      <char>
#>  1: -0.003194931  0.0521502112 0.4352753 -0.90846904 0.7977788          Si
#>  2:  0.775080036  0.0290589789 0.9334707 -1.08354798 2.5755901          Si
#>  3:  0.722150324  0.0025389135 0.7684915 -0.78660425 2.2258271          Si
#>  4: -0.134908891 -0.1002469873 0.8553445 -1.71110641 1.6417826          Si
#>  5:  0.915069584  0.0303290980 0.8524489 -0.78602861 2.5555096          Si
#>  6:  0.287039936 -0.0466480664 0.6494899 -0.93928875 1.6066648          Si
#>  7:  0.670260076  0.0005486812 0.7882882 -0.87530506 2.2147278          Si
#>  8:  0.166944446  0.0302882411 0.1655499 -0.18781561 0.4611280          Ti
#>  9:  0.790844781  0.0489921253 0.4033703 -0.04873861 1.5324439          Ti
#> 10:  0.427809920  0.0110362844 0.3463463 -0.26205267 1.0955999          Ti
#> 11:  0.601081795  0.0271508140 0.1755866  0.22978750 0.9180745          Ti
#> 12:  0.667729016  0.0014841582 0.3182652  0.04245662 1.2900331          Ti
#> 13:  0.435069842 -0.0033658699 0.1347292  0.17437128 0.7025001          Ti
#> 14:  0.618488509  0.0151041309 0.1979392  0.21543073 0.9913380          Ti
#>     parameters
#>         <char>
#>  1:      k_min
#>  2:          A
#>  3:         g1
#>  4:         g2
#>  5:     wndspd
#>  6:     radswd
#>  7:     inflow
#>  8:      k_min
#>  9:          A
#> 10:         g1
#> 11:         g2
#> 12:     wndspd
#> 13:     radswd
#> 14:     inflow
```

- `sobol_dummy`: list of the Sobol indices for the dummy parameter.

``` r

sa_res[[1]]$sobol_dummy
#> $surf_temp
#>   original          bias  std.error   low.ci high.ci sensitivity parameters
#> 1 1.975274 -5.400448e-05 0.03921562 1.898467 2.05219          Si      dummy
#> 2 0.000000  9.370624e-04 0.41982846 0.000000 0.00000          Ti      dummy
#> 
#> $bot_temp
#>   original        bias  std.error   low.ci   high.ci sensitivity parameters
#> 1 1.819478 0.003962476 0.08843149 1.642193 1.9888379          Si      dummy
#> 2 0.000000 0.005971816 0.65361858 0.000000 0.2623828          Ti      dummy
#> 
#> $surf_chla
#>    original       bias std.error low.ci   high.ci sensitivity parameters
#> 1 0.3057422 0.05488964 0.1606518      0 0.5657242          Si      dummy
#> 2 0.0000000 0.00721663 0.7286813      0 1.1373847          Ti      dummy
```

## Visualising sensitivity analysis results

The sensitivity analysis results can be visualised in different ways
using the functions: `plot_uncertainty`, `plot_scatter` and
`plot_multiscatter`. These plots are based on the output plots from the
`sensobol` package.

These functions take the following argument:

- `sa_res`: The sensitivity analysis results returned from the `read_sa`
  function.

### Uncertainty plot

The `plot_uncertainty` function plots the distribution of the model
output for each variable.

``` r

# Plot sensitivity analysis results
plot_uncertainty(sa_res)
#> Dropped 0 NA's from 432 rows for sim_id LID45819_gotmwet_S_001
```

![](sensitivity-analysis_files/figure-html/plot-sa-uncertainty-1.png)

### Scatter plot

The `plot_scatter` function plots the model output against the parameter
value for each variable. This is useful for identifying relationships
between the model output and the parameter value. For example, the plot
below shows that there is a relationship between the model surface
temperature (surf_temp\_) and the parameter value of the scaling factor
for shortwave radiation (MET_radswd), and also for surface chlorophyll-a
(surf_chla) and the light extinction coefficient (light.Kw). When there
is a low parameter value for Kw, the model chlorophyll-a is higher.

``` r

plot_scatter(sa_res)
```

![](sensitivity-analysis_files/figure-html/plot-sa-scatter-1.png)

### Multi-scatter plot

The `plot_multiscatter` function plots the parameters against each other
for each variable. The parameter on top is the x-axis and the parameter
below is the y-axis. This is useful for identifying relationships
between the parameters and response variable.

``` r

pl <- plot_multiscatter(sa_res)

pl[[1]][1]
#> $surf_temp
```

![](sensitivity-analysis_files/figure-html/plot-sa-multiscatter-1.png)

``` r


pl[[1]][2]
#> $bot_temp
```

![](sensitivity-analysis_files/figure-html/plot-sa-multiscatter-2.png)

``` r


pl[[1]][3]
#> $surf_chla
```

![](sensitivity-analysis_files/figure-html/plot-sa-multiscatter-3.png)

|                   |                 |
|-------------------|----------------:|
| create_sa_control | R Documentation |

## Create control list for sensitivity analysis

### Arguments

|  |  |
|----|----|
| `file_type` | Character. Output type: `"csv"` or `"db"`. Default `"db"`. |
| `file_name` | Character. Output file name. Defaults to `"results.db"` (db) or `"simulation_metadata.csv"` (csv). |
| `file_dir` | Character. Output directory. Default `"calib_sa"`. |
| `na_value` | Numeric. Penalty value substituted for `NA` fit values during optimisation to discourage parameter sets that produce invalid model output. Default `999`. |
| `parallel` | Logical. Run in parallel? Default `TRUE`. |
| `ncore` | Integer. Number of cores if `parallel = TRUE`. Default `parallel::detectCores() - 1`. |
| `timeout` | Numeric. Max runtime in seconds. Default `Inf`. |
| `N` | Integer. Base sample size. |
| `vars_sim` | Named list describing output variables. |
| `...` | Must be empty. Additional arguments are not allowed. |

|         |                 |
|---------|----------------:|
| sa_aeme | R Documentation |

## Run sensitivity analysis on AEME model parameters

### Arguments

|  |  |
|----|----|
| `aeme` | aeme; object. |
| `model` | vector; of models to be used. Can be 'dy_cd', 'glm_aed', 'gotm_wet'. |
| `param` | dataframe; of parameters read in from a csv file. Requires the columns c("model", "file", "name", "value", "min", "max", "log") |
| `FUN_list` | list of functions; named according to the variables in the `vars_sim`. Funtions are of the form `⁠function(df)⁠` which will be used to calculate model fit. If NULL, uses mean absolute error (MAE). |
| `path` | filepath; where input files are located relative to the current working directory. |
| `model_controls` | dataframe; of configuration loaded from "model_controls.csv". |
| `ctrl` | list; of controls for sensitivity analysis function created using the `create_control` function. See create_control for more details. |
| `param_df` | dataframe; of parameters to be used in the calibration. Requires the columns c("model", "file", "name", "value", "min", "max"). This is used to restart from a previous calibration. |
