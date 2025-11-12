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

| model    | file          | name                               |   value |    min |      max | module       | group |
|:---------|:--------------|:-----------------------------------|--------:|-------:|---------:|:-------------|:------|
| glm_aed  | glm3.nml      | light/Kw                           | 5.8e-01 |  0.100 | 5.52e+00 | hydrodynamic | NA    |
| glm_aed  | met           | MET_wndspd                         | 1.0e+00 |  0.700 | 1.30e+00 | hydrodynamic | NA    |
| glm_aed  | met           | MET_radswd                         | 1.0e+00 |  0.700 | 1.30e+00 | hydrodynamic | NA    |
| glm_aed  | glm3.nml      | mixing/coef_mix_conv               | 1.4e-01 |  0.100 | 2.00e-01 | hydrodynamic | NA    |
| glm_aed  | glm3.nml      | mixing/coef_wind_stir              | 2.1e-01 |  0.200 | 3.00e-01 | hydrodynamic | NA    |
| glm_aed  | glm3.nml      | mixing/coef_mix_shear              | 1.4e-01 |  0.100 | 2.00e-01 | hydrodynamic | NA    |
| glm_aed  | glm3.nml      | mixing/coef_mix_turb               | 5.6e-01 |  0.200 | 7.00e-01 | hydrodynamic | NA    |
| glm_aed  | glm3.nml      | mixing/coef_mix_hyp                | 7.4e-01 |  0.400 | 8.00e-01 | hydrodynamic | NA    |
| glm_aed  | inf           | inflow                             | 1.0e+00 |  0.500 | 2.50e+00 | hydrodynamic | NA    |
| gotm_wet | gotm.yaml     | turbulence/turb_param/k_min        | 6.0e-07 |  0.000 | 1.00e-05 | hydrodynamic | NA    |
| gotm_wet | gotm.yaml     | light_extinction/A/constant_value  | 5.5e-01 |  0.395 | 6.59e-01 | hydrodynamic | NA    |
| gotm_wet | gotm.yaml     | light_extinction/g1/constant_value | 5.9e-01 |  0.440 | 7.40e-01 | hydrodynamic | NA    |
| gotm_wet | gotm.yaml     | light_extinction/g2/constant_value | 2.0e-01 |  0.050 | 2.70e+00 | hydrodynamic | NA    |
| gotm_wet | met           | MET_wndspd                         | 1.0e+00 |  0.700 | 1.30e+00 | hydrodynamic | NA    |
| gotm_wet | met           | MET_radswd                         | 1.0e+00 |  0.700 | 1.30e+00 | hydrodynamic | NA    |
| gotm_wet | inf           | inflow                             | 1.0e+00 |  0.500 | 2.50e+00 | hydrodynamic | NA    |
| dy_cd    | cfg           | light_extinction_coefficient/7     | 9.0e-01 |  0.100 | 1.40e+00 | hydrodynamic | NA    |
| dy_cd    | dyresm3p1.par | vert_mix_coeff/15                  | 2.0e+02 | 50.000 | 7.50e+02 | hydrodynamic | NA    |
| dy_cd    | met           | MET_wndspd                         | 1.0e+00 |  0.700 | 1.30e+00 | hydrodynamic | NA    |
| dy_cd    | met           | MET_radswd                         | 1.0e+00 |  0.700 | 1.30e+00 | hydrodynamic | NA    |
| dy_cd    | inf           | inflow                             | 1.0e+00 |  0.500 | 2.50e+00 | hydrodynamic | NA    |

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
?create_control
```

|                |                 |
|----------------|----------------:|
| create_control | R Documentation |

## Create control list for calibration or sensitivity analysis

### Arguments

[TABLE]

Here is an example for examining surface temperature (surf_temp) in the
months December to February, bottom temperature (bot_temp), (10 - 13 m)
and also total chlorophyll-a (PHY_tchla) at the surface (0 - 2 m) during
the summer period.

``` r
ctrl <- create_control(method = "sa", N = 2^4, ncore = 2, na_value = 999,
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

|                  |                                                                                                                                                                                                    |
|------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `aeme`           | aeme; object.                                                                                                                                                                                      |
| `model`          | vector; of models to be used. Can be 'dy_cd', 'glm_aed', 'gotm_wet'.                                                                                                                               |
| `param`          | dataframe; of parameters read in from a csv file. Requires the columns c("model", "file", "name", "value", "min", "max", "log")                                                                    |
| `FUN_list`       | list of functions; named according to the variables in the `vars_sim`. Funtions are of the form `⁠function(df)⁠` which will be used to calculate model fit. If NULL, uses mean absolute error (MAE). |
| `path`           | filepath; where input files are located relative to the current working directory.                                                                                                                 |
| `model_controls` | dataframe; of configuration loaded from "model_controls.csv".                                                                                                                                      |
| `ctrl`           | list; of controls for sensitivity analysis function created using the `create_control` function. See create_control for more details.                                                              |
| `param_df`       | dataframe; of parameters to be used in the calibration. Requires the columns c("model", "file", "name", "value", "min", "max"). This is used to restart from a previous calibration.               |

The `sa_aeme` function writes the results to the file specified. The
`sa_aeme` function returns the `sim_id` of the run.

``` r
# Run sensitivity analysis AEME model
sim_id <- sa_aeme(aeme = aeme, path = path, param = param,
                  model = model, ctrl = ctrl, FUN_list = FUN_list)
#> Extracting indices for gotm_wet modelled variables [2025-11-12 20:26:02]
#> Complete! [2025-11-12 20:26:06]
#> Running sensitivity analysis in parallel for gotm_wet using 2 cores with 144 parameter sets [2025-11-12 20:26:06]
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
#> Completed gotm_wet! [2025-11-12 20:31:47]
#> Writing output for generation 1 to results.db with sim ID: 45819_gotmwet_S_001 [2025-11-12 20:31:47]
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
#> [1] "45819_gotmwet_S_001"
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

| sim_id              | model    | run | gen | parameter_name                       | parameter_value | fit_type  | fit_value | label |
|:--------------------|:---------|----:|----:|:-------------------------------------|----------------:|:----------|----------:|:------|
| 45819_gotmwet_S_001 | gotm_wet |   1 |   1 | NA/turbulence/turb_param/k_min       |        0.000005 | surf_temp |  21.91650 | k_min |
| 45819_gotmwet_S_001 | gotm_wet |   1 |   1 | NA/turbulence/turb_param/k_min       |        0.000005 | bot_temp  |  20.22430 | k_min |
| 45819_gotmwet_S_001 | gotm_wet |   1 |   1 | NA/turbulence/turb_param/k_min       |        0.000005 | surf_chla |   6.24598 | k_min |
| 45819_gotmwet_S_001 | gotm_wet |   1 |   1 | NA/light_extinction/A/constant_value |        0.527000 | surf_temp |  21.91650 | A     |
| 45819_gotmwet_S_001 | gotm_wet |   1 |   1 | NA/light_extinction/A/constant_value |        0.527000 | bot_temp  |  20.22430 | A     |
| 45819_gotmwet_S_001 | gotm_wet |   1 |   1 | NA/light_extinction/A/constant_value |        0.527000 | surf_chla |   6.24598 | A     |

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
#> Sum of first order indices: 2.127721 
#>        original         bias  std.error        low.ci     high.ci sensitivity
#>           <num>        <num>      <num>         <num>       <num>      <char>
#>  1:  0.49083628 -0.025826140 4.89872502  -9.084662190 10.11798702          Si
#>  2: -0.04096671  0.067685224 1.21898746  -2.497823443  2.28051958          Si
#>  3:  0.43322929 -0.053266339 3.89464012  -7.146858741  8.11985000          Si
#>  4:  0.51255738 -0.145059341 6.41476732 -11.915096190 13.23032964          Si
#>  5:  0.13530236  0.063750795 5.47713400 -10.663433815 10.80653695          Si
#>  6: -0.75341857 -0.060804981 6.01386636 -12.479575072 11.09434788          Si
#>  7:  1.35018080 -0.018078639 6.91436812 -12.183653048 14.92017193          Si
#>  8:  0.47583080  0.034146451 0.22891666  -0.006984053  0.89035275          Ti
#>  9:  0.02898283  0.002515951 0.01156242   0.003804956  0.04912881          Ti
#> 10:  0.30216373  0.025931493 0.15367313  -0.024961561  0.57742604          Ti
#> 11:  0.74838557  0.051558761 0.24670791   0.213288201  1.18036543          Ti
#> 12:  0.50914266  0.047275673 0.18225032   0.104662919  0.81907106          Ti
#> 13:  0.62928751  0.057090478 0.21032292   0.159971690  0.98442238          Ti
#> 14:  0.85658482  0.056247565 0.29857362   0.215143709  1.38553080          Ti
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
#> Sum of first order indices: 10.79209 
#>       original         bias std.error      low.ci    high.ci sensitivity
#>          <num>        <num>     <num>       <num>      <num>      <char>
#>  1:  0.1626769  0.718806860 3.9716235 -8.34036888  7.2281091          Si
#>  2: -0.2018673  0.540606294 2.6638272 -5.96347900  4.4785318          Si
#>  3:  0.9184955  0.679296074 3.7781129 -7.16576585  7.6441647          Si
#>  4:  4.5767636  0.190991900 4.6815851 -4.78996648 13.5615099          Si
#>  5:  1.1747192  0.494180183 3.9770148 -7.11426674  8.4753448          Si
#>  6:  2.1405822  0.426202491 3.8650435 -5.86096628  9.2897258          Si
#>  7:  2.0207200  0.408100667 4.4299077 -7.06984028 10.2950790          Si
#>  8:  0.5592932  0.029894877 0.2518134  0.03585318  1.0229435          Ti
#>  9:  0.3216517 -0.006350182 0.1902465 -0.04487443  0.7008782          Ti
#> 10:  0.4023476  0.051262616 0.1999342 -0.04077886  0.7429489          Ti
#> 11:  0.8612998  0.053424933 0.3458418  0.13003746  1.4857123          Ti
#> 12:  0.4667618  0.055546307 0.1922529  0.03440674  0.7880242          Ti
#> 13:  0.3721075  0.075105502 0.2211737 -0.13649057  0.7304946          Ti
#> 14:  0.5265778  0.089222981 0.2401321 -0.03329550  0.9080051          Ti
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
#> Sum of first order indices: 1.550933 
#>        original       bias std.error     low.ci   high.ci sensitivity
#>           <num>      <num>     <num>      <num>     <num>      <char>
#>  1:  0.32269124 0.01023520 0.5659217 -0.7967301 1.4216422          Si
#>  2:  1.05027188 0.11306256 0.9990551 -1.0209026 2.8953213          Si
#>  3: -0.30316633 0.29567904 1.0323232 -2.6221617 1.4244709          Si
#>  4: -0.15825364 0.36295002 1.2030186 -2.8790767 1.8366694          Si
#>  5: -0.01156204 0.37263086 1.2558619 -2.8456370 2.0772512          Si
#>  6:  0.29820771 0.49429548 1.7427490 -3.6118130 3.2196375          Si
#>  7:  0.35274448 0.56672764 1.7669414 -3.6771247 3.2491584          Si
#>  8:  0.18557630 0.03732472 0.1514318 -0.1485494 0.4450525          Ti
#>  9:  0.76615571 0.15992460 0.5411401 -0.4543841 1.6668463          Ti
#> 10:  0.61202724 0.11811155 0.4088231 -0.3073628 1.2951942          Ti
#> 11:  0.78401451 0.18349901 0.5937370 -0.5631877 1.7642187          Ti
#> 12:  0.84361060 0.19849079 0.6142423 -0.5587730 1.8490126          Ti
#> 13:  1.53857955 0.46191530 1.7958231 -2.4430844 4.5964129          Ti
#> 14:  1.48390095 0.50695979 1.6213401 -2.2008271 4.1547094          Ti
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
#>   original          bias  std.error   low.ci  high.ci sensitivity parameters
#> 1 1.973813 -9.618136e-05 0.03948901 1.896512 2.051306          Si      dummy
#> 2 0.000000  7.459994e-04 0.42578510 0.000000 0.000000          Ti      dummy
#> 
#> $bot_temp
#>   original        bias  std.error   low.ci   high.ci sensitivity parameters
#> 1 1.788663 0.004781789 0.09725557 1.593264 1.9744986          Si      dummy
#> 2 0.000000 0.003311261 0.68567059 0.000000 0.3838079          Ti      dummy
#> 
#> $surf_chla
#>    original        bias std.error low.ci   high.ci sensitivity parameters
#> 1 0.4045744  0.08401036 0.2742260      0 0.8580371          Si      dummy
#> 2 0.0000000 -0.08171439 0.8925427      0 1.2171421          Ti      dummy
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
#> Dropped 0 NA's from 432 rows for sim_id 45819_gotmwet_S_001
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
