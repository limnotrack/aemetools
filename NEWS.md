# aemetools 0.3.0.9000

## New features

* `pest_posterior_params()` extracts the posterior parameter ensemble from a
  finished `pestpp-ies` run as a list of runnable `param` dataframes (one per
  realisation), with frozen parameters carried through unchanged.
  `iteration = 0` returns the prior instead.
* `run_aeme_ensemble()` gains a `param_sets` argument: given a list of `param`
  dataframes, or a long data.frame with an `ensemble` id column, it runs that
  supplied ensemble instead of sampling from the parameter bounds. The
  sampling and supplied paths now share one internal parallel engine.

# aemetools 0.3.0

This is a large development release that overhauls the calibration and
sensitivity-analysis workflow, adds multi-objective (Pareto) calibration,
and moves model execution onto the AEME CLI. Version jumps from
`0.0.0.9000` to `0.3.0`.

## Breaking changes

* Model runs now require **AEME (>= 0.2.0)** and use the AEME command-line
  interface (`run_aeme()` from AEME) instead of calling the models directly.
  The `AEME` remote is tracked on the `@dev` branch.
* `create_control()` is **deprecated** and superseded by the dedicated
  constructors `create_calib_control()` (calibration) and
  `create_sa_control()` (sensitivity analysis).
* `get_param()` has been reworked. It now delegates to `get_best_params()`
  / `get_sim_params()` and gains `best`, `fit_col` and `quantile`
  arguments; the previous return shape has changed.
* Parameter names are now encoded/decoded consistently. Output tables and
  plots use the new `encode_param()` / `decode_param()` /
  `decode_param_full()` / `display_param_name()` helpers, which changes
  some column and label values.
* `calib_aeme()`, `sa_aeme()` and `run_and_fit()` have been substantially
  refactored; several arguments were renamed, reordered or removed.

## New features

### Calibration control and fitting

* `create_calib_control()` and `create_sa_control()` — separate, validated
  control-list constructors, with a `print.calib_sa_control()` method for
  clean console output.
* Exported goodness-of-fit functions: `bias()`, `pbias()`, `mae()`,
  `rmse()`, `nse()`, `kge()`, `kge_prime()` and `log_kge()`.
* `set_weights()` — set observation weights per variable / phytoplankton
  group for calibration and sensitivity analysis.
* `timeout` argument for calibration and sensitivity-analysis runs; the
  best parameter set found so far is returned if the timeout is hit.
* Global `ncore` option for controlling parallel workers.
* Calibration of stratification (`HYD_strat`) is now supported.

### Multi-objective / Pareto calibration

* New MOEDA calibration method for multi-objective optimisation.
* `get_pareto_front()` — extract the non-dominated parameter sets.
* `plot_pareto_generations()` — visualise Pareto fronts across generations.
* `create_param_var_matrix()` and `edit_param_var_matrix()` build and edit
  a parameter covariance matrix used when drawing the next generation of
  parameters; a default `param_var_matrix` dataset is shipped with the
  package.

### Parameter helpers and inspection

* `get_best_params()` — best parameter set from a calibration result.
* `get_sim_params()` — parameter sets below a fit quantile threshold.
* `encode_param()`, `decode_param()`, `decode_param_full()` — round-trip
  parameter-name encoding shared with AEME.
* `edit_parameters_shiny()` — interactive Shiny/`miniUI` gadget for editing
  parameter ranges in an `rhandsontable` grid.

### Plotting

* Plotting code split into focused files and functions:
  `plot_calib_convergence()`, `plot_calib_dotty()`,
  `plot_calib_histogram()` and `plot_calib_summary()`.
* `plot_calib()` reworked for clearer output.

### Spatial data

* `get_depth_contours()` — generate lake depth contours.
* DEM retrieval can now use New Zealand LiDAR data via LINZ basemap tiles.
* limnotrack API database query support (`api_request()` and helpers).

## Improvements

* User-facing messages migrated to the **cli** package (`cli_abort()`,
  `cli_inform()`, `cli_warn()`, etc.) for consistent, styled output.
* More robust error handling: model crashes, missing output files, and
  missing weights / `vars_sim` now return `NA` or a clear error instead of
  failing hard.
* Lifecycle badges added to the documentation for stable, experimental,
  superseded and deprecated functions.
* pkgdown site: added favicons and updated the package URL to
  <https://limnotrack.com/aemetools/>.
* Test suite reorganised — calibration tests are split per model
  (`glm`, `gotm`, `dyresm`, `simstrat`, multi-model and MOEDA) with shared
  AEME cache helpers.

## Dependencies

* Added: `cli`, `corpcor`, `glue`, `lifecycle`, `methods`, `miniUI`,
  `rhandsontable`.
* Removed: `forcats`, `plyr`, `reshape2`, `yaml`, `zip`.
* `AEME` bumped to `>= 0.2.0` (remote: `github::limnotrack/AEME@dev`).
* Documentation regenerated with roxygen2 8.0.0.
