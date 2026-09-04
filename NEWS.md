# aemetools 0.3.0.9000

## New features

* Three helpers for assembling a `param` dataframe ahead of a (staged)
  calibration, factored out of the `staged-calibration` script:
  * `set_param_log()` populates the logical `log` column that
    `pest_param_table()` and `pest_prior_cov()` read, flagging any strictly
    positive parameter whose `[min, max]` range spans at least `ratio`
    (default one order of magnitude).
  * `freeze_param()` sets `min == max == value` for named parameters (or
    all of them). Paired with the `partrans = "fixed"` support, this is how
    a parameter is held at a value while staying visible in the PEST
    parameter map, ensembles and sensitivity output. `carry_param()` is the
    common case: it takes a finished `read_calib()` run (or a
    `get_best_params()` frame), keeps/drops by name, and freezes the rest -
    ready to `rbind()` onto the next stage's adjustable set.
  * `check_param_targets()` compares each `param$name` against the fields
    present in `AEME::configuration(aeme)[[model]]` after a build and
    returns (or errors on) the rows that match nothing, catching a typo or
    an inactive module before it becomes a silent no-op or a forward-run
    error.
* `pest_posterior_params()` extracts the posterior parameter ensemble from a
  finished `pestpp-ies` run as a list of runnable `param` dataframes (one per
  realisation), with frozen parameters carried through unchanged.
  `iteration = 0` returns the prior instead.
* `run_aeme_ensemble()` gains a `param_sets` argument: given a list of `param`
  dataframes, or a long data.frame with an `ensemble` id column, it runs that
  supplied ensemble instead of sampling from the parameter bounds. The
  sampling and supplied paths now share one internal parallel engine.
* `ensemble_summary()` extracts an ensemble once into a tidy object holding
  the per-date (and per-depth, per-model, per-variable) `mean`, `sd`, `n` and
  quantile bands, the underlying per-member frame, and the depth-aligned
  observations. `plot_ensemble()` now accepts that object directly, so one
  extraction can back many plots (variables, depths, intervals) instead of
  re-running `AEME::get_var()` over every member on each call. `plot_ensemble()`
  on an `aeme` is unchanged in behaviour and just routes through it.
* `score_ensemble()` is added as a documented stub for forthcoming ensemble
  verification scores (coverage, ensemble-mean bias/RMSE, spread-skill ratio,
  CRPS).
* A frozen parameter (`value == min == max`) in a PEST calibration is now
  written to the control file as `partrans = "fixed"` instead of being baked
  into the model configuration and dropped. It stays visible in the parameter
  map, the `pestpp-ies` ensembles, `pest_param_summary()` and
  `pest_posterior_params()`. Built-in calibration engines are unchanged.
* `create_pest_control(prior_par_ensemble = )` also accepts a finished
  `pestpp-ies` run (a `read_calib()` object, a control, or a run directory).
  The new run's prior ensemble is seeded from that run's posterior:
  parameters shared by name carry their posterior marginals and correlations,
  parameters new to this run are drawn from their prior. This is the intended
  way to chain the stages of a staged calibration - see
  `?vignette("staged-calibration")` and `inst/scripts/staged-calibration.R`.

## Breaking changes

* `nse()`, `kge()`, `kge_prime()` and `log_kge()` now return the
  **conventional** statistic, where `1` is a perfect fit and higher is
  better - previously they returned `-1 *` that value. Calibration
  (`calib_aeme()`, `run_and_fit()`) minimises `FUN_list` entries, so pass the
  new `nse_loss()`, `kge_loss()`, `kge_prime_loss()` or `log_kge_loss()`
  companions (each `-1 *` the corresponding statistic) as calibration
  objectives instead of the bare metric. `mae()`, `rmse()` and `pbias()` are
  unchanged - they are already `0`-is-best and minimise-oriented, so they
  have no `_loss` companion.

## Compatibility

* Track the AEME (>= 0.4.0) lake-observations schema, which replaces the
  `depth_from` / `depth_to` column pair with a single `depth` column. All
  consumers of `observations(aeme)$lake` (`pest_obs_table()`, `run_and_fit()`,
  `get_calib_periods()`, `ensemble_summary()`) now read `depth` directly and
  still accept the legacy `depth_from` / `depth_to` layout, collapsing it to
  the interval midpoint - the same value they computed before. A latent
  half-thickness calculation in `run_aeme_param()` (unused) was removed.

## Bug fixes

* PEST++ runs now bind a free TCP port for the PANTHER master instead of
  always using the configured `port` (default `4004`). Concurrent
  calibration or sensitivity runs - one per lake, one per stage - all
  targeted the same port, and agents orphaned by an aborted run kept
  retrying it; the next master would accept those stale agents, reject them
  all and stall out. `port` is now the preferred starting point of a
  search: the run takes it if free and otherwise steps to the next free
  port.
* `read_pest_ensemble()` now always returns `realisation` as a character
  column. PEST++ quotes the realisation labels in some ensemble files and
  not others, so the column was typed as character for one iteration and
  integer for another; binding the prior and posterior together
  (`plot_pest_ensemble()`, `pest_param_summary()`) then aborted with a type
  mismatch.
* The PSOCK clusters started by `calib_aeme()`, `sa_aeme()` and
  `run_aeme_ensemble()` now allow 600 s (was `parallel`'s default 120 s)
  for a worker to come up, since a worker that activates `renv` on start
  can spend minutes building the package sandbox before it connects -
  previously read as "worker failed to connect" and collapsed to a serial
  fallback. Override with `AEMETOOLS_CLUSTER_SETUP_TIMEOUT`.

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
