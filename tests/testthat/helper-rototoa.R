# Fixtures for the Lake Rototoa (LID50270) PEST tests.
#
# The shipped object is a real monitoring record - 26 m deep, GLM-AED
# configured, ~40k lake observations of 33 variables spanning 1988-2025 plus
# ~3.9k water-level readings - which the AEME test lake is not. That is the
# point of testing against it: the observation window filtering, the
# per-variable weighting and the .pst assembly all behave differently on a
# record where most observations fall outside the simulation window and a
# third of the variables are not simulatable at all.
#
# Its own simulation window is nine years with a five-year spin-up, far too
# slow for a test, so the fixtures below shorten it. Anything that depends
# on the shipped window reads it from the object rather than hard-coding it.

#' Cores to give a local PEST run: 85% of what the machine has.
#'
#' PANTHER agents are separate processes, so this is a real core count, not
#' a thread count. Floored at 1 so a 2-core CI runner still works.
pest_ncore <- function(frac = 0.85) {
  max(1L, floor(frac * parallel::detectCores()))
}

#' The shipped Rototoa object, output stripped, unmodified otherwise.
rototoa_aeme <- function() {
  f <- system.file("extdata/LID50270_Rototoa.rds", package = "aemetools")
  # devtools::load_all() serves inst/ directly; an installed package serves
  # the same path from its root. Fall back to the source tree so the tests
  # run either way.
  if (!nzchar(f)) f <- testthat::test_path("..", "..", "inst", "extdata",
                                           "LID50270_Rototoa.rds")
  skip_if_not(file.exists(f), "LID50270_Rototoa.rds not found")
  readRDS(f)
}

#' Rototoa over a tractable window: `years` years ending 30 June, with a
#' one-year spin-up rather than the shipped five.
rototoa_window <- function(aeme = rototoa_aeme(), years = 1, spin_up = 365) {
  stop <- as.Date("2021-06-30")
  AEME::set_time(aeme, start = format(stop - 365 * years + 1), stop = format(stop),
                 spin_up = spin_up)
}

#' Every variable a calibration on `aeme` could actually target: observed
#' inside the simulation window *and* switched on in `model_controls`.
#'
#' Derived rather than hard-coded, so the set stays correct as AEME's
#' controls change. Water level comes from `observations()$level`, not
#' `$lake`, and is included - it is a first-class residual-mode target.
rototoa_vars <- function(aeme = rototoa_window()) {
  mc <- AEME::configuration(aeme)$model_controls
  sim_ok <- mc$var_aeme[mc$simulate]

  tme <- AEME::time(aeme)
  obs <- AEME::observations(aeme)
  inw <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(character())
    d <- df[df$Date >= as.Date(tme$start) & df$Date <= as.Date(tme$stop) &
              !is.na(df$value), ]
    unique(d$var_aeme)
  }
  sort(intersect(union(inw(obs$lake), inw(obs$level)), sim_ok))
}

#' Targets whose simulated equivalent is structurally non-finite, and which
#' therefore cannot be residual-mode targets at all.
#'
#' `pest_forward_run()` fails a run if *any* simulated value is non-finite,
#' by design - it cannot tell a sentinel from a real value, so padding would
#' corrupt the ensemble silently. But these NAs are structural rather than a
#' one-off crash, so they recur on every evaluation and every realisation
#' fails. Measured on a one-year Rototoa run: 26 non-finite values in 2964.
#'
#' * `PHY_cyano` - 17 of 17. This AED configuration never produces it.
#' * `CHM_oxycln` - 8 of 100, and `CHM_oxymet` - 1 of 11. Oxycline and
#'   metalimnetic oxygen are undefined on a day the lake is not stratified.
#'
#' Excluded from the end-to-end runs, not from the interface tests: the .pst
#' assembly handles them fine, it is the forward run that cannot.
rototoa_nonfinite_vars <- c("PHY_cyano", "CHM_oxycln", "CHM_oxymet")

#' `rototoa_vars()` less the ones above - every variable that can actually
#' be calibrated end to end, water level included.
rototoa_calibratable <- function(aeme = rototoa_window()) {
  setdiff(rototoa_vars(aeme), rototoa_nonfinite_vars)
}

.rototoa_cache <- new.env(parent = emptyenv())

#' A built (and run) Rototoa for `model`, cached for the session and handed
#' out as a fresh directory copy - same contract as get_cached_aeme_run(),
#' but for the shipped object rather than AEME's test lake.
#'
#' `vars_sim = NULL` (the default) switches on every targetable variable,
#' water level included; `use_bgc` defaults to TRUE because most of them are
#' biogeochemical.
rototoa_run <- function(model = "glm_aed", vars_sim = NULL, years = 1,
                        use_bgc = TRUE, run = TRUE) {

  aeme <- rototoa_window(years = years)
  vars_sim <- vars_sim %||% rototoa_vars(aeme)
  key <- paste(model, paste(vars_sim, collapse = ","), years, use_bgc, run,
               sep = "_")

  if (is.null(.rototoa_cache[[key]])) {
    mc <- AEME::set_vars_sim(AEME::get_model_controls(), vars_sim = vars_sim)
    build_path <- tempfile("rototoa_build_")
    dir.create(build_path, recursive = TRUE)

    aeme <- AEME::build_aeme(path = build_path, aeme = aeme, model = model,
                             model_controls = mc, ext_elev = 5,
                             use_bgc = use_bgc)
    if (run) {
      aeme <- AEME::run_aeme(aeme = aeme, model = model, path = build_path)
    }
    .rototoa_cache[[key]] <- list(aeme = aeme, build_path = build_path,
                                  vars_sim = vars_sim)
  }

  cached <- .rototoa_cache[[key]]
  new_path <- tempfile("rototoa_")
  dir.create(new_path)
  file.copy(list.files(cached$build_path, full.names = TRUE), new_path,
            recursive = TRUE)
  list(aeme = cached$aeme, path = new_path, vars_sim = cached$vars_sim)
}

#' KGE for every variable in `vars`, as calib_aeme()/validate_aeme() want it.
rototoa_fun_list <- function(vars) {
  stats::setNames(lapply(vars, function(v) kge), vars)
}

#' Trim an object's observations to its simulation window.
#'
#' `get_calib_periods()` splits the whole observation record and takes no
#' view on the simulation window, so on Rototoa - observed 1988-2025, run
#' over one or two years - it returns periods the built model cannot be run
#' over. Trimming first keeps both periods inside the window.
rototoa_trim_obs <- function(aeme) {
  tme <- AEME::time(aeme)
  obs <- AEME::observations(aeme)
  keep <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    df[df$Date >= as.Date(tme$start) & df$Date <= as.Date(tme$stop), ,
       drop = FALSE]
  }
  obs$lake <- keep(obs$lake)
  obs$level <- keep(obs$level)
  AEME::observations(aeme) <- obs
  aeme
}

#' Does `run_and_fit()` report the dates the model wrote for `vars`?
#'
#' Guards the end-to-end tests against a known failure: with
#' `use_bgc = TRUE` *and* `LKE_lvlwtr` among `vars_sim`,
#' `run_and_fit(return_indices = TRUE)` returns zero dates for the gridded
#' variables, so `pest_obs_table()` then discards every observation and
#' aborts with "No observations ... within the simulation period". The same
#' variable set works with `use_bgc = FALSE`, and BGC works without
#' `LKE_lvlwtr`; it is the combination that fails. Not a PEST fault - the
#' indices are wrong before PEST is involved.
rototoa_indices_ok <- function(aeme, path, vars, param = rototoa_param()) {
  vi <- try(suppressMessages(run_and_fit(
    aeme = aeme, param = param, model = "glm_aed", path = path,
    FUN_list = rototoa_fun_list(vars), vars_sim = vars,
    weights = set_weights(vars), return_indices = TRUE,
    include_wlev = "LKE_lvlwtr" %in% vars, fit = FALSE)), silent = TRUE)
  if (inherits(vi, "try-error")) return(FALSE)
  gridded <- setdiff(vars, "LKE_lvlwtr")
  length(gridded) == 0 ||
    all(vapply(gridded, function(v) length(vi[[v]]$dates) > 0, logical(1)))
}

#' A handful of calibratable parameters for `model`, straight from AEME's
#' shipped set.
rototoa_param <- function(model = "glm_aed", n = 4) {
  utils::data("aeme_parameters", package = "AEME",
              envir = environment())
  p <- aeme_parameters[aeme_parameters$model == model, ]
  p <- p[!duplicated(p$name), ]
  as.data.frame(utils::head(p, n))
}
