#' Sample and run an AEME parameter ensemble
#'
#' @description
#' Draws `n` parameter vectors from each parameter's `[min, max]` (normally or
#' uniformly, per `dist`) and runs the model once per vector with
#' [AEME::run_aeme_ensemble()]. Every member's output is stored on the returned
#' `aeme` object under `AEME::output()$ens_*`, ready for [plot_ensemble()].
#'
#' To run parameter sets you already have - for example the posterior from a
#' `pestpp-ies` calibration via [pest_posterior_params()] - call
#' [AEME::run_aeme_ensemble()] directly. Zone-ratio parameters (see
#' [zone_ratio_param()]) are expanded for you only by this function, so pass
#' such sets through [expand_zone_ratios()] first.
#'
#' @inheritParams calib_aeme
#' @param n numeric; number of ensemble members to sample.
#' @param dist character; distribution to sample from, `"norm"` (default) or
#'   `"unif"`.
#' @param param data.frame; parameter template supplying `min`/`max`. `NULL`
#'   (default) takes it from the `aeme` object.
#' @param parallel logical; run members in parallel. Default `FALSE`.
#' @param ncore numeric; cores for parallel processing. `NULL` (default) uses
#'   one fewer than are available.
#' @param model_controls,timeout passed to [AEME::run_aeme_ensemble()].
#'
#' @importFrom AEME check_aeme check_model get_aeme_path parameters
#' @importFrom rlang arg_match
#'
#' @return The `aeme` object with `AEME::output()$ens_001 .. ens_00k` and
#'   `$n_members` populated. Members that fail to run are dropped with a
#'   warning.
#' @seealso [AEME::run_aeme_ensemble()], [pest_posterior_params()],
#'   [plot_ensemble()]
#' @export
sample_aeme_ensemble <- function(aeme,
                                 model = NULL,
                                 path = NULL,
                                 n = 10,
                                 dist = c("norm", "unif"),
                                 param = NULL,
                                 parallel = FALSE,
                                 ncore = NULL,
                                 model_controls = NULL,
                                 timeout = Inf) {

  # Check inputs
  aeme <- AEME::check_aeme(aeme)
  model <- if (is.null(model)) AEME::list_models(aeme) else model
  model <- AEME::check_model(model)
  path <- if (is.null(path)) AEME::get_aeme_path(aeme) else AEME::check_path(path)
  dist <- rlang::arg_match(dist)

  if (is.null(param)) {
    param <- AEME::parameters(aeme)
    if (nrow(param) == 0) stop("No parameters found in aeme object")
  }

  FUN <- if (dist == "norm") rnorm_limits else stats::runif

  # One draw matrix per model (n x nrow(model_pars)); matrix() guards the
  # n == 1 collapse.
  by_model <- lapply(model, function(m) {
    model_pars <- dplyr::filter(param, model == m)
    draws <- matrix(
      vapply(seq_len(nrow(model_pars)), function(i)
        FUN(n = n, min = model_pars$min[i], max = model_pars$max[i]),
        numeric(n)),
      nrow = n)
    list(pars = model_pars, draws = draws)
  })

  # Member k holds the k-th draw for every requested model.
  sets <- lapply(seq_len(n), function(k) {
    dplyr::bind_rows(lapply(by_model, function(x) {
      mp <- x$pars
      mp$value <- x$draws[k, ]
      mp
    }))
  })

  .run_param_sets(aeme = aeme, model = model, sets = sets, path = path,
                  parallel = parallel, ncore = ncore,
                  model_controls = model_controls, timeout = timeout)
}

#' Run an AEME parameter ensemble
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `run_aeme_ensemble()` is deprecated. Use [sample_aeme_ensemble()] to sample
#' members from the parameter bounds, or [AEME::run_aeme_ensemble()] to run
#' supplied parameter sets (note that zone-ratio parameters must then be
#' expanded with [expand_zone_ratios()] first).
#'
#' @inheritParams sample_aeme_ensemble
#' @param param_sets a supplied ensemble to run instead of sampling. Either a
#'   list of long `param` dataframes (one per member, `value` set) - such as
#'   an [pest_posterior_params()] result - or a single long data.frame with
#'   an `ensemble` id column plus the `param` columns. When supplied, `n`,
#'   `dist` and `param` are ignored.
#' @param na_value `r lifecycle::badge("deprecated")` no longer used; members
#'   that fail to run are dropped.
#'
#' @return The `aeme` object with `AEME::output()$ens_001 .. ens_00k` and
#'   `$n_members` populated.
#' @seealso [sample_aeme_ensemble()], [AEME::run_aeme_ensemble()]
#' @export
run_aeme_ensemble <- function(aeme,
                              model = NULL,
                              path = NULL,
                              n = 10,
                              dist = c("norm", "unif"),
                              param = NULL,
                              param_sets = NULL,
                              parallel = FALSE,
                              ncore = NULL,
                              na_value = lifecycle::deprecated()) {

  if (is.null(param_sets)) {
    lifecycle::deprecate_warn("0.3.0.9000", "run_aeme_ensemble()",
                              "sample_aeme_ensemble()")
  } else {
    lifecycle::deprecate_warn("0.3.0.9000", "run_aeme_ensemble()",
                              "AEME::run_aeme_ensemble()")
  }
  if (lifecycle::is_present(na_value)) {
    lifecycle::deprecate_warn("0.3.0.9000", "run_aeme_ensemble(na_value)")
  }

  if (is.null(param_sets)) {
    return(sample_aeme_ensemble(aeme = aeme, model = model, path = path,
                                n = n, dist = dist, param = param,
                                parallel = parallel, ncore = ncore))
  }

  aeme <- AEME::check_aeme(aeme)
  model <- if (is.null(model)) AEME::list_models(aeme) else model
  model <- AEME::check_model(model)
  path <- if (is.null(path)) AEME::get_aeme_path(aeme) else AEME::check_path(path)

  .run_param_sets(aeme = aeme, model = model, sets = param_sets, path = path,
                  parallel = parallel, ncore = ncore)
}

#' Expand zone ratios in each member, then run via AEME
#'
#' A long data.frame with an `ensemble` id column is split into members here
#' (first-appearance order) so the zone-ratio expansion, which works on one
#' member at a time, can be applied before handing a list to
#' [AEME::run_aeme_ensemble()].
#' @noRd
.run_param_sets <- function(aeme, model, sets, path, parallel, ncore,
                            model_controls = NULL, timeout = Inf) {

  if (is.data.frame(sets)) {
    id <- intersect(c("ensemble", "realisation", ".ens"), names(sets))
    if (length(id) == 0) {
      cli::cli_abort(c(
        "A data.frame {.arg param_sets} needs an {.field ensemble} column
         identifying each realisation.",
        "i" = "Columns present: {.val {names(sets)}}."
      ))
    }
    id <- id[[1]]
    sets <- split(sets[, setdiff(names(sets), id), drop = FALSE],
                  factor(sets[[id]], levels = unique(sets[[id]])))
  }
  sets <- lapply(unclass(sets), function(d) {
    if (is.data.frame(d)) expand_zone_ratios(d) else d
  })

  AEME::run_aeme_ensemble(aeme = aeme, model = model, param_sets = sets,
                          path = path, parallel = parallel, ncore = ncore,
                          model_controls = model_controls, timeout = timeout)
}

#' Generate a normal distribution with a min and a max
#'
#' @noRd
#'
rnorm_limits <- function(n, min, max) {
  x <- rnorm(n)
  x <- (max - min) * x/diff(range(x))
  return(x - min(x) + min)
}
