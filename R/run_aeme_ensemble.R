#' Run an AEME parameter ensemble
#'
#' @description
#' Runs a model once per parameter set and stores every member's output on
#' the returned `aeme` object under `AEME::output()$ens_*`, ready for
#' [plot_ensemble()].
#'
#' By default the members are **sampled**: `n` parameter vectors are drawn
#' from each parameter's `[min, max]` (normally or uniformly, per `dist`).
#' Pass `param_sets` to run a **supplied** ensemble instead - for example the
#' posterior from a `pestpp-ies` calibration via [pest_posterior_params()] -
#' in which case `n`, `dist` and `param` are ignored.
#'
#' @inheritParams calib_aeme
#' @param n numeric; number of ensemble members to sample. Ignored when
#'   `param_sets` is supplied.
#' @param dist character; distribution to sample from, `"norm"` (default) or
#'   `"unif"`. Ignored when `param_sets` is supplied.
#' @param parallel logical; run members in parallel. Default `FALSE`.
#' @param ncore numeric; cores for parallel processing. `NULL` (default) uses
#'   one fewer than are available.
#' @param param data.frame; parameter template supplying `min`/`max` for the
#'   sampling path. `NULL` (default) takes it from the `aeme` object. Ignored
#'   when `param_sets` is supplied.
#' @param param_sets a supplied ensemble to run instead of sampling. Either a
#'   list of long `param` dataframes (one per member, `value` set) - such as
#'   an [pest_posterior_params()] result - or a single long data.frame with
#'   an `ensemble` id column plus the `param` columns.
#' @param na_value numeric; value a failed member run returns. Default 999;
#'   such members are dropped from the ensemble.
#'
#' @importFrom parallel parLapply clusterExport stopCluster detectCores
#' @importFrom parallel makeCluster
#' @importFrom dplyr filter mutate select distinct
#' @importFrom methods is
#' @importFrom rlang arg_match
#' @importFrom AEME check_aeme check_model get_aeme_path get_lake_dir
#' @importFrom AEME configuration output parameters
#'
#' @return The `aeme` object with `AEME::output()$ens_001 .. ens_00k` and
#'   `$n_members` populated. `attr(AEME::output(aeme), "realisation")` carries
#'   the surviving realisation ids when `param_sets` was named.
#' @seealso [pest_posterior_params()], [plot_ensemble()]
#' @export
run_aeme_ensemble <- function(aeme, model, n = 10, dist = c("norm", "unif"),
                              path = ".",
                              parallel = FALSE, ncore = NULL, param = NULL,
                              param_sets = NULL, na_value = 999) {

  # Check inputs
  aeme <- AEME::check_aeme(aeme)
  model <- AEME::check_model(model)
  if (missing(path)) {
    path <- AEME::get_aeme_path(aeme)
  } else {
    path <- AEME::check_path(path)
  }
  dist <- rlang::arg_match(dist)

  sets <- NULL
  if (!is.null(param_sets)) {
    sets <- .normalise_param_sets(param_sets)
    if (!missing(n) && !identical(n, 10)) {
      cli::cli_inform(c("i" = "{.arg n} is ignored when {.arg param_sets} is
                        supplied; running {length(sets)} member{?s}."))
    }
    n <- length(sets)
  } else if (is.null(param)) {
    param <- AEME::parameters(aeme)
    if (nrow(param) == 0) stop("No parameters found in aeme object")
  }

  if (dist == "norm") {
    FUN <- rnorm_limits
  } else if (dist == "unif") {
    FUN <- runif
  }

  mod_list <- lapply(model, function(m) {

    members <- if (is.null(sets)) {
      model_pars <- dplyr::filter(param, model == m)
      # n x nrow(model_pars); matrix() guards the n == 1 collapse.
      draws <- matrix(
        vapply(seq_len(nrow(model_pars)), function(i)
          FUN(n = n, min = model_pars$min[i], max = model_pars$max[i]),
          numeric(n)),
        nrow = n)
      lapply(seq_len(n), function(k) {
        mp <- model_pars
        mp$value <- draws[k, ]
        mp
      })
    } else {
      lapply(sets, function(s) s[s$model == m, , drop = FALSE])
    }

    .run_member_ensemble(aeme = aeme, model = m, member_params = members,
                         path = path, parallel = parallel, ncore = ncore,
                         na_value = na_value)
  })
  names(mod_list) <- model

  .assemble_ens_output(aeme, mod_list, model)
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
