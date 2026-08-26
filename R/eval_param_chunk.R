#' Evaluate one chunk of candidate parameter sets for a single model.
#'
#' Runs the model once per row of `pars_i`, updating `param$value` for each
#' candidate parameter set and recording the resulting fit (and per-variable
#' fit components) back onto `pars_i`. This is the per-worker unit of work
#' shared by the serial (`lapply`) and parallel (`parallel::parLapply`)
#' calibration loops in [calib_aeme()], so the evaluation logic only needs to
#' be maintained in one place.
#'
#' @param pars_i dataframe; one chunk of candidate parameter sets (rows are
#' members, columns are `param$name_full`).
#' @param path string; directory to run the model in.
#' @param model string; model to run, e.g. "glm_aed".
#' @param parallel logical; if `TRUE`, calls `run_and_fit()` via the
#' `aemetools::` namespace so the call resolves correctly on cluster worker
#' processes, which have not attached the package.
#' @inheritParams calib_aeme
#' @inheritParams run_and_fit
#'
#' @return `pars_i` with a `fit` column (and one column per `vars_sim` entry)
#' added, containing the model fit for each candidate parameter set.
#' @noRd
eval_param_chunk <- function(pars_i, path, aeme, param, model, vars_sim,
                             FUN_list, model_controls, ctrl, var_indices,
                             weights, include_wlev, parallel = FALSE) {

  pars_i[["fit"]] <- NA
  for (v in vars_sim) {
    pars_i[[v]] <- NA
  }

  for (p in seq_len(nrow(pars_i))) {

    # Update the parameter value in the parameter table
    for (n in names(pars_i)) {
      param$value[param$name_full == n] <- pars_i[p, n]
    }

    # On cluster workers the package is not attached, so `run_and_fit()`
    # must be namespace-qualified to resolve.
    if (parallel) {
      res <- aemetools::run_and_fit(aeme = aeme, param = param, model = model,
                                    path = path, vars_sim = vars_sim,
                                    FUN_list = FUN_list,
                                    model_controls = model_controls,
                                    na_value = ctrl$na_value,
                                    var_indices = var_indices,
                                    return_indices = FALSE,
                                    include_wlev = include_wlev, fit = TRUE,
                                    weights = weights, timeout = ctrl$timeout)
    } else {
      res <- run_and_fit(aeme = aeme, param = param, model = model,
                         path = path, vars_sim = vars_sim,
                         FUN_list = FUN_list, model_controls = model_controls,
                         na_value = ctrl$na_value, var_indices = var_indices,
                         return_indices = FALSE, include_wlev = include_wlev,
                         fit = TRUE, weights = weights,
                         timeout = ctrl$timeout)
    }

    for (v in vars_sim) {
      pars_i[[v]][p] <- res[[v]]
    }

    if (any(is.na(unlist(res)))) {
    # if (any(is_failed_fit(unlist(res), ctrl))) {
      res1 <- ctrl$na_value
    } else {
      res1 <- sum(unlist(res))
      res1 <- ifelse(is.na(res1), ctrl$na_value, res1)
    }

    pars_i[["fit"]][p] <- res1
    print(res1)
  }
  pars_i
}
