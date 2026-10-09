#' Evaluate one chunk of candidate parameter sets for a single model's
#' sensitivity analysis.
#'
#' Runs the model once per row of `pars_i`, updating `param$value` for each
#' candidate parameter set and recording the resulting per-variable output
#' (named by `nmes`, i.e. `names(ctrl$vars_sim)`) back onto `pars_i`, along
#' with a `run_failed` (0/1) column recording whether the run failed to
#' produce usable output at all. This is the per-worker unit of work shared
#' by the serial (`lapply`) and parallel (`parallel::parLapply`) sensitivity
#' analysis loops in [sa_aeme()], so the evaluation logic only needs to be
#' maintained in one place.
#'
#' `run_failed` is deliberately named with an underscore (rather than e.g.
#' `failed`) so it is swept up by the same `contains("_")` pivot that
#' `write_simulation_output()` already uses to long-format the other
#' response variables, without needing any schema changes there.
#'
#' @param pars_i dataframe; one chunk of candidate parameter sets (rows are
#' members, columns are `param$name_full`).
#' @param path string; directory to run the model in.
#' @param model string; model to run, e.g. "glm_aed".
#' @param nmes character vector; `names(ctrl$vars_sim)`, the sensitivity
#' analysis response labels to record.
#' @param parallel logical; if `TRUE`, calls `run_and_fit()` via the
#' `aemetools::` namespace so the call resolves correctly on cluster worker
#' processes, which have not attached the package.
#' @inheritParams sa_aeme
#' @inheritParams run_and_fit
#'
#' @return `pars_i` with one column per `nmes` entry plus `run_failed` added,
#' containing the model output (and failure status) for each candidate
#' parameter set.
#' @noRd
eval_param_chunk_sa <- function(pars_i, path, aeme, param, model, vars_sim,
                                FUN_list, model_controls, ctrl, var_indices,
                                weights, include_wlev, nmes, parallel = FALSE) {

  for (n in nmes) {
    pars_i[[n]] <- NA
  }
  pars_i[["run_failed"]] <- NA

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
                                    method = "sa", sa_ctrl = ctrl,
                                    weights = weights, timeout = ctrl$timeout)
    } else {
      res <- run_and_fit(aeme = aeme, param = param, model = model,
                         path = path, vars_sim = vars_sim,
                         FUN_list = FUN_list, model_controls = model_controls,
                         na_value = ctrl$na_value, var_indices = var_indices,
                         return_indices = FALSE, include_wlev = include_wlev,
                         fit = TRUE, method = "sa", sa_ctrl = ctrl,
                         weights = weights, timeout = ctrl$timeout)
    }

    for (n in nmes) {
      pars_i[[n]][p] <- res[[n]]
    }
    pars_i[["run_failed"]][p] <- as.numeric(isTRUE(res$failed))
  }
  pars_i
}
