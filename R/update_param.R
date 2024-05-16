#' Update parameter values in param based on best_pars
#'
#' @inheritParams get_param
#' @param param A data frame with parameters to update
#' @param best_pars A data frame with the best parameters from `get_param`.
#' Defaults to NULL. When NULL, `get_param` is called to get the best
#' parameters.
#'
#' @return data frame with updated parameter values for running the model with
#'  `run_aeme_param`
#' @export

update_param <- function(param, calib, na_value,
                         fit_col = "fit", best_pars = NULL) {

  if (is.null(best_pars)) {
    best_pars <- get_param(calib = calib, na_value = na_value,
                           fit_col = fit_col, best = TRUE)
  }

  for (i in seq_len(nrow(best_pars))) {
    idx <- grepl(best_pars$name[i], param$name) & grepl(best_pars$model[i],
                                                          param$model)
    if (sum(idx) == 0)
      stop("Parameter ", best_pars$param2[i], " not found in param")

    if (sum(idx) > 1)
      stop("Parameter ", best_pars$param2[i], " found in multiple places for ",
                           best_pars$model[i])
    param[idx, "value"] <- best_pars$parameter_value[i]
  }
  param
}
