#' Update parameter values in param based on best_pars
#'
#' @param param A data frame with parameters to update
#' @param best_pars A data frame with the best parameters from `get_param`
#'
#' @return data frame with updated parameter values for running the model with
#'  `run_aeme_param`
#' @export

update_param <- function(param, best_pars) {

  for (i in seq_len(nrow(best_pars))) {
    idx <- grepl(best_pars$param2[i], param$name) & grepl(best_pars$model[i],
                                                          param$model)
    if (sum(idx) == 0)
      stop("Parameter ", best_pars$param2[i], " not found in param")

    if (sum(idx) > 1)
      stop("Parameter ", best_pars$param2[i], " found in multiple places for ",
                           best_pars$model[i])
    param[idx, "value"] <- best_pars$value[i]
  }
  param
}
