#' Update parameter values in param based on best_pars
#'
#' @inheritParams get_param
#' @param param A data frame with parameters to update. Defaults to NULL. When
#' NULL, the parameter values are extracted from `calib$parameter_metadata`.
#' @param best_pars A data frame with the best parameters from `get_param`.
#' Defaults to NULL. When NULL, `get_param` is called to get the best
#' parameters.
#'
#' @return data frame with updated parameter values for running the model with
#'  `run_aeme_param`
#' @export

update_param <- function(calib, param = NULL, na_value, prob = 0.1,
                         fit_col = "fit", best_pars = NULL) {

  if (is.null(param)) {
    param_column_names <- get_param_column_names()
    param <- calib$parameter_metadata |>
      dplyr::select(all_of(param_column_names))
  }
  if (is.null(best_pars)) {
    best_pars <- get_param(calib = calib, na_value = na_value,
                           fit_col = fit_col, best = TRUE)
  }
  pars <- get_param(calib = calib, na_value = na_value,
                    fit_col = fit_col, best = FALSE)

  # Calculate min/max for each sim_id and parameter for the top prob % of the fit_value
  min_max <- pars |>
    dplyr::filter(fit_value != na_value) |>
    dplyr::group_by(sim_id, name) |>
    dplyr::filter(fit_value <= quantile(fit_value, prob)) |>
    dplyr::summarise(min = min(parameter_value),
                     max = max(parameter_value), .groups = "drop")

  for (i in seq_len(nrow(best_pars))) {
    idx <- grepl(best_pars$name[i], param$name) & grepl(best_pars$model[i],
                                                          param$model)
    j <- which(min_max$name == best_pars$name[i] & min_max$sim_id == best_pars$sim_id[i])
    if (sum(idx) == 0)
      stop("Parameter ", best_pars$param2[i], " not found in param")

    if (sum(idx) > 1)
      stop("Parameter ", best_pars$param2[i], " found in multiple places for ",
                           best_pars$model[i])
    param[idx, "value"] <- best_pars$parameter_value[i]
    param[idx, "min"] <- min_max$min[j]
    param[idx, "max"] <- min_max$max[j]
  }
  calib_col_names <- get_param_column_names()
  param <- param |>
    dplyr::select(dplyr::all_of(calib_col_names))

  return(param)
}

#' Get the column names for the calibration data frame
#'
#' @noRd
#'

get_param_column_names <- function() {
  c("model", "file", "name", "value", "min", "max", "module", "group" )
}

