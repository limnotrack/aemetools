#' Update parameter values in param based on best_pars
#'
#' @inheritParams AEME::build_aeme
#' @inheritParams get_param
#' @param param A data frame with parameters to update. Defaults to NULL. When
#' NULL, the parameter values are extracted from `calib$parameter_metadata`.
#' @param best_pars A data frame with the best parameters from `get_param`.
#' Defaults to NULL. When NULL, `get_param` is called to get the best
#' parameters.
#' @param na_value The value to replace NA values with. Defaults to NULL. When
#' NULL, the value is extracted from `calib$calibration_metadata$na_value`.
#' @param aeme aeme; object. Defaults to NULL. When NULL, a dataframe of the
#' updated parameter values is returned. When provided, the updated parameter
#' values are added to the aeme object and the aeme object is returned.
#' @param replace Logical. If TRUE, the parameter values in the aeme object are
#' replaced with the updated values. Defaults to FALSE. Only used when aeme is
#' provided.
#' @param quantile The quantile to use for the top quantile of the fit_value.
#' Defaults to 0.1.
#'
#' @importFrom dplyr filter group_by select summarise all_of anti_join arrange
#'
#' @return data frame with updated parameter values for running the model with
#'  `run_aeme_param`
#' @export

update_param <- function(calib, param = NULL, na_value = NULL, aeme = NULL,
                         replace = FALSE, quantile = 0.1, fit_col = "fit",
                         best_pars = NULL) {

  if (is.null(param)) {
    param_column_names <- get_param_column_names()
    param <- calib$parameter_metadata |>
      dplyr::select(all_of(param_column_names))
  }
  if (is.null(na_value)) {
    na_value <- calib$calibration_metadata$na_value[1]
  }
  if (is.null(best_pars)) {
    best_pars <- get_param(calib = calib, na_value = na_value,
                           fit_col = fit_col, best = TRUE)
  }
  pars <- get_param(calib = calib, na_value = na_value,
                    fit_col = fit_col, best = FALSE)

  # Calculate min/max for each sim_id and parameter for the top quantile of the
  # fit_value
  min_max <- pars |>
    dplyr::filter(fit_value != na_value) |>
    dplyr::group_by(sim_id, group, name) |>
    dplyr::filter(fit_value <= quantile(fit_value, quantile)) |>
    dplyr::summarise(min = min(parameter_value),
                     max = max(parameter_value), .groups = "drop")

  for (i in seq_len(nrow(best_pars))) {
    if (is.na(best_pars$group[i])) {
      idx <- grepl(best_pars$name[i], param$name) &
        grepl(best_pars$model[i], param$model)
      j <- which(min_max$name == best_pars$name[i] &
                   min_max$sim_id == best_pars$sim_id[i])
    } else {
      idx <- grepl(best_pars$name[i], param$name) &
        grepl(best_pars$model[i], param$model) &
        grepl(best_pars$group[i], param$group)
      j <- which(min_max$name == best_pars$name[i] &
                   min_max$sim_id == best_pars$sim_id[i] &
                   min_max$group == best_pars$group[i])

    }
    if (sum(idx) == 0)
      stop("Parameter ", best_pars$name[i], " not found in param")

    if (sum(idx) > 1)
      stop("Parameter ", best_pars$name[i], " found in multiple places for ",
                           best_pars$model[i])
    param[idx, "value"] <- best_pars$parameter_value[i]
    param[idx, "min"] <- min_max$min[j]
    param[idx, "max"] <- min_max$max[j]
  }
  calib_col_names <- get_param_column_names()
  param <- param |>
    dplyr::select(dplyr::all_of(calib_col_names))

  if (is.null(aeme)) {
    return(param)
  } else {
    if (replace) {
      AEME::parameters(aeme) <- param
    } else {
      old_pars <- AEME::parameters(aeme)
      if (nrow(old_pars) > 0) {
        par_diff <- dplyr::anti_join(old_pars, param, by = c("model", "name",
                                                             "group"))
        param <- dplyr::bind_rows(par_diff, param) |>
          dplyr::arrange(model, group, name)
      }
      AEME::parameters(aeme) <- param
    }
    return(aeme)
  }
}

#' Get the column names for the calibration data frame
#'
#' @noRd
#'

get_param_column_names <- function() {
  c("model", "file", "name", "value", "min", "max", "module", "group" )
}

