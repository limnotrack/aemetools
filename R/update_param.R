#' Update parameter values in param based on best_pars
#'
#' @inheritParams AEME::build_aeme
#' @inheritParams plot_calib
#' @inheritParams get_param
#' @param param A data frame with parameters to update. Defaults to NULL. When
#' NULL, the parameter values are extracted from `calib$parameter_metadata`.
#' @param best_pars A data frame with the best parameters from \code{\link{get_param}}.
#' Defaults to NULL. When NULL, \code{\link{get_param}} is called to get the 
#' best parameters.
#' @param aeme aeme; object. Defaults to NULL. When NULL, a dataframe of the
#' updated parameter values is returned. When provided, the updated parameter
#' values are added to the aeme object and the aeme object is returned.
#' @param replace Logical. If TRUE, the parameter values in the aeme object are
#' replaced with the updated values. Defaults to FALSE. Only used when aeme is
#' provided.
#' @param quantile `r lifecycle::badge("deprecated")` The quantile to use for 
#' the top quantile of the fit_value. Defaults to 0.1. This is no longer needed 
#' and will be removed in a future version.
#' @param na_value `r lifecycle::badge("deprecated")` Numeric. Penalty value 
#' substituted for \code{NA} fit values, this is no longer needed as NA values 
#' are now written to simulation_data in output of calib_aeme() and sa_aeme(). 
#' The argument will be removed in a future version.
#'
#' @importFrom dplyr filter group_by select summarise all_of anti_join arrange
#' @importFrom dplyr bind_rows
#'
#' @return data frame with updated parameter values for running the model with
#'  \code{\link{run_aeme_param}}
#' @export

update_param <- function(calib, param, aeme, replace = FALSE,
                         fit_col = "fit", best_pars, quantile = 0.1, 
                         na_value = NULL) {
  
  param_column_names <- AEME::param_colnames(incl_opt = FALSE)
  if (missing(param)) {
    param <- calib$parameter_metadata |>
      dplyr::select(all_of(param_column_names)) |> 
      dplyr::mutate(
        name_full = encode_param(group, name, index)
      )
  }
  na_value <- resolve_na_value(na_value = na_value, calib = calib)
  
  if (missing(best_pars)) {
    best_pars <- get_best_params(calib = calib, fit_col = fit_col) |> 
      dplyr::mutate(
        name_full = encode_param(group, name, index)
      )
  }
  pars <- get_sim_params(calib = calib, fit_col = fit_col)

  
  for (i in seq_len(nrow(best_pars))) {
    idx <- best_pars$name_full[i] == param$name_full &
      grepl(best_pars$model[i], param$model)

    if (sum(idx) == 0) {
      cli::cli_alert_warning(
        "Parameter {.val {best_pars$name[i]}} not found in param, adding it."
      )
      # Add to param
      new_row <- best_pars[i, ] |> 
        dplyr::select(dplyr::all_of(param_column_names))
      new_row$min <- min_max$min[j]
      new_row$max <- min_max$max[j]
      param <- dplyr::bind_rows(param, new_row)
      next
      
    }
    
    if (sum(idx) > 1) {
      cli::cli_abort(
        "Parameter {.val {best_pars$name[i]}} found in multiple places for
        {.val {best_pars$model[i]}}"
      )
    }
    param[idx, "value"] <- best_pars$value[i]
    param[idx, "min"] <- best_pars$min[i]
    param[idx, "max"] <- best_pars$max[i]
  }
  # param$value %in% best_pars$value
  param <- param |>
    dplyr::select(dplyr::all_of(param_column_names))
  
  if (missing(aeme)) {
    return(param)
  } else {
    if (replace) {
      AEME::parameters(aeme) <- param
    } else {
      old_pars <- AEME::parameters(aeme)
      if (nrow(old_pars) > 0) {
        par_diff <- dplyr::anti_join(old_pars, param, by = c("model", "file",
                                                             "name", "group",
                                                             "index"))
        param <- dplyr::bind_rows(par_diff, param) |>
          dplyr::arrange(model, group, name)
      }
      AEME::parameters(aeme) <- param
    }
    return(aeme)
  }
}
