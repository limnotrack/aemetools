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
#' @param quantile The quantile to use for the top quantile of the fit_value.
#' Defaults to 0.1.
#'
#' @importFrom dplyr filter group_by select summarise all_of anti_join arrange
#' @importFrom dplyr bind_rows
#'
#' @return data frame with updated parameter values for running the model with
#'  \code{\link{run_aeme_param}}
#' @export

update_param <- function(calib, param, na_value, aeme, replace = FALSE, quantile = 0.1,
                         fit_col = "fit", best_pars) {
  
  param_column_names <- AEME::param_colnames(incl_opt = FALSE)
  if (missing(param)) {
    param <- calib$parameter_metadata |>
      dplyr::select(all_of(param_column_names)) |> 
      dplyr::mutate(
        name_full = encode_param(group, name, index)
      )
  }
  if (missing(na_value)) {
    na_value <- calib$calibration_metadata$na_value[1]
  }

  if (missing(best_pars)) {
    best_pars <- get_param(calib = calib, na_value = na_value,
                           fit_col = fit_col, best = TRUE,
                           quantile = quantile) |> 
      dplyr::mutate(
        name_full = encode_param(group, name, index)
      )
  }
  pars <- get_param(calib = calib, na_value = na_value,
                    fit_col = fit_col, best = FALSE)
  
  # Calculate min/max for each sim_id and parameter for the top quantile of the
  # fit_value
  # min_max <- pars |>
  #   dplyr::filter(fit_value != na_value) |>
  #   dplyr::group_by(sim_id, parameter_name) |>
  #   dplyr::filter(fit_value <= quantile(fit_value, quantile)) |>
  #   dplyr::summarise(min = min(parameter_value),
  #                    value = parameter_value[which.min(fit_value)],
  #                    max = max(parameter_value), 
  #                    .groups = "drop") |> 
  #   dplyr::rename(name_full = parameter_name) |>
  #   dplyr::mutate(
  #     decode_param_full(name_full)
  #   )
  
  for (i in seq_len(nrow(best_pars))) {
    idx <- best_pars$name_full[i] == param$name_full &
      grepl(best_pars$model[i], param$model)
    # j <- which(min_max$name_full == best_pars$name_full[i] &
    #              min_max$sim_id == best_pars$sim_id[i])
    
    # if (is.na(best_pars$group[i])) {
    #   idx <- grepl(best_pars$name[i], param$name) &
    #     grepl(best_pars$model[i], param$model)
    #   j <- which(min_max$name == best_pars$name[i] &
    #                min_max$sim_id == best_pars$sim_id[i])
    # } else {
    #   idx <- grepl(best_pars$name[i], param$name) &
    #     grepl(best_pars$model[i], param$model) &
    #     grepl(best_pars$group[i], param$group)
    #   j <- which(min_max$name == best_pars$name[i] &
    #                min_max$sim_id == best_pars$sim_id[i] &
    #                min_max$group == best_pars$group[i])
    # 
    # }
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
