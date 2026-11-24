#' Get best parameter values from calibration results
#'
#' @inheritParams plot_calib
#' @inheritParams get_param
#' @inheritParams lifecycle::deprecate_warn
#'
#' @importFrom dplyr case_when filter group_by mutate summarise left_join select
#' @importFrom dplyr rename arrange
#' @importFrom stringr str_split_i
#'
#' @return A data frame with the parameter values.
#' @export
get_best_params <- function(calib, fit_col = "fit", na_value = NULL) {
  
  na_value <- resolve_na_value(na_value = na_value, calib = calib)  
  all_pars <- get_sim_params(calib = calib, na_value = na_value, 
                             fit_col = fit_col)
  param <- calib$parameter_metadata |> 
    dplyr::mutate(
      parameter_name = encode_param(group = group, name = name, index = index)
    ) |> 
    dplyr::select(sim_id, model, file, name, group, index, parameter_name)
  
  pars_df <- all_pars |>
    dplyr::filter(fit_value != na_value, !is.na(fit_value)) |>
    dplyr::group_by(sim_id, fit_type, parameter_name) |>
    dplyr::summarise(
      label = label[which.min(fit_value)],
      gen = gen[which.min(fit_value)],
      min = min(parameter_value), 
      max = max(parameter_value),
      parameter_value = parameter_value[which.min(fit_value)],
      par = par[which.min(fit_value)],
      fit_value = min(fit_value),
      .groups = "drop"
    ) |> 
    dplyr::select(sim_id, parameter_name, parameter_value, min, max, fit_type,
                  fit_value, gen) |> 
    dplyr::rename(value = parameter_value)
  
  param_names <- AEME::param_colnames(incl_opt = FALSE)
  param_df <- param |> 
    dplyr::left_join(pars_df, by = c("sim_id", "parameter_name")) |> 
    dplyr::arrange(sim_id, model, file, name, group, index) |> 
    dplyr::select(dplyr::all_of(c("sim_id", param_names, "fit_value", "gen",
                                  "fit_type"))) 
  return(param_df)
}
