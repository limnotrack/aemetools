#' Get parameter values from calibration results
#' 
#' @description
#' Retrieves parameter values from calibration results, with optional filtering 
#' based on fit values. This function is useful for extracting all parameter 
#' sets used in the calibration process, and can be filtered to include only 
#' those with fit values below a specified quantile threshold.
#'
#' @param calib A list with the calibration results loaded using
#' \code{\link{read_calib}}.
#' @inheritParams get_param
#' @inheritParams lifecycle::deprecate_warn
#' @param quantile_threshold Numeric. A value between 0 and 1 specifying the 
#' quantile threshold for selecting the best parameter values based on their fit
#'  values. For example, a value of 0.1 will select parameter values with fit 
#'  values in the lowest 10% of the distribution. Default is 1 (no filtering).
#'
#' @importFrom dplyr case_when filter group_by mutate summarise
#' @importFrom stringr str_split_i
#'
#' @return A data frame with all the parameter values.
#' @export

get_sim_params <- function(calib, fit_col = "fit", na_value = NULL,
                           quantile_threshold = 1) {
  
  if (!all(fit_col %in% calib$simulation_data$fit_type)) {
    cli::cli_abort("fit_col {.val {fit_col}} not found in 
                   {.code calib$simulation_data$fit_type}")
  }
  if (!is.numeric(quantile_threshold) || length(quantile_threshold) != 1 || 
      quantile_threshold < 0 || quantile_threshold > 1) {
    cli::cli_abort("{.arg quantile_threshold} must be a single number between 0
                   and 1.")
  }
  
  na_value <- resolve_na_value(na_value = na_value, calib = calib)
  
  sim_ids <- calib$simulation_metadata$sim_id
  
  all_pars <- lapply(sim_ids, \(x) {
    model <- calib$simulation_metadata |>
      dplyr::filter(sim_id == x) |>
      dplyr::pull(model)
    
    df_idx <- calib$simulation_data |>
      dplyr::filter(sim_id == x, fit_type == "fit") |>
      tidyr::pivot_wider(id_cols = c("gen", "run"), names_from = parameter_name,
                         values_from = parameter_value) |>
      dplyr::arrange(gen, run) |>
      dplyr::mutate(index = dplyr::row_number()) |>
      as.data.frame() |>
      dplyr::select(gen, run, index)
    
    calib$simulation_data |>
      dplyr::filter(sim_id == x) |>
      dplyr::left_join(df_idx, by = c("gen", "run")) |>
      dplyr::filter(
        fit_type %in% fit_col
      ) |>
      dplyr::mutate(
        model = model,
        fit2 = dplyr::case_when(
          fit_value == na_value ~ NA_real_,
          .default = fit_value
        )) |>
      dplyr::mutate(
        gen = factor(gen),
        name = decode_param(parameter_name),
        label = abbrev_pars(parameter_name, model),
        group = stringr::str_split_i(parameter_name, "/", 1),
        par = stringr::str_split_i(label, "%", 2)
      ) |>
      dplyr::mutate(group = dplyr::case_when(
        group == "NA" ~ NA,
        .default = group
      ))
  }) |>
    dplyr::bind_rows() |>
    dplyr::select(sim_id, model, gen, run, index, dplyr::everything())
  
  if (quantile_threshold < 1) {
    qtile <- all_pars |>
      dplyr::filter(!is.na(fit2)) |>
      dplyr::group_by(sim_id) |>
      dplyr::summarise(
        cutoff = quantile(fit_value, probs = quantile_threshold, na.rm = TRUE),
        .groups = "drop"
      )
    all_pars <- all_pars |>
      dplyr::left_join(qtile, by = "sim_id") |>
      dplyr::filter(!is.na(fit2), fit2 <= cutoff) |>
      dplyr::select(-cutoff)
  }
  
  return(all_pars)
}
