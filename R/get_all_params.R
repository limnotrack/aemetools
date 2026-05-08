#' Get all parameter values from calibration results
#'
#' @param calib A list with the calibration results loaded using
#' \code{\link{read_calib}}.
#' @inheritParams plot_calib
#' @inheritParams lifecycle::deprecate_warn
#'
#' @importFrom dplyr case_when filter group_by mutate summarise
#' @importFrom stringr str_split_i
#'
#' @return A data frame with all the parameter values.
#' @export

get_all_params <- function(calib, na_value, fit_col = "fit") {
  
  if (!all(fit_col %in% calib$simulation_data$fit_type)) {
    cli::cli_abort("fit_col {.val {fit_col}} not found in 
                   {.code calib$simulation_data$fit_type}")
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
  
  return(all_pars)
}
