#' Prepare the parameter/fit data shared by the `plot_calib_*()` plot
#' functions.
#'
#' @inheritParams plot_calib
#'
#' @return list with `nsims`, `sim_ids`, `all_pars` (every simulated
#' parameter value, from [get_sim_params()]) and `summ` (the best parameter
#' value per simulation, from [get_best_params()], joined to parameter
#' labels).
#' @noRd
prepare_calib_plot_data <- function(calib, fit_col = "fit") {
  nsims <- nrow(calib$simulation_metadata)
  sim_ids <- calib$simulation_metadata$sim_id

  all_pars <- get_sim_params(calib = calib, fit_col = fit_col)
  all_pars_label <- all_pars |>
    dplyr::distinct(parameter_name, name, group, label)
  summ <- get_best_params(calib = calib, fit_col = fit_col) |>
    dplyr::mutate(parameter_name = encode_param(group = group, name = name,
                                                index = index)) |>
    dplyr::left_join(all_pars_label, by = c("parameter_name"))

  list(nsims = nsims, sim_ids = sim_ids, all_pars = all_pars, summ = summ)
}

#' Fit-value y-axis label for a `fit_col`.
#' @inheritParams plot_calib
#' @return string.
#' @noRd
calib_fit_ylab <- function(fit_col) {
  if (fit_col == "fit") {
    return("Fit")
  }
  var_name <- AEME::key_naming |>
    dplyr::filter(var_aeme == fit_col) |>
    dplyr::pull(name_text)
  paste0("Fit (", var_name, ")")
}
