plot_calib_runs <- function(calib) {
  meta <- calib$simulation_metadata |> 
    dplyr::select(sim_id, model)
  lake_meta <- calib$lake_metadata |> 
    dplyr::left_join(calib$simulation_metadata, by = "id") |> 
    dplyr::select(sim_id, name, depth) |> 
    dplyr::arrange(depth) 
  
  label_ord <- lake_meta |> 
    dplyr::distinct(name, depth) |> 
    dplyr::mutate(lake_label = factor(paste0(name, " (", depth, " m)"), 
                                      levels = paste0(name, " (", depth, " m)"))) |> 
    dplyr::select(-depth)
  lake_meta <- lake_meta |> 
    dplyr::left_join(label_ord, by = "name")
  param_meta <- calib$parameter_metadata |> 
    dplyr::group_by(model) |> 
    dplyr::mutate(parameter_name = encode_param(group, name, index),
                  label = abbrev_pars(par = parameter_name, model = model)
                  ) |> 
    dplyr::ungroup() |> 
    dplyr::select(sim_id, parameter_name, label)
  
  param_summ <- calib$simulation_data |> 
    dplyr::left_join(param_meta, by = c("sim_id", "parameter_name")) |> 
    dplyr::group_by(sim_id, label, gen) |> 
    dplyr::summarise(
      median = median(parameter_value),
      lower = quantile(parameter_value, 0.05),
      upper = quantile(parameter_value, 0.95),
      .groups = "drop"
    ) |> 
    dplyr::left_join(lake_meta, by = "sim_id") |> 
    dplyr::left_join(meta, by = "sim_id") |> 
    dplyr::mutate(label = paste0(AEME::toggle_models(model), " | ", label))
  
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = param_summ, 
                         ggplot2::aes(x = gen, ymin = lower, ymax = upper, 
                                      group = sim_id, 
                                      fill = as.factor(lake_label)), 
                alpha = 0.2) +
    ggplot2::geom_line(data = param_summ, 
                       ggplot2::aes(x = gen, y = median, group = sim_id,
                                    color = as.factor(lake_label)), size = 1) +
    ggplot2::facet_wrap(~ label, scales = "free_y") +
    ggplot2::labs(x = "Generation", y = "Parameter Value", 
                  color = "Lake") +
    ggplot2::theme_minimal(base_size = 8)
}
