#' Plot Pareto front evolution across generations
#' 
#' This function visualizes the evolution of the Pareto front across generations
#'  in a multi-objective optimization context. It creates pairwise scatter plots
#'  of the objective values, highlighting the points on the Pareto front for 
#'  each generation.
#'  
#' @inheritParams plot_calib
#' @param generations Optional vector of generations to include in the plot. If 
#' NULL, all generations are included.
#' 
#' @importFrom ggplot2 ggplot geom_point geom_path scale_y_log10 scale_x_log10
#' @importFrom ggplot2 scale_colour_viridis_d labs theme_bw
#' @importFrom tidyr pivot_wider drop_na
#' @returns A ggplot object showing the evolution of the Pareto front across
#' generations. Each point represents a run in a generation, with points on the
#' Pareto front highlighted and colored by generation. The best parameter set 
#' (with the lowest 'fit' value) is highlighted in red. The axes are on a log 
#' scale to better visualize the distribution of objective values.
#' 
#' @export
#' 

plot_pareto_generations <- function(
    calib,
    generations = NULL
) {
  
  df <- calib$simulation_data
  stopifnot(all(c("sim_id", "gen", "run", "fit_type", "fit_value") %in% names(df)))
  objectives <- unique(df$fit_type)

  
  # Optional generation filter
  if (!is.null(generations)) {
    df <- df |> dplyr::filter(gen %in% generations)
  }
  
  # Keep only objective rows
  df_obj <- df |>
    dplyr::filter(
      fit_type %in% objectives,
      # fit_type != exclude_fit_type,
      fit_value != calib$calibration_metadata$na_value
    ) |>
  dplyr::mutate(gen = factor(gen))
  
  min_val <- min(df_obj$fit_value, na.rm = TRUE)
  if (min_val < 0) {
    adj <- abs(min_val) + 1
    df_obj <- df_obj |> dplyr::mutate(fit_value = fit_value + adj)
  }
  
  # best_pars <- df_obj |>
  #   dplyr::filter(fit_type == "fit") |>
  #   dplyr::group_by(sim_id, gen) |>
  
  # Wide format: one row = one run in one generation
  wide <- df_obj |>
    dplyr::group_by(sim_id, gen, run, fit_type) |>
    dplyr::summarise(fit_value = mean(fit_value), .groups = "drop") |>
    dplyr::select(sim_id, gen, run, fit_type, fit_value) |>
    tidyr::pivot_wider(
      names_from  = fit_type,
      values_from = fit_value
    ) |>
    tidyr::drop_na()
  
  # Compute Pareto front per generation
  obj_no_fit <- setdiff(objectives, "fit")
  pareto_flagged <- wide |>
    dplyr::group_by(gen) |>
    get_pareto_front(obj_cols = objectives) |>
    dplyr::ungroup()
  
  best_pars <- wide |> 
    # dplyr::group_by(gen) |>
    dplyr::filter(fit == min(fit)) |> 
    dplyr::ungroup() |> 
    dplyr::distinct(fit, .keep_all = TRUE) 
  
  # Pairwise plots for all objective combinations
  combs <- combn(obj_no_fit, 2, simplify = FALSE)
  
  plots <- lapply(combs, function(vars) {
    x <- vars[1]
    y <- vars[2]
    
    ggplot2::ggplot() +
      # geom_hline(yintercept = -1) +
      # geom_vline(xintercept = -1) +
      # ggplot2::geom_point(data = wide,
      #                     ggplot2::aes(x = .data[[x]], y = .data[[y]]),
      #            alpha = alpha_all, colour = "grey50") +
      ggplot2::geom_point(
        data = pareto_flagged, # |> dplyr::filter(pareto),
        ggplot2::aes(x = .data[[x]], y = .data[[y]], colour = gen),
        size = 2
      ) +
      ggplot2::geom_point(
        data = best_pars,
        ggplot2::aes(x = .data[[x]], y = .data[[y]]), colour = "red",
        size = 4, shape = 19
      ) +
      # ggplot2::geom_path(
      #   data = pareto_flagged, # |> dplyr::filter(pareto) |> dplyr::arrange(gen),
      #   ggplot2::aes(x = .data[[x]], y = .data[[y]], group = gen, colour = gen)
      # ) +
      ggplot2::scale_y_log10() +
      ggplot2::scale_x_log10() +
      ggplot2::scale_colour_viridis_d(option = "plasma") +
      ggplot2::labs(
        # title = paste("Pareto front evolution:", x, "vs", y),
        colour = "Generation"
      ) +
      ggplot2::theme_bw()
  })
  
  p <- patchwork::wrap_plots(plots, ncol = 2, guides = "collect") + patchwork::plot_annotation(
    title = "Pareto front evolution across generations",
    subtitle = paste("Objectives:", paste(obj_no_fit, collapse = ", "))
  )
  
  return(p)
}
