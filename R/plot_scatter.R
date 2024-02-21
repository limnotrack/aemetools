#' Plot the scatter of the output of a sensitivity analysis
#'
#' @inheritParams plot_uncertainty
#' @param cutoff numeric. The maximum value of the fit to include in the plot.
#' This can be useful to remove outliers.
#'
#' @return \code{ggplot} object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point facet_wrap ylab xlab theme_bw
#' @importFrom dplyr group_by do tibble
#'

plot_scatter <- function(sa, cutoff = NA) {

  df <- lapply(names(sa), \(x) {
    sa[[x]]$df |>
      dplyr::filter(fit_type != "fit") |>
      dplyr::mutate(sim_id = x, label = abbrev_pars(parameter_name, model[1]))
  }) |>
    dplyr::bind_rows()

  # Generate summary statistics
  # summ <- df |>
  #   dplyr::group_by(sim_id, fit_type) |>
  #   dplyr::summarise(mean = mean(fit_value, na.rm = TRUE), median = median(fit_value, na.rm = TRUE),
  #                    sd = sd(fit_value, na.rm = TRUE),
  #                    .groups = "keep") |>
  #   as.data.frame()
  # summ


  # Check for parameters to remove ----
  rem_pars <- df |>
    dplyr::group_by(label, fit_type, sim_id) |>
    dplyr::summarise(na = all(is.na(fit_value)),
                     no_range = max(parameter_value, na.rm = TRUE) ==
                       min(parameter_value, na.rm = TRUE), .groups = "keep") |>
    dplyr::ungroup() |>
    dplyr::filter(na | no_range) |>
    dplyr::pull(label) |>
    unique()

  df <- df |>
    dplyr::filter(!label %in% rem_pars)

  if (!is.na(cutoff)) {
    df <- df |>
      # dplyr::group_by(label, fit_type, sim_id) |>
      dplyr::filter(fit_value <= cutoff)
      # dplyr::ungroup()
  }
  # Remove values > prob percentile for each parameter
  df <- df |>
    dplyr::group_by(label, fit_type, sim_id) |>
    dplyr::filter(fit_value <= quantile(fit_value, prob, na.rm = TRUE)) |>
    dplyr::ungroup()


  mean_y <- df |>
    dplyr::group_by(label, fit_type, sim_id) |>
    dplyr::reframe(
      value = seq(min(parameter_value, na.rm = TRUE),
                  max(parameter_value, na.rm = TRUE), length.out = 30),
      mean_y = suppressWarnings(approx(parameter_value, fit_value, xout = value)$y)
    )

  uni_pars <- df |>
    dplyr::group_by(sim_id) |>
    dplyr::summarise(n_pars = length(unique(label)), .groups = "keep")
  n_pars <- sum(uni_pars$n_pars)

  # If number of parameters greater than 20 create a list of plots
  plist <- list()
  if (n_pars > 20) {
    for (sid in names(sa)) {
      sub <- df |> dplyr::filter(sim_id == sid)
      param_list <- split(unique(sub$label),
                          ceiling(seq_along(unique(sub$label)) / 20))
      plist[[sid]] <- lapply(param_list, \(x) {
        sub2 <- sub |>  dplyr::filter(label %in% x)
        my <- mean_y |> dplyr::filter(label %in% x & sid == sim_id)

        ggplot2::ggplot() +
          ggplot2::geom_point(data = sub2, ggplot2::aes(x = parameter_value, y = fit_value)) +
          ggplot2::geom_point(data = my, ggplot2::aes(x = value, y = mean_y,
                                                          colour = "Mean")) +
          # ggplot2::facet_wrap(fit_type ~ label, scales = "free") +
          ggplot2::facet_grid(fit_type ~ label, scales = "free") +
          ggplot2::xlab("Value") +
          ggplot2::ylab("y") +
          ggplot2::theme_bw(base_size = 8)
      })
    }
    return(plist)
  } else {
    ggplot2::ggplot() +
      ggplot2::geom_point(data = df, ggplot2::aes(x = parameter_value, y = fit_value)) +
      ggplot2::geom_point(data = mean_y, ggplot2::aes(x = value, y = mean_y,
                                                      colour = "Mean")) +
      # ggplot2::facet_wrap(fit_type ~ label, scales = "free") +
      ggplot2::facet_grid(fit_type ~ sim_id * label, scales = "free") +
      ggplot2::xlab("Value") +
      ggplot2::ylab("y") +
      ggplot2::theme_bw()
  }
}
