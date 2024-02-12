#' Plot the scatter of the output of a sensitivity analysis
#'
#' @inheritParams plot_uncertainty
#'
#' @return \code{ggplot} object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point facet_wrap ylab xlab theme_bw
#' @importFrom dplyr group_by do tibble
#'

plot_scatter <- function(sa) {

  df <- lapply(names(sa), \(x) {
    sa[[x]]$df |>
      dplyr::filter(fit_type != "fit") |>
      dplyr::mutate(sim_id = x, label = abbrev_pars(parameter_name, model[1]))
  }) |>
    dplyr::bind_rows()

  mean_y <- df |>
    dplyr::group_by(label, fit_type, sim_id) |>
    dplyr::do(
      dplyr::tibble(
        value = seq(min(.$parameter_value), max(.$parameter_value), length.out = 30),
        mean_y = suppressWarnings(approx(.$parameter_value, .$fit_value, xout = value)$y)
      )
    )

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
