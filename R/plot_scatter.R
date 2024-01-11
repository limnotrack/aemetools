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

  mean_y <- sa$df |>
    dplyr::group_by(parameter, variable) |>
    dplyr::do(
      dplyr::tibble(
        value = seq(min(.$value), max(.$value), length.out = 30),
        mean_y = suppressWarnings(approx(.$value, .$output, xout = value)$y)
      )
    )

  ggplot2::ggplot() +
    ggplot2::geom_point(data = sa$df, ggplot2::aes(x = value, y = output)) +
    ggplot2::geom_point(data = mean_y, ggplot2::aes(x = value, y = mean_y,
                                                    colour = "Mean")) +
    ggplot2::facet_wrap(parameter ~ variable, scales = "free") +
    ggplot2::xlab("Value") +
    ggplot2::ylab("y") +
    ggplot2::theme_bw()
}
