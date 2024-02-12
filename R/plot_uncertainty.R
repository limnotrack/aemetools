#' Plot the uncertainty of the output of a sensitivity analysis
#'
#' @param sa list; of sensitivity analysis results read in with \code{\link{read_sa}}
#' @param bins integer; number of bins for the histogram. Default is 30.
#'
#' @return \code{ggplot} object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_histogram facet_wrap ylab xlab theme_bw
#' @importFrom dplyr filter
#'

plot_uncertainty <- function(sa, bins = 30) {

  df <- lapply(names(sa), \(x) {
    sa[[x]]$df |>
      dplyr::filter(parameter_name == parameter_name[1] & fit_type != "fit") |>
      dplyr::mutate(sim_id = x)
  }) |>
    dplyr::bind_rows()
  # df <- sa$df |>
  #   dplyr::filter( parameter == sa$df$parameter[1])

  ggplot2::ggplot() +
    ggplot2::geom_histogram(data = df, ggplot2::aes(x = fit_value, fill = NULL),
                            colour = "black", bins = bins, fill = "grey") +
    ggplot2::facet_wrap(sim_id ~ fit_type, scales = "free") +
    ggplot2::ylab("Counts") +
    ggplot2::xlab("y") +
    ggplot2::theme_bw()
}
