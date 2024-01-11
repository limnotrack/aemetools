#' Plot the uncertainty of the output of a sensitivity analysis
#'
#' @param sa list; of sensitivity analysis results read in with \code{\link{read_sa}}
#' @param bins integer; number of bins for the histogram. Default is 30.
#'
#' @return \code{ggplot} object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_histogram facet_wrap ylab xlab theme_bw
#'

plot_uncertainty <- function(sa, bins = 30) {

  ggplot2::ggplot() +
    ggplot2::geom_histogram(data = sa$df, ggplot2::aes(x = output, fill = NULL),
                            colour = "black", bins = bins, fill = "grey") +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::ylab("Counts") +
    ggplot2::xlab("y") +
    ggplot2::theme_bw()

}
