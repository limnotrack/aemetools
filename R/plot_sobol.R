#' Visualization of first, total, second, third and fourth-order Sobol' indices
#'
#' @inheritParams plot_uncertainty
#'
#' @importFrom ggplot2 ggtitle
#'
#' @return list of \code{ggplot} objects for each variable
#' @export
#'
plot_sobol <- function(sa, use_dummy = TRUE, ...) {

  lapply(names(sa$sobol_indices), \(v) {
    dummy <- if (use_dummy) sa$sobol_dummy_indices[[v]] else NULL
    sensobol:::plot.sensobol(sa$sobol_indices[[v]],
                             dummy = dummy, ...) +
      ggplot2::ggtitle(v)
  })
}
