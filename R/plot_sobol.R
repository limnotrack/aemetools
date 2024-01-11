#' Visualization of first, total, second, third and fourth-order Sobol' indices
#'
#' @inheritParams plot_uncertainty
#'
#' @return list of \code{ggplot} objects
#' @export
#'
plot_sobol <- function(sa) {

  lapply(names(sa$sobol_indices), \(v) {
    sensobol:::plot.sensobol(sa$sobol_indices[[v]],
                             dummy = sa_res$sobol_dummy_indices[[v]]) +
      ggplot2::ggtitle(v)
  })
}
