#' Plot the uncertainty of the output of a sensitivity analysis
#'
#' @param sa list; of sensitivity analysis results read in with \code{\link{read_sa}}
#' @param bins integer; number of bins for the histogram. Default is 30.
#' @inheritParams base::mean
#'
#' @return \code{ggplot} object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_histogram facet_wrap ylab xlab theme_bw
#' @importFrom dplyr filter
#'

plot_uncertainty <- function(sa, na.rm = TRUE, bins = 30) {

  df <- lapply(names(sa), \(x) {
    dat <- sa[[x]]$df |>
      dplyr::filter(parameter_name == parameter_name[1] & fit_type != "fit") |>
      dplyr::mutate(sim_id = x)
    if (na.rm) {
      n_nas <- sum(is.na(dat$fit_value) | dat$fit_value >= 1e4)
      message(paste0("Dropped ", n_nas, " NA's from ", nrow(dat),
                     " rows for sim_id ", x))
      dat |>
        dplyr::filter(!is.na(fit_value) & fit_value < 1e4)
    } else {
      dat
    }
  }) |>
    dplyr::bind_rows()
  # df <- sa$df |>
  #   dplyr::filter( parameter == sa$df$parameter[1])

  ggplot2::ggplot() +
    ggplot2::geom_histogram(data = df, ggplot2::aes(x = fit_value, fill = NULL),
                            colour = "black", bins = bins, fill = "grey") +
    ggplot2::facet_wrap(sim_id ~ fit_type, scales = "free") +
    ggplot2::ylab("Counts") +
    ggplot2::xlab("value") +
    ggplot2::theme_bw()
}
