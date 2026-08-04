#' Plot calibration histogram plot
#'
#' Plots a histogram of every simulated parameter value, filled by
#' generation, with the best value per parameter marked.
#'
#' @inheritParams plot_calib
#' @param plot_data list; optional, pre-computed via the internal
#' `prepare_calib_plot_data()` helper. When supplied, `calib`/`fit_col` are
#' not re-queried. Used by [plot_calib()] so the underlying data is only
#' prepared once for all three plot types; leave as `NULL` (the default)
#' when calling this function directly.
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline facet_wrap labs
#' @importFrom ggplot2 theme_bw scale_fill_viridis_d
#' @importFrom patchwork wrap_plots
#' @importFrom rlang `%||%`
#'
#' @examples
#' \dontrun{
#' calib <- read_calib(sim_id = sim_id, ctrl = ctrl)
#' plot_calib_histogram(calib = calib)
#' }
#' @return patchwork object with one histogram plot per simulation ID.
#' @export
plot_calib_histogram <- function(calib, fit_col = "fit", nrow = 2,
                                 base_size = 8, plot_data = NULL) {

  d <- plot_data %||% prepare_calib_plot_data(calib = calib, fit_col = fit_col)
  all_pars <- d$all_pars
  summ <- d$summ
  sim_ids <- d$sim_ids
  nsims <- d$nsims

  # Reversed so the oldest generation draws on top of the newest.
  all_pars$gen <- factor(all_pars$gen, levels = rev(levels(all_pars$gen)))

  plist <- lapply(sim_ids, \(s) {
    ggplot2::ggplot() +
      ggplot2::geom_histogram(data = all_pars[all_pars$sim_id == s, ],
                              ggplot2::aes(parameter_value, fill = gen),
                              bins = 50) +
      ggplot2::geom_vline(data = summ[summ$sim_id == s, ],
                          ggplot2::aes(xintercept = value)) +
      ggplot2::facet_wrap( ~ label, scales = "free_x", nrow = nrow) +
      ggplot2::labs(title = paste("Simulation ID:", s), x = "Parameter value",
                    y = "Count", fill = "Generation") +
      ggplot2::scale_fill_viridis_d(direction = -1) +
      ggplot2::theme_bw(base_size = base_size)
  })
  patchwork::wrap_plots(plist, nrow = nsims, guides = "collect")
}
