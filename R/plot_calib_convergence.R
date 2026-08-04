#' Plot calibration convergence plot
#'
#' Plots each simulated parameter value against simulation iteration,
#' coloured by generation, with the best value per parameter marked.
#'
#' @inheritParams plot_calib
#' @param plot_data list; optional, pre-computed via the internal
#' `prepare_calib_plot_data()` helper. When supplied, `calib`/`fit_col` are
#' not re-queried. Used by [plot_calib()] so the underlying data is only
#' prepared once for all three plot types; leave as `NULL` (the default)
#' when calling this function directly.
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_point facet_wrap labs
#' @importFrom ggplot2 theme_bw scale_colour_viridis_d
#' @importFrom patchwork wrap_plots
#' @importFrom rlang `%||%`
#'
#' @examples
#' \dontrun{
#' calib <- read_calib(sim_id = sim_id, ctrl = ctrl)
#' plot_calib_convergence(calib = calib)
#' }
#' @return patchwork object with one convergence plot per simulation ID.
#' @export
plot_calib_convergence <- function(calib, fit_col = "fit", nrow = 2,
                                   base_size = 8, plot_data = NULL) {

  d <- plot_data %||% prepare_calib_plot_data(calib = calib, fit_col = fit_col)
  all_pars <- d$all_pars
  summ <- d$summ
  sim_ids <- d$sim_ids
  nsims <- d$nsims

  plist <- lapply(sim_ids, \(s) {
    ggplot2::ggplot() +
      ggplot2::geom_hline(data = summ[summ$sim_id == s, ],
                          ggplot2::aes(yintercept = value)) +
      ggplot2::geom_point(data = all_pars[all_pars$sim_id == s, ],
                          ggplot2::aes(index, parameter_value, colour = gen,
                                       group = model)) +
      ggplot2::scale_colour_viridis_d() +
      ggplot2::labs(title = paste("Simulation ID:", s), x = "Iteration",
                    y = "Parameter value", colour = "Generation") +
      ggplot2::facet_wrap( ~ label, scales = "free_y", ncol = nrow) +
      ggplot2::theme_bw(base_size = base_size)
  })
  patchwork::wrap_plots(plist, nrow = nsims, guides = "collect")
}
