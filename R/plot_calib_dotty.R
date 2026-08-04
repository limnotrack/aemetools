#' Plot calibration dotty plot
#'
#' Plots every simulated parameter value against its fit value, coloured by
#' generation, with the best value per parameter highlighted.
#'
#' @inheritParams plot_calib
#' @param plot_data list; optional, pre-computed via the internal
#' `prepare_calib_plot_data()` helper. When supplied, `calib`/`fit_col` are
#' not re-queried. Used by [plot_calib()] so the underlying data is only
#' prepared once for all three plot types; leave as `NULL` (the default)
#' when calling this function directly.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_vline geom_text
#' @importFrom ggplot2 facet_wrap labs theme_bw scale_colour_viridis_d
#' @importFrom ggplot2 coord_cartesian scale_y_log10
#' @importFrom patchwork wrap_plots
#' @importFrom dplyr filter distinct mutate left_join
#' @importFrom rlang `%||%`
#'
#' @examples
#' \dontrun{
#' calib <- read_calib(sim_id = sim_id, ctrl = ctrl)
#' plot_calib_dotty(calib = calib)
#' }
#' @return patchwork object with one dotty plot per simulation ID.
#' @export
plot_calib_dotty <- function(calib, fit_col = "fit", nrow = 2, base_size = 8,
                             log_y = TRUE, plot_data = NULL) {

  d <- plot_data %||% prepare_calib_plot_data(calib = calib, fit_col = fit_col)
  all_pars <- d$all_pars
  summ <- d$summ
  sim_ids <- d$sim_ids
  nsims <- d$nsims

  if (min(all_pars$fit2, na.rm = TRUE) <= 0 & log_y) {
    adj <- ceiling(abs(min(all_pars$fit2, na.rm = TRUE))) + 0.1
    message(strwrap(paste0("Negative fit values detected, adding ", adj,
                           " to all values to ensure log scale is possible."),
                    exdent = 2))
    all_pars$fit2 <- all_pars$fit2 + adj
    summ$fit2 <- summ$fit_value + adj
  } else {
    summ$fit2 <- summ$fit_value
  }
  ylims <- c(min(all_pars$fit2, na.rm = TRUE),
             stats::quantile(all_pars$fit2, 0.75, na.rm = TRUE))
  ylab <- calib_fit_ylab(fit_col)

  plist <- lapply(sim_ids, \(s) {
    ggplot2::ggplot() +
      ggplot2::geom_point(data = all_pars[all_pars$sim_id == s, ],
                          ggplot2::aes(parameter_value, fit2, colour = gen,
                                       group = model)) +
      ggplot2::geom_point(data = summ[summ$sim_id == s, ],
                          ggplot2::aes(value, fit2),
                          colour = "red") +
      ggplot2::geom_vline(data = summ[summ$sim_id == s, ],
                          ggplot2::aes(xintercept = value)) +
      {if (log_y) ggplot2::scale_y_log10()} +
      ggplot2::scale_colour_viridis_d() +
      ggplot2::coord_cartesian(ylim = ylims) +
      ggplot2::labs(title = paste("Simulation ID:", s), x = "Parameter value",
                    y = ylab, colour = "Generation") +
      ggplot2::geom_text(data = summ[summ$sim_id == s, ],
                         ggplot2::aes(x = Inf, y = Inf,
                                      label = signif(value, 3)),
                         vjust = 4,
                         hjust = 2, size = 3) +
      ggplot2::facet_wrap( ~ label, scales = "free_x", nrow = nrow) +
      ggplot2::theme_bw(base_size = base_size)
  })
  patchwork::wrap_plots(plist, nrow = nsims, guides = "collect")
}
