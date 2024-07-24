#' Plot calibration results
#'
#' @param calib dataframe; output from \code{\link{read_calib}}
#' @param na_value numeric; value to replace NA values with
#' @param fit_col character; name of column containing fit values. Default is
#'  \code{"fit"}.
#' @param nrow integer; number of rows in plot
#' @param base_size numeric; base size for theme
#' @param return_pars logical; return parameter values
#' @param log_y logical; use log scale on y-axis. Default is \code{TRUE}.
#'
#' @importFrom ggplot2 aes geom_point geom_vline geom_hline geom_smooth
#' @importFrom ggplot2 facet_wrap labs theme_bw scale_colour_brewer
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous scale_fill_brewer
#' @importFrom ggplot2 scale_colour_viridis_d
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 geom_text annotate
#' @importFrom forcats fct_reorder
#' @importFrom patchwork wrap_plots
#'
#' @return list of plots
#' @export

plot_calib <- function(calib, na_value, fit_col = "fit", nrow = 2,
                       base_size = 8, return_pars = FALSE, log_y = TRUE) {

  nsims <- nrow(calib$simulation_metadata)
  sim_ids <- calib$simulation_metadata$sim_id

  all_pars <- get_param(calib, na_value = na_value, fit_col = fit_col,
                        best = FALSE)
  summ <- get_param(calib, na_value = na_value, fit_col = fit_col, best = TRUE)
  if (min(all_pars$fit2, na.rm = TRUE) <= 0 & log_y) {
    adj <- ceiling(abs(min(all_pars$fit2, na.rm = TRUE)))
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
  ylab <- ifelse(fit_col == "fit", "Fit", paste0("Fit (", fit_col, ")"))


  # Summary plot of best parameter values for multiple fits ----
  if (length(fit_col) > 1) {
    err_bars <- summ |>
      dplyr::group_by(sim_id, label) |>
      dplyr::summarise(xmin = min(parameter_value, na.rm = TRUE),
                       xmax = max(parameter_value, na.rm = TRUE),
                       ymin = min(fit2, na.rm = TRUE),
                       ymax = max(fit2, na.rm = TRUE), gen = dplyr::first(gen))
    ylab <- "Fit"
    plist <- lapply(sim_ids, \(s) {
      ggplot2::ggplot() +
        # ggplot2::geom_vline(data = summ[summ$sim_id == s, ],
        #                     ggplot2::aes(xintercept = parameter_value, colour = gen)) +
        ggplot2::geom_point(data = all_pars[all_pars$sim_id == s, ],
                            ggplot2::aes(parameter_value, fit2, colour = gen,
                                         group = sim_id, shape = fit_type),
                            alpha = 0) +
        # ggplot2::stat_ellipse(data = all_pars[all_pars$sim_id == s, ],
        #                       ggplot2::aes(parameter_value, fit2, colour = gen,
        #                                    group = fit_type)) +
        ggplot2::geom_point(data = summ[summ$sim_id == s, ],
                            ggplot2::aes(parameter_value, fit2, colour = gen,
                                         group = model, shape = fit_type)) +
        ggplot2::geom_point(data = summ[summ$sim_id == s, ],
                            ggplot2::aes(parameter_value, fit2, colour = gen,
                                         group = model, shape = fit_type)) +
        # ggplot2::stat_ellipse(data = summ[summ$sim_id == s, ], level = 0.9,
        #                       ggplot2::aes(parameter_value, fit2)) +
        ggplot2::geom_errorbar(data = err_bars[err_bars$sim_id == s, ],
                               ggplot2::aes(x = (xmin + xmax) / 2, ymin = ymin,
                                            ymax = ymax), width = 0) +
        ggplot2::geom_errorbarh(data = err_bars[err_bars$sim_id == s, ],
                                ggplot2::aes(y = (ymin + ymax) / 2, xmin = xmin,
                                             xmax = xmax), height = 0) +
        {if (log_y) ggplot2::scale_y_log10()} +
        ggplot2::scale_colour_viridis_d() +
        ggplot2::coord_cartesian(ylim = ylims) +
        ggplot2::xlab("") +
        ggplot2::ylab(ylab) +
        # annotate(geom = 'text', label = 'sometext', x = -Inf, y = Inf, hjust = 0,
        #          vjust = 1) +
        # ggplot2::geom_text(data = summ[summ$sim_id == s, ],
        #                    ggplot2::aes(x = Inf, y = Inf,
        #                                 label = signif(parameter_value, 3)),
        #                    vjust = 4,
        #                    hjust = 2, size = 3) +
        # facet_wrap(model ~ param, scales = "free_x", nrow = nrow) +
        ggplot2::facet_grid(sim_id ~ label, scales = "free_x") +
        ggplot2::theme_bw(base_size = base_size)
    })
    pdotty <- patchwork::wrap_plots(plist, nrow = nsims,
                                    guides = "collect")
    return(pdotty)

  }

  # Dotty plot ----
  plist <- lapply(sim_ids, \(s) {
    ggplot2::ggplot() +
      ggplot2::geom_point(data = all_pars[all_pars$sim_id == s, ],
                          ggplot2::aes(parameter_value, fit2, colour = gen,
                                       group = model)) +
      ggplot2::geom_point(data = summ[summ$sim_id == s, ],
                          ggplot2::aes(parameter_value, fit2),
                          colour = "red") +
      ggplot2::geom_vline(data = summ[summ$sim_id == s, ],
                          ggplot2::aes(xintercept = parameter_value)) +
      {if (log_y) ggplot2::scale_y_log10()} +
      ggplot2::scale_colour_viridis_d() +
      ggplot2::coord_cartesian(ylim = ylims) +
      ggplot2::xlab("") +
      ggplot2::ylab(ylab) +
      # annotate(geom = 'text', label = 'sometext', x = -Inf, y = Inf, hjust = 0,
      #          vjust = 1) +
      ggplot2::geom_text(data = summ[summ$sim_id == s, ],
                         ggplot2::aes(x = Inf, y = Inf,
                                      label = signif(parameter_value, 3)),
                         vjust = 4,
                         hjust = 2, size = 3) +
      # facet_wrap(model ~ param, scales = "free_x", nrow = nrow) +
      ggplot2::facet_grid(sim_id ~ label, scales = "free_x") +
      ggplot2::theme_bw(base_size = base_size)
  })
  pdotty <- patchwork::wrap_plots(plist, nrow = nsims,
                                  guides = "collect")

  # Convergence plot ----
  plist <- lapply(sim_ids, \(s) {
    ggplot2::ggplot() +
      ggplot2::geom_hline(data = summ[summ$sim_id == s, ],
                          ggplot2::aes(yintercept = parameter_value)) +
      ggplot2::geom_point(data = all_pars[all_pars$sim_id == s, ],
                          ggplot2::aes(index, parameter_value, colour = gen, group = model)) +
      ggplot2::scale_colour_viridis_d() +
      ggplot2::xlab("Iteration") +
      ggplot2::facet_grid(label ~ sim_id, scales = "free") +
      ggplot2::theme_bw(base_size = base_size)
  })
  pconverge <- patchwork::wrap_plots(plist, nrow = nsims,
                                     guides = "collect")

  all_pars$gen <- forcats::fct_rev(all_pars$gen)

  # Histogram ----
  plist <- lapply(sim_ids, \(s) {
    ggplot2::ggplot() +
      ggplot2::geom_histogram(data = all_pars[all_pars$sim_id == s, ],
                              ggplot2::aes(parameter_value, fill = gen), bins = 50) +
      ggplot2::geom_vline(data = summ[summ$sim_id == s, ],
                          ggplot2::aes(xintercept = parameter_value)) +
      ggplot2::facet_grid(sim_id ~ label, scales = "free") +
      ggplot2::xlab("Parameter value") +
      ggplot2::scale_fill_viridis_d(direction = -1) +
      ggplot2::theme_bw(base_size = base_size)
  })
  phist <- patchwork::wrap_plots(plist, nrow = nsims,
                                 guides = "collect")
  return(list(dotty = pdotty, histogram = phist, convergence = pconverge))
}
