#' Plot calibration results
#'
#' @param calib dataframe; output from \code{\link{read_calib}}
#' @inheritParams calib_aeme
#' @param na_value numeric; value to replace NA values with
#' @param nrow integer; number of rows in plot
#' @param base_size numeric; base size for theme
#' @param return_pars logical; return parameter values
#'
#' @importFrom ggplot2 aes geom_point geom_vline geom_hline geom_smooth
#' @importFrom ggplot2 facet_wrap labs theme_bw scale_colour_brewer
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous scale_fill_brewer
#'
#' @return list of plots
#' @export

plot_calib <- function(calib, model, na_value, nrow = 2, base_size = 8,
                       return_pars = FALSE) {

  all_pars <- get_param(calib, model, na_value = na_value, best = FALSE)
  summ <- get_param(calib, model, na_value = na_value, best = TRUE)

  # f_pars <- all_pars[all_pars$fit < 0, ]
  ylims <- c(min(all_pars$fit, na.rm = TRUE),
             stats::quantile(all_pars$fit, 0.75, na.rm = TRUE))

  # Convergence plot ----
  plist <- lapply(model, \(m) {
    ggplot2::ggplot() +
      ggplot2::geom_point(data = all_pars[all_pars$model == m, ],
                          ggplot2::aes(value, fit, colour = gen,
                                       group = model)) +
      ggplot2::geom_point(data = summ[summ$model == m, ],
                          ggplot2::aes(value, fit), colour = "red") +
      ggplot2::geom_vline(data = summ[summ$model == m, ],
                          ggplot2::aes(xintercept = value)) +
      ggplot2::scale_y_log10() +
      ggplot2::scale_colour_viridis_d() +
      ggplot2::coord_cartesian(ylim = ylims) +
      ggplot2::xlab("") +
      # annotate(geom = 'text', label = 'sometext', x = -Inf, y = Inf, hjust = 0,
      #          vjust = 1) +
      ggplot2::geom_text(data = summ[summ$model == m, ],
                         ggplot2::aes(x = Inf, y = Inf,
                                      label = signif(value, 3)), vjust = 4,
                         hjust = 2, size = 3) +
      # facet_wrap(model ~ param, scales = "free_x", nrow = nrow) +
      ggplot2::facet_grid(model ~ param2, scales = "free_x") +
      ggplot2::theme_bw(base_size = base_size)
  })
  pdotty <- patchwork::wrap_plots(plist, nrow = length(model),
                                  guides = "collect")

  # Caterpillar plot ----
  plist <- lapply(model, \(m) {
    ggplot2::ggplot() +
      ggplot2::geom_hline(data = summ[summ$model == m, ],
                          ggplot2::aes(yintercept = value)) +
      ggplot2::geom_point(data = all_pars[all_pars$model == m, ],
                          ggplot2::aes(index, value, colour = gen, group = model)) +
      ggplot2::scale_colour_viridis_d() +
      ggplot2::xlab("Iteration") +
      ggplot2::facet_grid(param2 ~ model, scales = "free") +
      ggplot2::theme_bw(base_size = base_size)
  })
  pconverge <- patchwork::wrap_plots(plist, nrow = length(model),
                                   guides = "collect")

  all_pars$gen <- forcats::fct_rev(all_pars$gen)

  # Histogram ----
  plist <- lapply(model, \(m) {
    ggplot2::ggplot() +
      ggplot2::geom_histogram(data = all_pars[all_pars$model == m, ],
                              ggplot2::aes(value, fill = gen), bins = 50) +
      ggplot2::geom_vline(data = summ[summ$model == m, ],
                          ggplot2::aes(xintercept = value)) +
      ggplot2::facet_grid(model ~ param2, scales = "free") +
      ggplot2::xlab("Parameter value") +
      ggplot2::scale_fill_viridis_d(direction = -1) +
      ggplot2::theme_bw(base_size = base_size)
  })
  phist <- patchwork::wrap_plots(plist, nrow = length(model),
                                 guides = "collect")
  return(list(dotty = pdotty, histogram = phist, convergence = pconverge))
}




