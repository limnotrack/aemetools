#' Plot calibration results
#'
#' @param calib dataframe; output from \code{\link{read_calib}}
#' @param fit_col character; name of column containing fit values. If missing,
#'  all fit types in \code{calib$simulation_data$fit_type} are used.
#' @param nrow integer; number of rows in plot
#' @param base_size numeric; base size for theme
#' @param log_y logical; use log scale on y-axis. Default is \code{TRUE}.
#'
#' @importFrom ggplot2 aes geom_point geom_vline geom_hline geom_smooth
#' @importFrom ggplot2 facet_wrap labs theme_bw scale_colour_brewer
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous scale_fill_brewer
#' @importFrom ggplot2 scale_colour_viridis_d
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 geom_text annotate
#' @importFrom patchwork wrap_plots
#'
#' @return list of plots
#' @export

plot_calib_summary <- function(calib, fit_col, nrow = 2, base_size = 8,
                               log_y = TRUE) {
  
  nsims <- nrow(calib$simulation_metadata)
  sim_ids <- calib$simulation_metadata$sim_id
  na_value <- unique(calib$calibration_metadata$na_value)
  if (missing(fit_col)) {
    fit_col <- calib$simulation_data |> 
      dplyr::pull(fit_type) |>
      unique()
  }
  
  all_pars <- get_param(calib, na_value = na_value, fit_col = fit_col,
                        best = FALSE)
  summ <- get_param(calib, na_value = na_value, fit_col = fit_col, 
                    best = TRUE) |> 
    dplyr::mutate(
      enc_name = encode_param(group, name, index),
      label = abbrev_pars(enc_name, model))
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
  # Summary plot of best parameter values for multiple fits ----
  err_bars <- summ |>
    dplyr::group_by(sim_id, label) |>
    dplyr::summarise(xmin = min(value, na.rm = TRUE),
                     xmax = max(value, na.rm = TRUE),
                     ymin = min(fit2, na.rm = TRUE),
                     ymax = max(fit2, na.rm = TRUE),
                     gen = dplyr::first(gen), 
                     .groups = "drop") |> 
    dplyr::mutate(xmid = (xmin + xmax) / 2,
                  ymid = (ymin + ymax) / 2)
  
  ylims <- c(min(all_pars$fit2, na.rm = TRUE),
             max(c(quantile(all_pars$fit2, 0.75, na.rm = TRUE), 
                   err_bars$ymax)))
  # ylab <- ifelse(fit_col == "fit", "Fit", paste0("Fit (", fit_col, ")"))
  
  
  ylab <- "Fit"
  plist <- lapply(sim_ids, \(s) {
    sub_all_pars <- all_pars |> 
      dplyr::filter(sim_id == s, !is.na(fit2))
    sub_summ <- summ |>
      dplyr::filter(sim_id == s)
    sub_err <- err_bars |>
      dplyr::filter(sim_id == s)
    ggplot2::ggplot() +
      facet_wrap( ~ label, scales = "free_x", nrow = nrow) +
      ggplot2::geom_point(data = sub_all_pars,
                          ggplot2::aes(parameter_value, fit2, colour = fit_type,
                                       group = sim_id),
                          alpha = 0) +
      ggplot2::geom_point(data = sub_summ,
                          ggplot2::aes(value, fit2, colour = fit_type,
                                       group = model)) +
      ggplot2::geom_point(data = sub_summ,
                          ggplot2::aes(value, fit2, colour = fit_type,
                                       group = model)) +
      ggplot2::geom_errorbar(data = sub_err,
                             ggplot2::aes(x = xmid, ymin = ymin,
                                          ymax = ymax), width = 0) +
      ggplot2::geom_errorbar(data = sub_err,
                              ggplot2::aes(y = ymid, xmin = xmin,
                                           xmax = xmax), width = 0) +
      {if (log_y) ggplot2::scale_y_log10()} +
      ggplot2::coord_cartesian(ylim = ylims) +
      ggplot2::xlab("") +
      ggplot2::ylab(ylab) +
      ggplot2::theme_bw(base_size = base_size)
  })
  psum <- patchwork::wrap_plots(plist, nrow = nsims,
                                guides = "collect")
  return(psum)
  
}
