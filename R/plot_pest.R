#' Plot the PEST++ objective-function trajectory
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Phi against iteration, with the ensemble's min-max range shaded and the
#' mean and best member drawn on top. A trajectory that flattens says
#' further iterations are not improving the fit; one where the band stays
#' wide says the ensemble has not converged even if the best member has.
#'
#' @inheritParams read_pest_phi
#' @param log_y Logical. Log-scale the phi axis? Default `TRUE`, since phi
#'   typically falls by orders of magnitude in the first iteration and a
#'   linear axis then hides everything that follows.
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point labs
#' @importFrom ggplot2 theme_bw scale_y_log10
#'
#' @return A ggplot object.
#' @seealso [read_pest_phi()], [plot_pest_ensemble()]
#' @export
plot_pest_phi <- function(ctrl, type = "actual", log_y = TRUE) {

  ctrl <- .pest_locate(ctrl)
  phi <- read_pest_phi(ctrl, type = type)

  p <- ggplot2::ggplot(phi, ggplot2::aes(x = iteration)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = min, ymax = max),
                         fill = "steelblue", alpha = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y = mean), colour = "steelblue",
                       linewidth = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = min), colour = "firebrick",
                       linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(y = min), colour = "firebrick") +
    ggplot2::labs(x = "Iteration", y = paste0("Phi (", type, ")"),
                  title = "PEST++ objective function",
                  subtitle = "band = ensemble range, red = best, blue = mean") +
    ggplot2::theme_bw()

  # A zero or negative phi cannot be shown on a log axis; fall back rather
  # than emitting a ggplot warning and dropping the point silently.
  if (log_y && all(phi$min > 0, na.rm = TRUE)) {
    p <- p + ggplot2::scale_y_log10()
  }
  p
}

#' Plot prior against posterior parameter distributions
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' One panel per parameter, prior and posterior ensembles as violins on a
#' shared axis, with the calibration bounds marked. This is the plot that
#' shows which parameters the observations informed: a posterior much
#' narrower than the prior was constrained, one the same width was not, and
#' one pressed against a bound suggests the bound is doing the work rather
#' than the data.
#'
#' @inheritParams pest_param_summary
#' @param scaled Logical. Rescale each parameter to `0-1` across its
#'   `[min, max]` bounds and draw a single panel? Default `FALSE`. Useful
#'   when there are many parameters with wildly different magnitudes.
#'
#' @importFrom ggplot2 ggplot aes geom_violin geom_hline facet_wrap labs
#' @importFrom ggplot2 theme_bw coord_flip
#'
#' @return A ggplot object.
#' @seealso [pest_param_summary()]
#' @export
plot_pest_ensemble <- function(ctrl, param, scaled = FALSE) {

  ctrl <- .pest_locate(ctrl)
  if (!"name_full" %in% names(param)) {
    param$name_full <- encode_param(param$group, param$name, param$index)
  }

  prior <- read_pest_ensemble(ctrl, iteration = 0, type = "par")
  post <- read_pest_ensemble(ctrl, type = "par")
  df <- dplyr::bind_rows(
    dplyr::mutate(prior, ensemble = "prior"),
    dplyr::mutate(post, ensemble = "posterior")
  )
  df$ensemble <- factor(df$ensemble, levels = c("prior", "posterior"))

  bnd <- param[, c("name_full", "min", "max")]

  if (scaled) {
    df <- dplyr::left_join(df, bnd, by = "name_full") |>
      dplyr::mutate(value = ifelse((max - min) > 0,
                                   (value - min) / (max - min), NA_real_))
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = name_full, y = value,
                                       fill = ensemble)) +
        ggplot2::geom_violin(position = "dodge", alpha = 0.6,
                             scale = "width") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Value, scaled to [min, max]",
                      fill = NULL, title = "Prior vs posterior parameters") +
        ggplot2::theme_bw()
    )
  }

  ggplot2::ggplot(df, ggplot2::aes(x = ensemble, y = value,
                                   fill = ensemble)) +
    ggplot2::geom_violin(alpha = 0.6, scale = "width") +
    ggplot2::geom_hline(data = bnd, ggplot2::aes(yintercept = min),
                        linetype = "dashed", colour = "grey40") +
    ggplot2::geom_hline(data = bnd, ggplot2::aes(yintercept = max),
                        linetype = "dashed", colour = "grey40") +
    ggplot2::facet_wrap(~name_full, scales = "free_y") +
    ggplot2::labs(x = NULL, y = "Parameter value", fill = NULL,
                  title = "Prior vs posterior parameters",
                  subtitle = "dashed = calibration bounds") +
    ggplot2::theme_bw()
}

#' Plot posterior residuals
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Simulated against observed for every realisation, faceted by variable,
#' with a 1:1 line. Structure here - a tilt, a curve, or a cluster of
#' points the whole ensemble misses - is what tells you the misfit is
#' systematic rather than random, which no aggregate fit statistic will.
#'
#' @inheritParams pest_residuals
#' @param type Character. `"scatter"` (default) plots simulated against
#'   observed; `"time"` plots the residual against date, which exposes
#'   seasonal or drift structure.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_abline geom_hline
#' @importFrom ggplot2 facet_wrap labs theme_bw
#'
#' @return A ggplot object.
#' @seealso [pest_residuals()]
#' @export
plot_pest_residuals <- function(ctrl, iteration = NULL, obs_tbl = NULL,
                                type = "scatter") {

  type <- rlang::arg_match(type, c("scatter", "time"))
  ctrl <- .pest_locate(ctrl)
  res <- pest_residuals(ctrl, iteration = iteration, obs_tbl = obs_tbl)

  if (type == "time") {
    return(
      ggplot2::ggplot(res, ggplot2::aes(x = Date, y = residual)) +
        ggplot2::geom_point(alpha = 0.3, colour = "steelblue") +
        ggplot2::geom_hline(yintercept = 0, colour = "firebrick") +
        ggplot2::facet_wrap(~var_aeme, scales = "free_y") +
        ggplot2::labs(x = NULL, y = "Modelled - observed",
                      title = "Posterior residuals over time") +
        ggplot2::theme_bw()
    )
  }

  ggplot2::ggplot(res, ggplot2::aes(x = obs, y = model)) +
    ggplot2::geom_point(alpha = 0.3, colour = "steelblue") +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "firebrick") +
    ggplot2::facet_wrap(~var_aeme, scales = "free") +
    ggplot2::labs(x = "Observed", y = "Modelled",
                  title = "Posterior simulated vs observed",
                  subtitle = "one point per realisation per observation") +
    ggplot2::theme_bw()
}

#' Plot the per-group objective-function trajectory
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' One line per observation group (i.e. per `vars_sim` variable) of its phi
#' contribution against iteration. Divergent lines - one variable falling,
#' another flat or rising - say the run is trading fit in one variable for
#' another, which the total phi in \code{\link{plot_pest_phi}} cannot show.
#'
#' @inheritParams read_pest_phi
#' @param log_y Logical. Log-scale the phi axis? Default `TRUE`.
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_bw
#' @importFrom ggplot2 scale_y_log10
#'
#' @return A ggplot object.
#' @seealso [read_pest_phi_group()], [plot_pest_phi()]
#' @export
plot_pest_phi_group <- function(ctrl, log_y = TRUE) {

  ctrl <- .pest_locate(ctrl)
  g <- read_pest_phi_group(ctrl)
  g$series <- ifelse(is.na(g$var_aeme), g$obgnme, g$var_aeme)

  p <- ggplot2::ggplot(g, ggplot2::aes(x = iteration, y = phi,
                                       colour = series)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Iteration", y = "Phi (group contribution)",
                  colour = NULL, title = "PEST++ objective function by group") +
    ggplot2::theme_bw()

  if (log_y && all(g$phi > 0, na.rm = TRUE)) p <- p + ggplot2::scale_y_log10()
  p
}

#' Plot the posterior simulated ensemble against observations over time
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' For each variable, the ensemble's simulated range over time (a ribbon of
#' across-realisation quantiles with the median on top) with the observed
#' values as points. This is the plot that shows *when* the ensemble
#' reproduces the observations and when it does not - seasonal drift, a
#' missed stratification onset - which neither the aggregate fit nor a
#' simulated-vs-observed scatter reveals.
#'
#' @inheritParams pest_residuals
#' @param vars Character or `NULL`. Restrict to these `var_aeme` values.
#' @param ci Length-2 numeric. Lower/upper quantiles for the ribbon.
#'   Default `c(0.05, 0.95)`.
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point labs
#' @importFrom ggplot2 facet_wrap theme_bw
#' @importFrom stats quantile median
#'
#' @return A ggplot object.
#' @seealso [pest_residuals()], [plot_pest_residuals()]
#' @export
plot_pest_timeseries <- function(calib, obs_tbl = NULL, vars = NULL,
                                 iteration = NULL, ci = c(0.05, 0.95)) {

  if (length(ci) != 2 || any(ci < 0) || any(ci > 1) || ci[1] >= ci[2]) {
    cli::cli_abort("{.arg ci} must be two increasing probabilities in [0, 1].")
  }
  ctrl <- .pest_locate(calib)
  sim <- read_pest_ensemble(ctrl, iteration = iteration, type = "obs")
  if (!is.null(vars)) sim <- sim[sim$var_aeme %in% vars, , drop = FALSE]
  if (nrow(sim) == 0) cli::cli_abort("No simulated observations to plot.")

  obsval <- if (!is.null(obs_tbl)) {
    stats::setNames(obs_tbl$obsval, obs_tbl$obsnme)
  } else {
    .pest_read_pst_obs(ctrl)
  }

  has_depth <- !all(is.na(sim$depth))
  dfac <- function(x) if (has_depth) factor(round(x, 2)) else factor("all")

  grp <- sim |>
    dplyr::filter(!is_base) |>
    dplyr::group_by(var_aeme, Date, depth) |>
    dplyr::summarise(
      lo  = stats::quantile(model, ci[1], na.rm = TRUE),
      mid = stats::median(model, na.rm = TRUE),
      hi  = stats::quantile(model, ci[2], na.rm = TRUE),
      .groups = "drop"
    )
  grp$depth_f <- dfac(grp$depth)

  obs_df <- unique(sim[, c("obsnme", "var_aeme", "Date", "depth")])
  obs_df$obs <- if (is.null(obsval)) NA_real_ else unname(obsval[obs_df$obsnme])
  obs_df$depth_f <- dfac(obs_df$depth)

  p <- ggplot2::ggplot(grp, ggplot2::aes(x = Date)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi, fill = depth_f),
                         alpha = 0.25) +
    ggplot2::geom_line(ggplot2::aes(y = mid, colour = depth_f)) +
    ggplot2::geom_point(data = obs_df[!is.na(obs_df$obs), ],
                        ggplot2::aes(y = obs, colour = depth_f), size = 1) +
    ggplot2::facet_wrap(~var_aeme, scales = "free_y") +
    ggplot2::labs(x = NULL, y = "Value", colour = "Depth", fill = "Depth",
                  title = "Posterior ensemble vs observations",
                  subtitle = sprintf("ribbon = %g-%g quantiles, line = median",
                                     ci[1], ci[2])) +
    ggplot2::theme_bw()

  if (!has_depth) p <- p + ggplot2::guides(colour = "none", fill = "none")
  p
}
