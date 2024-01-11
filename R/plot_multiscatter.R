#' Plot multiscatter of the parameters against each other
#'
#' @inheritParams plot_uncertainty
#'
#' @return list of \code{ggplot} objects
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point facet_wrap labs theme_bw
#' scale_colour_gradientn theme scale_x_continuous scale_y_continuous
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_wider
#' @importFrom scales pretty_breaks
#' @importFrom utils combn

plot_multiscatter <- function(sa) {

  vars <- unique(sa$df$variable)

  pl <- lapply(vars, \(v) {
    df <- sa$df |>
      dplyr::filter(variable == v)

    params <- unique(df$parameter)
    dt <- df # data.table::data.table(df)
    out <- t(utils::combn(params, 2))
    N <- nrow(t)
    Y <- dt$output
    wid <- tidyr::pivot_wider(df, names_from = parameter, values_from = value)
    da <- list()
    for (i in 1:nrow(out)) {
      cols <- out[i, ]
      dat <- data.frame(xvar = cols[1], yvar = cols[2],
                        x = wid[, cols[1]], y = wid[, cols[2]], output = Y)
      names(dat) <- c("x", "y", "xvar", "yvar", "output")
      da[[i]] <- dat
    }
    output <- do.call(rbind, da)

    gg <- ggplot2::ggplot(output, ggplot2::aes(xvar, yvar, color = output)) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_colour_gradientn(colours = grDevices::terrain.colors(10),
                                      name = v) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
      ggplot2::facet_wrap(x ~ y, scales = "free") + ggplot2::theme_bw() +
      ggplot2::labs(x = "", y = "") +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     legend.background = ggplot2::element_rect(fill = "transparent",
                                                               color = NA),
                     legend.key = ggplot2::element_rect(fill = "transparent",
                                                        color = NA),
                     # legend.key.size = ggplot2::unit(0.5, "cm"),
                     strip.background = ggplot2::element_rect(fill = "white"),
                     legend.position = "top")
    gg
  })
  return(pl)
}
