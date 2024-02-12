#' Plot multiscatter of the parameters against each other
#'
#' @inheritParams plot_uncertainty
#'
#' @return list of \code{ggplot} objects
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point facet_wrap labs theme_bw
#' scale_colour_gradientn theme scale_x_continuous scale_y_continuous
#' labs
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_wider
#' @importFrom utils combn

plot_multiscatter <- function(sa) {

  # pall <- lapply(names)

  pall <- lapply(names(sa), \(x) {
    all <- sa[[x]]$df |>
      dplyr::filter(fit_type != "fit")

    vars <- unique(all$fit_type)
    names(vars) <- vars

    pl <- lapply(vars, \(v) {
      df <- all |>
        dplyr::filter(fit_type == v)

      # GGally::ggpairs(df, columns = c("label", "fit_value"))

      params <- unique(df$label)
      dt <- df # data.table::data.table(df)
      out <- t(utils::combn(params, 2))
      N <- nrow(t)
      Y <- dt$fit_value
      fit <- df |>
        dplyr::select(run, fit_value) |>
        dplyr::distinct()
      wid <- tidyr::pivot_wider(df, id_cols = c(run), names_from = label,
                                values_from = parameter_value)
      da <- list()
      for (i in 1:nrow(out)) {
        cols <- out[i, ]
        dat <- data.frame(run = wid[["run"]], xvar = cols[1], yvar = cols[2],
                          x = wid[, cols[1]], y = wid[, cols[2]]) |>
          dplyr::left_join(fit, by = "run")
        names(dat) <- c("run", "x", "y", "xvar", "yvar", "fit_value")


        da[[i]] <- dat
      }

      output <- dplyr::bind_rows(da) |>
        dplyr::mutate(label = paste0(x, " * ", y))

      g <- ggplot2::ggplot(output, ggplot2::aes(xvar, yvar,
                                                color = fit_value)) +
        ggplot2::geom_point(size = 2) +
        ggplot2::scale_colour_gradientn(colours = grDevices::terrain.colors(10),
                                        name = v) +
        ggplot2::facet_wrap( ~ label, scales = "free") +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "", y = "")
      g
    })
    return(pl)
  })
  names(pall) <- names(sa)
  pall

}
