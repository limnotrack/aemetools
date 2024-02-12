#' Visualization of first, total, second, third and fourth-order Sobol' indices
#'
#' @inheritParams plot_uncertainty
#'
#' @importFrom ggplot2 ggtitle
#'
#' @return list of \code{ggplot} objects for each variable
#' @export
#'
plot_sobol <- function(sa, order = "first", add_errorbars = TRUE,
                       use_dummy = TRUE, ...) {

  sim_ids <- names(sa)

  lst <- lapply(sim_ids, \(sid) {
    sub1 <- lapply(names(sa[[sid]]$sobol_indices), \(v) {
      sensitivity <- parameters <- original <- low.ci <- high.ci <- NULL
      data <- sa[[sid]]$sobol_indices[[v]]$results
      colNames <- colnames(data)
      dt <- data[sensitivity %in% c("Si", "Ti")] |>
        dplyr::mutate(fit_type = v, sim_id = sid)
      if (nrow(dt) == 0) {
        return(data.frame())
      } else {
        return(dt)
      }
    })

    sub1 |>
      dplyr::bind_rows()
  })

  dt <- lst |>
    dplyr::bind_rows()
  colNames <- colnames(dt)

  dummy <- lapply(sim_ids, \(sid) {
    lapply(names(sa[[sid]]$sobol_indices), \(v) {
      dummy <- if (use_dummy) {
        sa[[sid]]$sobol_dummy_indices[[v]] |>
          dplyr::mutate(fit_type = v, sim_id = sid)
      } else {
        NULL
      }
    }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows()
  if (nrow(dummy) == 0) {
    dummy <- NULL
  }
  # Plot only first-order indices
  # -----------------------------------------

  if (order == "first") {
    # dt <- data[sensitivity %in% c("Si", "Ti")]
    gg <- ggplot2::ggplot(dt, ggplot2::aes(parameters, original, fill = sensitivity)) +
      ggplot2::geom_bar(stat = "identity",
                        position = ggplot2::position_dodge(0.6),
                        color = "black") +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
      ggplot2::facet_wrap(sim_id ~ fit_type, scales = "free") +
      ggplot2::labs(x = "",
                    y = "Sobol' index") +
      ggplot2::scale_fill_discrete(name = "Sobol' indices",
                                   labels = c(expression(S[italic(i)]),
                                              expression(T[italic(i)]))) #+
    # theme_AP()

    # Check if there are confidence intervals
    # -----------------------------------------

    if (any(grepl("high.ci", colNames)) & add_errorbars) {
      gg <- gg +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = low.ci,
                                            ymax = high.ci),
                               position = ggplot2::position_dodge(0.6))
    }

    # Check if there are indices for the dummy parameter
    # -----------------------------------------

    if (!is.null(dummy)) {
      col_names <- colnames(dummy)

      if(any(grepl("high.ci", col_names)) == TRUE) {
        yint <- "high.ci"

      } else {
        yint <- "original"
      }
      gg <- gg +
        ggplot2::geom_hline(data = dummy,
                            ggplot2::aes(yintercept = .data[[yint]],
                                         color = .data[["sensitivity"]]),
                            lty = 2) +
        ggplot2::guides(linetype = "none", color = "none")
    }

  } else if (!order == "first") {

    # Define for second and third-order indices
    # -----------------------------------------

    if (order == "second") {
      dt <- data[sensitivity %in% "Sij"][low.ci > 0]

    } else if (order == "third") {
      dt <- data[sensitivity %in% "Sijl"][low.ci > 0]

    } else if (order == "fourth") {
      dt <- data[sensitivity %in% "Sijlm"][low.ci > 0]

    } else {
      stop("Order should be first, second or third")
    }
    gg <- ggplot2::ggplot(dt, ggplot2::aes(stats::reorder(parameters, original),
                                           original)) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = low.ci,
                                          ymax = high.ci)) +
      ggplot2::labs(x = "",
                    y = "Sobol' index") +
      ggplot2::geom_hline(yintercept = 0,
                          lty = 2,
                          color = "red")
  }
  return(gg)
}
