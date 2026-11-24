#' Plot `pestpp-sen` Method of Morris sensitivity indices
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A mu-star vs sigma scatter, one panel per response variable. Mu-star (the
#' mean absolute elementary effect) ranks a parameter's overall influence;
#' sigma (the spread of its elementary effects) flags a parameter whose
#' effect is nonlinear or interacts with others. A parameter low on both
#' axes is a candidate to fix.
#'
#' @param sen The object returned by \code{\link{read_sen}}, or a
#'   `sensitivity_indices` dataframe (as stored by \code{\link{sa_aeme}} or
#'   returned by \code{\link{read_pest_sen_indices}}).
#' @param sim_id Optional character vector; restrict the plot to these
#'   simulation ids. Defaults to all present.
#' @param label Logical; label each point with the parameter name. Default
#'   `TRUE`.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_text facet_wrap labs
#' @importFrom ggplot2 geom_abline
#'
#' @return A `ggplot` object.
#' @seealso \code{\link{read_sen}}, \code{\link{plot_sobol}}
#' @export
plot_sen <- function(sen, sim_id = NULL, label = TRUE) {

  df <- if (is.data.frame(sen)) {
    sen
  } else {
    do.call(rbind, lapply(sen, function(x) x$indices))
  }
  if (is.null(df) || nrow(df) == 0) {
    cli::cli_abort("No sensitivity indices to plot.")
  }
  if (!is.null(sim_id) && "sim_id" %in% names(df)) {
    df <- df[df$sim_id %in% sim_id, , drop = FALSE]
  }
  if (!all(c("mu_star", "sigma") %in% df$index_type)) {
    cli::cli_abort(c(
      "Expected {.val mu_star} and {.val sigma} rows (Method of Morris).",
      "i" = "Found {.val {unique(df$index_type)}}."
    ))
  }

  idvars <- c(if ("sim_id" %in% names(df)) "sim_id", "variable", "label")
  ms <- df[df$index_type == "mu_star", , drop = FALSE]
  sg <- df[df$index_type == "sigma", , drop = FALSE]
  key <- function(z) do.call(paste, c(z[idvars], sep = "\r"))

  wide <- ms[, idvars, drop = FALSE]
  wide$mu_star <- ms$value
  wide$sigma <- sg$value[match(key(wide), key(sg))]

  gg <- ggplot2::ggplot(wide, ggplot2::aes(x = .data[["mu_star"]],
                                           y = .data[["sigma"]])) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2,
                         colour = "grey60") +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ variable, scales = "free") +
    ggplot2::labs(x = expression(mu * "*"), y = expression(sigma))

  if (label) {
    gg <- gg + ggplot2::geom_text(ggplot2::aes(label = .data[["label"]]),
                                  vjust = -0.6, size = 3)
  }
  gg
}
