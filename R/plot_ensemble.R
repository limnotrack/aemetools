#' Plot AEME ensemble output
#'
#' @inheritParams AEME::plot_output
#' @inheritParams AEME::get_var
#' @param conf_int numeric; confidence interval to plot when `type = "ribbon"`.
#'  Default is 0.95.
#' @param type character; type of plot to create. Can be `"ribbon"`, where it
#' plots a geom_ribbon to represent the confidence intervals specified in
#' `conf_int` or `"line`, where it plots all the ensemble members as lines.
#'  Default is "ribbon".
#'
#' @return ggplot object
#' @export
#'
#' @importFrom ggplot2 ggplot geom_line geom_ribbon scale_x_date scale_y_continuous
#' @importFrom dplyr filter mutate bind_rows group_by summarise
#' @importFrom withr local_locale local_timezone
#' @importFrom AEME observations input time
#'

plot_ensemble <- function(aeme, model, var_sim = "HYD_temp", depth = NULL,
                          conf_int = 0.95, type = "ribbon",
                          remove_spin_up = TRUE, add_obs = TRUE,
                          var_lims = NULL) {

  # Set timezone temporarily to UTC
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")

  # Check if aeme is a aeme class
  if (!inherits(aeme, "aeme")) stop("aeme must be an aeme class")

  # Check if model is a character vector
  if (!is.character(model)) stop("model must be a character vector")

  # Check if model length is 0
  if (length(model) == 0) stop("model must be a character vector of length >0")

  # Check if var_sim is a character vector
  if (!is.character(var_sim)) stop("var_sim must be a character vector")

  # Load data from aeme
  obs <- AEME::observations(aeme)
  inp <- AEME::input(aeme)
  tme <- AEME::time(aeme)

  if (!is.null(obs$level)) {
    obs_level <- obs$level
  }

  outp <- AEME::output(aeme)

  ens_lab <- paste0("ens_", sprintf("%03d", 1))

  # Check if var_sim is in output
  chk <- sapply(model, \(m){
    var_sim %in% names(outp[[ens_lab]][[m]])
  })
  if (any(!chk)) {
    if (all(!chk)) {
      stop(paste0("Variable '", var_sim, "' not in output for model(s) ",
                  paste0(model, collapse = ", ")))
    }
    warning(paste0("Variable '", var_sim, "' not in output for model(s) ",
                   paste0(model[!chk], collapse = ", ")))
    model <- model[chk]
  }

  # Date lims
  # Find date range and have output in Date format
  this.list <- sapply(model, \(m){
    "[["(outp[[ens_lab]][[m]], "Date")
  })
  xlim <- as.Date(range(this.list, na.rm = TRUE))
  if (remove_spin_up) {
    xlim <- c(as.Date(tme$start), as.Date(tme$stop))
  }

  # Filter observations by variable and Date
  if (!is.null(obs$lake)) {
    obs_lake <- obs$lake |>
      dplyr::filter(var_aeme == var_sim & Date >= xlim[1] & Date <= xlim[2])
  } else {
    obs_lake <- NULL
  }

  # colour lims
  if (is.null(var_lims)) {
    this.list <- sapply(1:outp$n_members, \(i) {
      ens_lab <- paste0("ens_", sprintf("%03d", i))
      sapply(model, \(m){
        "[["(outp[[ens_lab]][[m]], var_sim)
      })
    })
    vect <- unlist(this.list)
    if (add_obs) {
      var_lims <- range(c(vect, obs_lake[["value"]]), na.rm = TRUE)
    } else {
      var_lims <- range(vect, na.rm = TRUE)
    }
  }

  df <- lapply(1:outp$n_members, \(i) {
    ens_lab <- paste0("ens_", sprintf("%03d", i))
    AEME::get_var(aeme = aeme, model = model, var_sim = var_sim, ens_n = i,
                   return_df = TRUE, remove_spin_up = remove_spin_up,
                   cumulative = FALSE, depth = depth) |>
      dplyr::mutate(ens = i)
  }) |>
    dplyr::bind_rows()

  # Align observations to modelled depths because observations are relative to
  # the lake surface while modelled depths are relative to the lake bottom
  obs <- AEME::align_depth_data(aeme = aeme, model = model, ens_n = 1,
                                var_sim = var_sim)

  # Extract y labels
  utils::data("key_naming", package = "AEME", envir = environment())
  var_df <- key_naming |>
    dplyr::filter(name == var_sim)

  y_lab <- eval(parse(text = var_df$name_parse[1]))

  if (!is.null(depth)) {
    obs_lake <- obs$lake |>
      dplyr::mutate(depth_mid = (depth_from + depth_to) / 2) |>
      dplyr::filter(depth_mid >= (depth - 0.5) &
                      depth_mid <= (depth + 0.5) & var_aeme == var_sim &
                      Date >= xlim[1] & Date <= xlim[2])
  }

  if (type == "ribbon") {
    df2 <- df |>
      dplyr::group_by(Date, Model, var_sim) |>
      dplyr::summarise(mn = mean(value, na.rm = TRUE),
                       med = median(value, na.rm = TRUE),
                       lower = quantile(value, (1 - conf_int) / 2,
                                        na.rm = TRUE),
                       upper = quantile(value, 1 - (1 - conf_int) / 2,
                                        na.rm = TRUE),
                       n = dplyr::n(),
                       .groups = "drop")

    p <- ggplot2::ggplot(df2) +
      ggplot2::geom_ribbon(ggplot2::aes(x = Date, ymin = lower, ymax = upper,
                                        fill = Model), alpha = 0.2) +
      ggplot2::geom_line(ggplot2::aes(x = Date, y = med, color = Model)) +
      ggplot2::ylab(y_lab) +
      ggplot2::theme_bw()
  } else if (type == "line") {
    p <- ggplot2::ggplot(df) +
      ggplot2::geom_line(ggplot2::aes(x = Date, y = value, color = Model,
                                      group = ens), alpha = 0.8) +
      ggplot2::ylab(y_lab) +
      ggplot2::theme_bw()
  }

  if (add_obs) {
    p <- p +
      ggplot2::geom_point(data = obs_lake, ggplot2::aes(x = Date, y = value,
                                                      fill = "Obs")) +
      ggplot2::labs(fill = "")
  }

  return(p)
}
