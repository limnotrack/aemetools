#' Plot calibration results
#'
#' @param calib dataframe; output from \code{\link{read_calib}}
#' @param na_value A numeric value which corresponds to the NA value used in
#' the calibration.
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
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' model_controls <- AEME::get_model_controls()
#' model <- c("glm_aed", "gotm_wet")
#' path <- "aeme"
#' aeme <- AEME::build_aeme(aeme = aeme, model = model, path = path,
#'                          model_controls = model_controls, ext_elev = 5) |>
#'   AEME::run_aeme()
#' 
#' data("aeme_parameters", package = "AEME")
#' param <- aeme_parameters
#' 
#' # Function to calculate fitness
#' nse <- function(df) {
#' # Calculate Nash-Sutcliffe Efficiency
#'   nse <- 1 - (sum((df$obs - df$model)^2) / sum((df$obs - mean(df$obs))^2))
#'   -1 * nse
#' }
#' FUN_list <- list(HYD_temp = nse, LKE_lvlwtr = nse)
#' 
#' ctrl <- create_control(method = "calib", NP = 10, itermax = 20, ncore = 2,
#'                        parallel = TRUE, file_type = "db",
#'                        file_name = "results.db")
#' 
#' vars_sim <- c("HYD_temp", "LKE_lvlwtr")
#' weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 1)
#' 
#' # Calibrate AEME model
#' sim_id <- calib_aeme(aeme = aeme, model = model, path = path,
#'                      param = param, FUN_list = FUN_list, ctrl = ctrl,
#'                      vars_sim = vars_sim, weights = weights)
#'                      
#' # Read calibration output                      
#' calib <- read_calib(sim_id = sim_id, ctrl = ctrl)
#' plist <- plot_calib(calib = calib)
#' 
#' # Dotty plot
#' plist$dotty
#' 
#' # Convergence plot
#' plist$convergence
#' 
#' # Histogram plot
#' plist$histogram
#' @return list of plots
#' @export

plot_calib <- function(calib, na_value, fit_col = "fit", nrow = 2,
                       base_size = 8, return_pars = FALSE, log_y = TRUE) {
  
  nsims <- nrow(calib$simulation_metadata)
  sim_ids <- calib$simulation_metadata$sim_id
  if (missing(na_value)) {
    na_value <- calib$calibration_metadata$na_value[1]
  }
  
  all_pars <- get_param(calib, na_value = na_value, fit_col = fit_col,
                        best = FALSE) 
  all_pars_label <- all_pars |> 
    dplyr::distinct(parameter_name, name, group, label)
  summ <- get_param(calib, na_value = na_value, fit_col = fit_col, 
                    best = TRUE) |> 
    dplyr::mutate(parameter_name = encode_param(group = group, name = name, index = index)) |> 
    dplyr::left_join(all_pars_label, by = c("parameter_name"))
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
  if (fit_col != "fit") {
    data("key_naming", package = "AEME", envir = environment())
    var_name <- key_naming |> 
      dplyr::filter(name == fit_col) |>
      dplyr::pull(name_text)
  }
  ylab <- ifelse(fit_col == "fit", "Fit", paste0("Fit (", var_name, ")"))
  
  # Dotty plot ----
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
      # ggplot2::xlab("") +
      # ggplot2::ylab(ylab) +
      # annotate(geom = 'text', label = 'sometext', x = -Inf, y = Inf, hjust = 0,
      #          vjust = 1) +
      ggplot2::geom_text(data = summ[summ$sim_id == s, ],
                         ggplot2::aes(x = Inf, y = Inf,
                                      label = signif(value, 3)),
                         vjust = 4,
                         hjust = 2, size = 3) +
      ggplot2::facet_wrap( ~ label, scales = "free_x", nrow = nrow) +
      # ggplot2::facet_grid(sim_id ~ label, scales = "free_x") +
      ggplot2::theme_bw(base_size = base_size)
  })
  pdotty <- patchwork::wrap_plots(plist, nrow = nsims,
                                  guides = "collect")
  
  # Convergence plot ----
  plist <- lapply(sim_ids, \(s) {
    ggplot2::ggplot() +
      ggplot2::geom_hline(data = summ[summ$sim_id == s, ],
                          ggplot2::aes(yintercept = value)) +
      ggplot2::geom_point(data = all_pars[all_pars$sim_id == s, ],
                          ggplot2::aes(index, parameter_value, colour = gen,
                                       group = model)) +
      ggplot2::scale_colour_viridis_d() +
      ggplot2::labs(title = paste("Simulation ID:", s), y = "Parameter value", 
                    y = "Iteration", colour = "Generation") +
      # ggplot2::facet_grid(label ~ sim_id, scales = "free") +
      ggplot2::facet_wrap( ~ label, scales = "free_y", ncol = nrow) +
      ggplot2::theme_bw(base_size = base_size)
  })
  pconverge <- patchwork::wrap_plots(plist, nrow = nsims,
                                     guides = "collect")
  
  all_pars$gen <- forcats::fct_rev(all_pars$gen)
  
  # Histogram ----
  plist <- lapply(sim_ids, \(s) {
    ggplot2::ggplot() +
      ggplot2::geom_histogram(data = all_pars[all_pars$sim_id == s, ],
                              ggplot2::aes(parameter_value, fill = gen),
                              bins = 50) +
      ggplot2::geom_vline(data = summ[summ$sim_id == s, ],
                          ggplot2::aes(xintercept = value)) +
      # ggplot2::facet_grid(sim_id ~ label, scales = "free") +
      ggplot2::facet_wrap( ~ label, scales = "free_x", nrow = nrow) +
      ggplot2::labs(title = paste("Simulation ID:", s), x = "Parameter value", 
                    y = "Count", fill = "Generation") +
      ggplot2::scale_fill_viridis_d(direction = -1) +
      ggplot2::theme_bw(base_size = base_size)
  })
  phist <- patchwork::wrap_plots(plist, nrow = nsims,
                                 guides = "collect")
  return(list(dotty = pdotty, histogram = phist, convergence = pconverge))
}
