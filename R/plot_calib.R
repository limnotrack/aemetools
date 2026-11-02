#' Plot calibration results
#'
#' @description
#' Convenience wrapper that runs [plot_calib_dotty()], [plot_calib_histogram()]
#' and [plot_calib_convergence()] and returns all three. Call the individual
#' `plot_calib_*()` functions directly if you only need one of them.
#'
#' @inheritParams get_param
#' @param fit_col character; name of column containing fit values. Default is
#'  \code{"fit"}.
#' @param nrow integer; number of rows in plot
#' @param base_size numeric; base size for theme
#' @param return_pars logical; return parameter values
#' @param log_y logical; use log scale on y-axis. Default is \code{TRUE}.
#'
#' @importFrom lifecycle deprecated deprecate_warn is_present
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
#' # Function to calculate fitness (nse_loss = -1 * NSE, minimised by calib_aeme)
#' FUN_list <- list(HYD_temp = nse_loss, LKE_lvlwtr = nse_loss)
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

plot_calib <- function(calib, fit_col = "fit", nrow = 2, base_size = 8,
                       return_pars = FALSE, log_y = TRUE,
                       na_value = deprecated()) {

  if (lifecycle::is_present(na_value)) {
    lifecycle::deprecate_warn("0.2.0", "plot_calib(na_value)",
                             details = "NA values are now resolved automatically.")
  }

  # Prepared once and reused across all three plots, rather than each
  # plot_calib_*() function re-deriving it from `calib`.
  plot_data <- prepare_calib_plot_data(calib = calib, fit_col = fit_col)

  list(
    dotty = plot_calib_dotty(calib = calib, fit_col = fit_col, nrow = nrow,
                             base_size = base_size, log_y = log_y,
                             plot_data = plot_data),
    histogram = plot_calib_histogram(calib = calib, fit_col = fit_col,
                                     nrow = nrow, base_size = base_size,
                                     plot_data = plot_data),
    convergence = plot_calib_convergence(calib = calib, fit_col = fit_col,
                                         nrow = nrow, base_size = base_size,
                                         plot_data = plot_data)
  )
}
