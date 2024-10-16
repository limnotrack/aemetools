#' Get simulation time for each model
#'
#' @inheritParams calib_aeme
#'
#' @return numeric vector of simulation time for each model.
#' @export
#'
#' @examples
#' tmpdir <- tempdir()
#' aeme_dir <- system.file("extdata/lake/", package = "AEME")
#' # Copy files from package into tempdir
#' file.copy(aeme_dir, tmpdir, recursive = TRUE)
#' path <- file.path(tmpdir, "lake")
#' aeme <- AEME::yaml_to_aeme(path = path, "aeme.yaml")
#' model_controls <- AEME::get_model_controls()
#' model <- c("glm_aed", "gotm_wet")
#' aeme <- AEME::build_aeme(path = path, aeme = aeme,
#' model = model, model_controls = model_controls,
#' ext_elev = 5, use_bgc = FALSE)
#' aeme <- AEME::run_aeme(aeme = aeme, model = model, path = path)
#' utils::data("aeme_parameters", package = "AEME")
#' param <- aeme_parameters
#' # Function to calculate fitness
#' fit <- function(df) {
#' mean(abs(df$obs - df$model))
#' }
#' FUN_list <- list(HYD_temp = fit, LKE_lvlwtr = fit)
#' ctrl <- create_control(method = "calib", NP = 10, itermax = 30, ncore = 2,
#' parallel = TRUE, file_type = "db", file_name = "results.db")
#' vars_sim <- c("HYD_temp", "LKE_lvlwtr")
#' weights <- c("HYD_temp" = 10, "LKE_lvlwtr" = 1)
#' sim_times <- get_simulation_time(aeme = aeme, model = model, path = path,
#' param = param, FUN_list = FUN_list, vars_sim = vars_sim, weights = weights)

get_simulation_time <- function(aeme, model, path, param, FUN_list,
                                vars_sim, weights) {

  include_wlev <- ifelse("LKE_lvlwtr" %in% vars_sim, TRUE, FALSE)
  times <- sapply(model, \(m) {
    t0 <- Sys.time()
    suppressMessages({
      run_and_fit(aeme = aeme, model = m, path = path,
                  param = param, na_value = 999, include_wlev = include_wlev,
                  FUN_list = FUN_list, vars_sim = vars_sim, weights = weights)
    })
    t1 <- Sys.time()
    difftime(t1, t0, units = "secs") |> as.numeric()
  })
  return(times)
}
