#' Create control list for calibration or sensitivity analysis
#'
#' @param method The method to be used. It can be either "calib" for calibration
#' or "sa" for sensitivity analysis.
#' @param ... Additional arguments passed to `create_control()`. 
#' 
#' **Common arguments (both methods):**
#' 
#' * `file_type` Character. Output type: `"csv"` or `"db"`. Default `"db"`.
#' * `file_name` Character. Output file name. Defaults to
#'   `"results.db"` (db) or `"simulation_metadata.csv"` (csv).
#' * `file_dir` Character. Output directory. Defaults to `"calib_sa"`
#'   in the working directory (created if needed).
#' * `na_value` Numeric. Replacement for `NA` in observations. Default `999`.
#' * `parallel` Logical. Run in parallel? Default `TRUE`.
#' * `ncore` Integer. Number of cores (used if `parallel = TRUE`).
#'   Default `parallel::detectCores() - 1`.
#' * `timeout` Numeric. Maximum runtime in seconds. Default `Inf`.
#' 
#' **Calibration-only arguments (`method = "calib"`):**
#' 
#' * `VTR` Numeric. Target objective value. Default `-Inf`.
#' * `NP` Integer. Population size. Default `NA` (internally reset).
#' * `itermax` Integer. Maximum iterations. Default `200`.
#' * `reltol` Numeric. Relative convergence tolerance. Default `0.07`.
#' * `cutoff` Numeric. Quantile cutoff for parent selection (0–1).
#' * `mutate` Numeric. Fraction of population to mutate (0–1).
#' * `c_method` Character. Calibration method: `"CMAES"` or `"LHC"`.
#'   Default `"CMAES"`.
#' 
#' **Sensitivity-analysis-only arguments (`method = "sa"`):**
#' 
#' * `N` Integer. Base sample size.
#' * `vars_sim` Named list describing output variables. Each element must contain:
#'   - `var` Character. Variable name.
#'   - `month` Integer vector. Months to include.
#'   - `depth_range` Numeric vector (length 2). Min/max depth.
#'
#' @return list with the control parameters
#' @export
#'
#' @examples
#' calib_ctrl <- create_control("calib", VTR = -Inf, NP = 10, itermax = 30,
#'                               reltol = 0.07, cutoff = 0.25, mutate = 0.1,
#'                               parallel = TRUE, file_type = "db",
#'                               file_name = "results.db", na_value = 999,
#'                               ncore = 3)
#'
#' sa_ctrl <- create_control(method = "sa", N = 2^2, ncore = 2L, na_value = 999,
#'                           parallel = TRUE, file_type = "db",
#'                           file_name = "results.db",
#'                           vars_sim = list(surf_temp = list(var = "HYD_temp",
#'                                                            month = c(10:12, 1:3),
#'                                                            depth_range = c(0, 2)
#'                                                            ),
#'                                           bot_temp = list(var = "HYD_temp",
#'                                                           month = c(10:12, 1:3),
#'                                                           depth_range = c(10, 13)
#'                                                           )
#'                                           )
#'                         )
#'
#'

create_control <- function(method, ...) {
  ls <- list(...)

  check_names <- c("na_value", "file_type", "file_name", "file_dir", "parallel",
                   "ncore", "VTR", "NP", "itermax", "reltol", "cutoff",
                   "mutate", "N", "vars_sim", "c_method", "timeout")

  if (any(!names(ls) %in% check_names)) {
    stop(strwrap("Invalid argument(s) passed to create_control. Please check
                 which arguments to include with `?create_control()`."))
  }



  na_value <- ifelse("na_value" %in% names(ls), ls$na_value, 999)
  file_type <- ifelse("file_type" %in% names(ls), ls$file_type, "db")
  if (file_type == "db") {
    file_name <- ifelse("file_name" %in% names(ls), ls$file_name, "results.db")
  } else if (file_type == "csv") {
    file_name <- "simulation_metadata.csv"
  }
  file_dir <- ifelse("file_dir" %in% names(ls), ls$file_dir, "calib_sa")
  parallel <- ifelse("parallel" %in% names(ls), ls$parallel, TRUE)
  ncore <- ifelse("ncore" %in% names(ls), ls$ncore,
                  (parallel::detectCores() - 1))
  timeout <- ifelse("timeout" %in% names(ls), ls$timeout, Inf)


  if (method == "calib") {

    VTR <- ifelse("VTR" %in% names(ls), ls$VTR, -Inf)
    NP <- ifelse("NP" %in% names(ls), ls$NP, 10)
    itermax <- ifelse("itermax" %in% names(ls), ls$itermax, 30)
    reltol <- ifelse("reltol" %in% names(ls), ls$reltol, 0.07)
    cutoff <- ifelse("cutoff" %in% names(ls), ls$cutoff, 0.25)
    mutate <- ifelse("mutate" %in% names(ls), ls$mutate, 0.1)
    c_method <- ifelse("c_method" %in% names(ls), ls$c_method, "CMAES")

    ctrl <- list(VTR = VTR, NP = NP, itermax = itermax, reltol = reltol,
                 cutoff = cutoff, mutate = mutate, parallel = parallel,
                 file_type = file_type, file_name = file_name,
                 file_dir = file_dir, na_value = na_value, ncore = ncore,
                 method = method, c_method = c_method, timeout = timeout)
  } else if (method == "sa") {

    N <- ifelse("N" %in% names(ls), ls$N, 2^2)
    # print(ls$vars_sim)
    if ("vars_sim" %in% names(ls)) {
      vars_sim <- ls$vars_sim
      names(vars_sim) <- names(ls$vars_sim) # ensure that the names are carried over
    } else {
      vars_sim <- list(
        surf_temp = list(var = "HYD_temp",
                         month = c(10:12, 1:3),
                         depth_range = c(0, 2)
        )
      )
    }

    ctrl <- list(N = N, parallel = parallel, ncore = ncore, na_value = na_value,
                 file_type = file_type, file_name = file_name,
                 file_dir = file_dir, vars_sim = vars_sim, method = method,
                 timeout = timeout)
  }
  return(ctrl)
}
