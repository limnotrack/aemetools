#' Create control list for calibration or sensitivity analysis
#'
#' @param method The method to be used. It can be either "calib" for calibration
#' or "sa" for sensitivity analysis.
#' @param ... Additional arguments to be passed to the function
#'  `create_control`. The arguments are different for calibration and
#'  sensitivity analysis. There are arguments which are common to both methods:
#'
#'  * `file_type` string; file type to write the output to. Options are c("csv",
#'  "db"). Defaults to "db".
#'  * `file_name` string; file name to write the output to. Only used if `file_type`
#'  is "db". Defaults to "results.db"
#'  * `na_value` value to replace NA values with in observations. Defaults to 999.
#'  * `parallel` boolean; run calibration in parallel. Default to TRUE
#'  * `ncore`: The number of cores to use for the calibration. This is only used
#'  if `parallel = TRUE`. Default to `parallel::detectCores() - 1`.
#'
#'   For calibration, the arguments are:
#' * `VTR` Value to be reached. The optimization process will stop if
#' either the maximum number of iterations itermax is reached or the best
#' parameter vector bestmem has found a value fn(bestmem) <= VTR. Default to
#'  -Inf.
#' * `NP` number of population members. Defaults to NA; if the user does not
#'  change the value of NP from NA it is reset as
#'   `10 * sum(param$model == model)`. For many  problems it is best to set NP
#'   to be at least 10 times the length of the parameter vector.
#' * `itermax` the maximum iteration (population generation) allowed.
#' Default is 200.
#' * `reltol` relative convergence tolerance. The algorithm stops if it is
#'  unable to reduce the value by a factor of `reltol * (abs(val) + reltol)`.
#'  Default = 0.07
#'  * `cutoff`: The quantile cutoff used to select the parents for the next
#'  generation. For example, if `cutoff = 0.25`, the best 25% of the population
#'  will be used as parents for the next generation.
#'  * `mutate` fraction of population to undergo mutation (0-1).
#'
#' For sensitivity analysis, the arguments are:
#'
#' -   `N`: The initial sample size of the base sample matrix.
#' -   `vars_sim`: A named list of output variables for sensitivity analysis.
#'   The name is user defined but each list must contain:
#'     - `var`: The variable name to use for the sensitivity analysis.
#'     - `month`: A vector of months to use for the sensitivity analysis.
#'     - `depth_range`: A vector of length 2 with the minimum and maximum depth
#'   range to use for the sensitivity analysis.
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

  check_names <- c("na_value", "file_type", "file_name", "parallel", "ncore",
                   "VTR", "NP", "itermax", "reltol", "cutoff", "mutate",
                   "N", "vars_sim")

  if (any(!names(ls) %in% check_names)) {
    stop(strwrap("Invalid argument(s) passed to create_control. Please check
                 which arguments to include with `?create_control()`."))
  }



  na_value <- ifelse("na_value" %in% names(ls), ls$na_value, 999)
  file_type <- ifelse("file_type" %in% names(ls), ls$file_type, "db")
  if (file_type == "db") {
    file_name <- ifelse("file_name" %in% names(ls), ls$file_name, "results.db")
  } else if (file_type == "csv") {
    file_name <- NULL
  }
  parallel <- ifelse("parallel" %in% names(ls), ls$parallel, TRUE)
  ncore <- ifelse("ncore" %in% names(ls), ls$ncore,
                  (parallel::detectCores() - 1))


  if (method == "calib") {

    VTR <- ifelse("VTR" %in% names(ls), ls$VTR, -Inf)
    NP <- ifelse("NP" %in% names(ls), ls$NP, 10)
    itermax <- ifelse("itermax" %in% names(ls), ls$itermax, 30)
    reltol <- ifelse("reltol" %in% names(ls), ls$reltol, 0.07)
    cutoff <- ifelse("cutoff" %in% names(ls), ls$cutoff, 0.25)
    mutate <- ifelse("mutate" %in% names(ls), ls$mutate, 0.1)

    ctrl <- list(VTR = VTR, NP = NP, itermax = itermax, reltol = reltol,
                 cutoff = cutoff, mutate = mutate, parallel = parallel,
                 file_type = file_type, file_name = file_name,
                 na_value = na_value, ncore = ncore, method = method)
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
                 vars_sim = vars_sim, method = method)
  }
  return(ctrl)
}
