#' Create control list for calibration or sensitivity analysis
#'
#' @param method The method to be used. It can be either "calib" for calibration
#' or "sa" for sensitivity analysis.
#' @param ... Additional arguments to be passed to the function.
#'
#' @return list with the control parameters
#' @export
#'
#' @examples
#' ctrl <- create_control("calib", VTR = -Inf, NP = 10, itermax = 30,
#'                         reltol = 0.07, cutoff = 0.25, mutate = 0.1,
#'                         parallel = TRUE, out_file = "results.db",
#'                         na_value = 999, ncore = 3)
#'
#'

create_control <- function(method, ...) {
  ls <- list(...)

  na_value <- ifelse("na_value" %in% names(ls), ls$na_value, 999)
  out_file <- ifelse("out_file" %in% names(ls), ls$out_file, "results.db")
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
                 out_file = out_file, na_value = na_value, ncore = ncore)
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
                 out_file = out_file,
                 vars_sim = vars_sim)
  }
  return(ctrl)
}
