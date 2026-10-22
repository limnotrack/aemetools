#' Mark a sensitivity-analysis return list as a failed run.
#'
#' No-op for `method == "calib"`, so calibration's `sum(unlist(res))` fit
#' calculation in `eval_param_chunk()` never picks up a stray `failed`
#' element.
#'
#' @param return_list list; the value `run_and_fit()` is about to return.
#' @param method string; `"calib"` or `"sa"`.
#'
#' @return `return_list`, with `$failed <- TRUE` added when `method == "sa"`.
#' @noRd
mark_sa_failure <- function(return_list, method) {
  if (identical(method, "sa")) {
    return_list$failed <- TRUE
  }
  return_list
}
