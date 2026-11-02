#' Create control list (superseded)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' `create_control()` is superseded. Please use
#' [create_calib_control()] or [create_sa_control()]
#' instead.
#'
#' @param method Character. Either `"calib"` or `"sa"`.
#' @param ... Arguments passed to the appropriate function.
#' 
#' @importFrom lifecycle signal_stage
#' @importFrom rlang arg_match
#'
#' @export

create_control <- function(method = c("calib", "sa"), ...) {
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "create_control()",
    with = "create_calib_control()",
    details = paste(
      "Use `create_calib_control()` when method = 'calib'.",
      "Use `create_sa_control()` when method = 'sa'."
    )
  )
  method <- rlang::arg_match(method)
  if (method == "calib") {
    create_calib_control(...)
  } else {
    create_sa_control(...)
  }
}
