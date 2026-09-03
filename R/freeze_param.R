#' Freeze calibration parameters at their current value
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Sets `min` and `max` equal to `value` for the selected parameters (every
#' row when `names` is `NULL`). A frozen parameter is still passed to
#' \code{\link{calib_aeme}} and \code{\link{sa_aeme}}:
#'
#' * the built-in engines write it to the model configuration and drop it
#'   from the search;
#' * the PEST engines keep it in the control file as `partrans = "fixed"`,
#'   so it stays visible in the parameter map, the `pestpp-ies` ensembles,
#'   `pest_param_summary()` and the sensitivity output.
#'
#' This is the mechanism for carrying an earlier stage of a staged
#' calibration forward as a constant - see \code{\link{carry_param}} for the
#' common case of freezing the winners of a finished run.
#'
#' @param param dataframe; as passed to \code{\link{calib_aeme}}, requiring
#'   `value`, and `name` when `names` is used.
#' @param names Character or `NULL`. Which `param$name` entries to freeze.
#'   `NULL` (default) freezes every row. A name not present in `param` is an
#'   error rather than a silent no-op.
#'
#' @return `param`, with `min == max == value` for the selected rows.
#' @seealso [carry_param()], [pest_param_table()], [set_param_log()]
#' @export
#'
#' @examples
#' param <- data.frame(
#'   model = "glm_aed", file = "glm4.nml", group = "light",
#'   name = c("Kw", "ce"), index = NA_real_,
#'   value = c(0.5, 0.0013), min = c(0.1, 5e-4), max = c(1.5, 5e-3)
#' )
#' freeze_param(param, names = "Kw")
freeze_param <- function(param, names = NULL) {

  cols <- base::names(param)
  if (!is.data.frame(param) || !"value" %in% cols) {
    cli::cli_abort("{.arg param} must be a data frame with a {.field value} column.")
  }

  rows <- rep(TRUE, nrow(param))
  if (!is.null(names)) {
    if (!"name" %in% cols) {
      cli::cli_abort("{.arg param} needs a {.field name} column to freeze by name.")
    }
    unknown <- setdiff(names, param$name)
    if (length(unknown) > 0) {
      cli::cli_abort("Not found in {.arg param$name}: {.val {unknown}}.")
    }
    rows <- param$name %in% names
  }

  bad <- rows & is.na(param$value)
  if (any(bad)) {
    cli::cli_abort("Cannot freeze {.val {param$name[bad]}}: {.field value} is {.code NA}.")
  }

  param$min[rows] <- param$value[rows]
  param$max[rows] <- param$value[rows]
  param
}

#' Carry the best parameters of a finished calibration forward, frozen
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Takes the best parameter set from a completed \code{\link{calib_aeme}}
#' run (via \code{\link{get_best_params}}) and freezes it with
#' \code{\link{freeze_param}}, returning a `param` dataframe ready to
#' `rbind()` onto the adjustable parameters of the next stage of a staged
#' calibration.
#'
#' Physics constrains biogeochemistry but barely the reverse, so a staged
#' calibration fixes the water balance, then the thermal structure, then the
#' biogeochemistry - each stage's winners becoming the next stage's
#' constants. `carry_param()` is the "freeze the winners" half of that; the
#' looser alternative, keeping them adjustable but re-bounded by the
#' posterior, is `create_pest_control(prior_par_ensemble = )`.
#'
#' @param x A `calib` list from \code{\link{read_calib}}, or a dataframe
#'   from \code{\link{get_best_params}}.
#' @param names,except Character or `NULL`. `names` keeps only those
#'   parameters; `except` then drops any of those. Both match on
#'   `param$name`.
#' @param fit_col Character. Fit column ranked on when `x` is a `calib`
#'   list; passed to \code{\link{get_best_params}}. Default `"fit"`.
#'
#' @return A `param` dataframe (`model`, `file`, `name`, `value`, `min`,
#'   `max`, `group`, `index`, plus `name_full`) with `min == max == value`.
#' @seealso [freeze_param()], [get_best_params()], [create_pest_control()]
#' @export
carry_param <- function(x, names = NULL, except = NULL, fit_col = "fit") {

  pcols <- AEME::param_colnames(incl_opt = FALSE)
  best <- if (is.data.frame(x)) {
    x
  } else {
    get_best_params(calib = x, fit_col = fit_col)
  }

  miss <- setdiff(pcols, base::names(best))
  if (length(miss) > 0) {
    cli::cli_abort(c("{.arg x} is missing column{?s}: {.field {miss}}.",
                     "i" = "Pass a {.fn read_calib} object or a
                            {.fn get_best_params} result."))
  }
  best <- best[, pcols, drop = FALSE]

  if (!is.null(names)) best <- best[best$name %in% names, , drop = FALSE]
  if (!is.null(except)) best <- best[!best$name %in% except, , drop = FALSE]

  drop_na <- is.na(best$value)
  if (any(drop_na)) {
    AEME::cli_safe(
      paste0("Dropping parameter{?s} with no best value: {.val ",
             paste(unique(best$name[drop_na]), collapse = ", "), "}"),
      FUN = cli::cli_alert_warning)
    best <- best[!drop_na, , drop = FALSE]
  }

  best <- freeze_param(best)
  best$name_full <- encode_param(best$group, best$name, best$index)
  best
}
