#' Check that calibration parameters exist in the built model
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Every row of a `param` dataframe names a field in one of the model's
#' configuration files. \code{\link{calib_aeme}} and \code{\link{sa_aeme}}
#' write those fields through `AEME::input_model_parameters()`, which matches
#' on name: a name the built model does not carry is either a hard error
#' inside a forward run (or a PEST worker) or a silent no-op. Neither is an
#' obvious way to find a typo, or a module that was never switched on.
#'
#' `check_param_targets()` compares each parameter name against the keys
#' present in `AEME::configuration(aeme)[[model]]` - every namelist / YAML
#' field, plus the row keys of any table-valued block such as the AED
#' phytoplankton and zooplankton parameter tables - and returns the rows
#' that match nothing. Run it after `AEME::build_aeme()` and before the
#' first `calib_aeme()` / `sa_aeme()` call.
#'
#' The comparison is by bare name (any `block/` or `phyto/` qualifier is
#' stripped first), so it catches typos and inactive modules but not a
#' parameter placed in the wrong namelist block.
#'
#' @param param dataframe; as passed to \code{\link{calib_aeme}}, requiring
#'   `name` and, for the message, `file`.
#' @param aeme A built `aeme` object.
#' @param model Character or `NULL`. Which configured model to check
#'   against. `NULL` (default) takes the single `param$model`, or the first
#'   model `AEME::configuration(aeme)` carries.
#' @param error Logical. Abort when any parameter is unmatched, instead of
#'   warning and returning them. Default `FALSE`.
#'
#' @return Invisibly, the subset of `param` rows whose `name` matched no key
#'   in the built model (a zero-row dataframe when everything resolved).
#' @seealso [validate_aeme()], [calib_aeme()], [freeze_param()]
#' @export
#'
#' @examples
#' \dontrun{
#' aeme <- AEME::build_aeme(path = path, aeme = aeme, model = "glm_aed",
#'                          model_controls = mc, use_bgc = TRUE)
#' check_param_targets(param, aeme, error = TRUE)
#' }
check_param_targets <- function(param, aeme, model = NULL, error = FALSE) {

  if (!is.data.frame(param) || !"name" %in% names(param)) {
    cli::cli_abort("{.arg param} must be a data frame with a {.field name} column.")
  }

  cfg <- AEME::configuration(aeme = aeme)
  known <- intersect(c("dy_cd", "glm_aed", "gotm_wet", "simstrat"), names(cfg))

  if (is.null(model)) {
    model <- if ("model" %in% names(param) &&
                 length(unique(stats::na.omit(param$model))) == 1L) {
      unique(stats::na.omit(param$model))
    } else if (length(known) >= 1L) {
      known[1]
    } else {
      cli::cli_abort("Could not infer {.arg model} from the configuration; pass it.")
    }
  }
  if (is.null(cfg[[model]])) {
    cli::cli_abort("{.arg aeme} has no configuration for model {.val {model}}.")
  }

  keys <- tolower(.config_keys(cfg[[model]]))

  tok <- tolower(sub("^.*/", "", param$name))   # drop any block/ qualifier
  tok <- sub("^phyto/", "", tok)
  bad <- !tok %in% keys
  out <- param[bad, , drop = FALSE]

  if (nrow(out) > 0) {
    lbl <- if ("file" %in% names(out)) {
      paste0(out$name, " (", out$file, ")")
    } else {
      out$name
    }
    header <- sprintf(
      "Parameter(s) with no matching field in the built '%s' model:", model)
    body <- stats::setNames(unique(lbl), rep("*", length(unique(lbl))))
    if (isTRUE(error)) {
      cli::cli_abort(c(header, body))
    } else {
      cli::cli_warn(c(header, body,
                      "i" = "A forward run will error or silently ignore these."))
    }
  }

  invisible(out)
}

# Every name that could be a parameter target in a configured model: the
# element names at every level of the namelist / YAML list, plus the first
# column (the p_name / z_name key) of any data-frame leaf, such as the AED
# phytoplankton and zooplankton parameter tables. Deliberately broad - a
# false pass on a block name is preferable to flagging a valid parameter.
.config_keys <- function(x) {
  if (is.data.frame(x)) {
    return(if (ncol(x) > 0) as.character(x[[1]]) else character())
  }
  if (is.list(x)) {
    out <- names(x)
    for (el in x) out <- c(out, .config_keys(el))
    return(unique(out[nzchar(out)]))
  }
  character()
}
