#' Coerce a parameter/variable specification into a `param_var_matrix`
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A `param_var_matrix` declares which parameters are allowed to be informed
#' by which response variables. It is used in two places:
#'
#' * `c_method = "MOEDA"` masks the joint covariance it resamples from, so
#'   parameters that share no variable are never correlated (see
#'   \code{\link{calib_aeme}});
#' * `engine = "pest"` turns the same declaration into a `pestpp-ies`
#'   localizer (see \code{\link{pest_localizer}}).
#'
#' The canonical form is the dataframe that
#' \code{\link{create_param_var_matrix}} returns - one row per parameter, one
#' logical column per variable - but writing that out by hand is tedious.
#' This function accepts the shorthand forms as well and returns the
#' canonical dataframe, so both engines take either.
#'
#' @section Accepted forms:
#'
#' \describe{
#'   \item{dataframe}{The canonical form: columns `model`, `file`,
#'     `name_full` and one logical column per variable. Passed through with
#'     validation only.}
#'   \item{named list}{Keyed by variable, each element a character vector of
#'     *selectors*. A selector matches a full parameter name
#'     (`"light/Kw[1]"`), a parameter group (`"light"`) or a bare parameter
#'     name (`"Kw"`); `TRUE` or `"all"` selects every parameter and
#'     `character(0)`, `FALSE` or `NULL` selects none. A selector that
#'     matches nothing is an error rather than a silent omission.}
#'   \item{logical matrix}{Parameters as rows (row names are `name_full`, or
#'     `param` order when unnamed), variables as columns.}
#' }
#'
#' @section Defaults for what you leave out:
#'
#' A **variable** that the specification does not mention is left
#' *unrestricted*: every parameter is linked to it. A partial specification
#' therefore only tightens what it names, and forgetting a variable cannot
#' silently remove parameters from the problem.
#'
#' A **parameter** that no variable selects is linked to nothing.
#' \code{\link{calib_aeme}} warns about those and drops them from the
#' calibration, which is the intended reading of "this parameter drives
#' nothing I am fitting" - and, on the PEST path, avoids handing
#' `pestpp-ies` a parameter it would treat as fixed without saying so.
#'
#' @param x A dataframe, named list, logical matrix or `NULL`. See
#'   *Accepted forms*.
#' @param param dataframe; as passed to \code{\link{calib_aeme}}, requiring
#'   the columns `model`, `file`, `name` and (optionally) `group`, `index`,
#'   `name_full`.
#' @param vars_sim Character vector of the variables being calibrated.
#'
#' @return A dataframe with columns `model`, `file`, `name_full` and one
#'   logical column per entry of `vars_sim`, or `NULL` when `x` is `NULL`.
#' @seealso [create_param_var_matrix()], [pest_localizer()], [calib_aeme()]
#' @export
#'
#' @examples
#' param <- data.frame(model = "glm_aed", file = "glm3.nml",
#'                     group = c("light", "light", NA),
#'                     name = c("Kw", "ce", "MET_tmpair"),
#'                     index = c(1, NA, NA))
#' # Temperature is driven by the light and meteorological parameters;
#' # oxygen only by Kw. Any other variable would be left unrestricted.
#' as_param_var_matrix(list(HYD_temp = c("light", "MET_tmpair"),
#'                          CHM_oxy = "Kw"),
#'                     param = param,
#'                     vars_sim = c("HYD_temp", "CHM_oxy"))
as_param_var_matrix <- function(x, param, vars_sim) {

  if (is.null(x)) return(NULL)

  if (missing(vars_sim) || length(vars_sim) == 0) {
    cli::cli_abort("{.arg vars_sim} must name at least one variable.")
  }
  vars_sim <- unique(as.character(vars_sim))
  param <- .pvm_param(param)

  if (is.data.frame(x)) {
    .pvm_from_df(x, param, vars_sim)
  } else if (is.matrix(x)) {
    .pvm_from_df(.pvm_mat_to_df(x, param), param, vars_sim)
  } else if (is.list(x)) {
    .pvm_from_list(x, param, vars_sim)
  } else {
    cli::cli_abort(c(
      "{.arg param_var_matrix} must be a dataframe, a named list or a
       logical matrix, not {.cls {class(x)[1]}}.",
      "i" = "See {.fn as_param_var_matrix} for the accepted forms."
    ))
  }
}

# Internal helpers -------------------------------------------------------

#' Columns of a `param_var_matrix` that are not variables.
#' @noRd
.pvm_id_cols <- c("model", "file", "name_full", "group", "name", "index")

#' Fill in the parameter columns `as_param_var_matrix()` relies on.
#' @noRd
.pvm_param <- function(param) {
  if (!is.data.frame(param)) {
    cli::cli_abort("{.arg param} must be a dataframe.")
  }
  miss <- setdiff(c("model", "file", "name"), names(param))
  if (length(miss) > 0) {
    cli::cli_abort("{.arg param} is missing the column{?s} {.val {miss}}.")
  }
  if (!"group" %in% names(param)) param$group <- NA_character_
  if (!"index" %in% names(param)) param$index <- NA
  if (!"name_full" %in% names(param)) {
    param$name_full <- encode_param(param$group, param$name, param$index)
  }
  param
}

#' A `model`-qualified key, because `name_full` alone is not unique across
#' models - the same parameter can be calibrated in more than one of them.
#' @noRd
.pvm_key <- function(model, name_full) paste0(model, "::", name_full)

#' An all-`value` canonical matrix for `param`.
#' @noRd
.pvm_skeleton <- function(param, vars_sim, value = FALSE) {
  out <- data.frame(model = as.character(param$model),
                    file = as.character(param$file),
                    name_full = as.character(param$name_full),
                    stringsAsFactors = FALSE)
  for (v in vars_sim) out[[v]] <- value
  out
}

#' Expand the named-list shorthand.
#' @noRd
.pvm_from_list <- function(x, param, vars_sim) {

  nms <- names(x)
  if (length(x) == 0 || is.null(nms) || any(!nzchar(nms))) {
    cli::cli_abort(c(
      "A {.arg param_var_matrix} list must be named by variable.",
      "i" = "e.g. {.code list(HYD_temp = c(\"light\", \"mixing\"))}"
    ))
  }
  dups <- unique(nms[duplicated(nms)])
  if (length(dups) > 0) {
    cli::cli_abort("{.arg param_var_matrix} names {.val {dups}} more than
                   once.")
  }
  unknown <- setdiff(nms, vars_sim)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "{.arg param_var_matrix} names variable{?s} that {?is/are} not being
       calibrated: {.val {unknown}}.",
      "i" = "{.arg vars_sim} is {.val {vars_sim}}."
    ))
  }

  out <- .pvm_skeleton(param, vars_sim, value = FALSE)

  # A variable nobody mentioned is unrestricted, not empty - see the
  # "Defaults for what you leave out" section.
  unlisted <- setdiff(vars_sim, nms)
  if (length(unlisted) > 0) {
    for (v in unlisted) out[[v]] <- TRUE
    AEME::cli_safe(
      paste0("{.arg param_var_matrix} does not mention {.val ",
             paste(unlisted, collapse = ", "), "}; leaving ",
             if (length(unlisted) == 1) "it" else "them",
             " linked to every parameter."),
      FUN = cli::cli_alert_info)
  }

  for (v in nms) out[[v]] <- .pvm_resolve(x[[v]], param, v)
  out
}

#' Resolve one variable's selectors to a logical vector over `param` rows.
#' @noRd
.pvm_resolve <- function(sel, param, v) {

  n <- nrow(param)
  if (is.null(sel)) return(rep(FALSE, n))

  if (is.logical(sel)) {
    if (length(sel) == 1) return(rep(isTRUE(sel), n))
    if (length(sel) == n) {
      sel[is.na(sel)] <- FALSE
      return(sel)
    }
    cli::cli_abort("The entry for {.val {v}} is a logical vector of length
                   {.val {length(sel)}}; it must be length 1 or {.val {n}}.")
  }

  sel <- as.character(sel)
  sel <- sel[!is.na(sel)]
  if (length(sel) == 0) return(rep(FALSE, n))
  if (length(sel) == 1 && tolower(sel) == "all") return(rep(TRUE, n))

  keep <- rep(FALSE, n)
  bad <- character()
  for (s in sel) {
    hit <- param$name_full == s |
      (!is.na(param$group) & param$group == s) |
      param$name == s
    hit[is.na(hit)] <- FALSE
    if (any(hit)) keep <- keep | hit else bad <- c(bad, s)
  }

  if (length(bad) > 0) {
    grps <- unique(param$group[!is.na(param$group)])
    cli::cli_abort(c(
      "{.arg param_var_matrix}: no parameter matches {.val {bad}} for
       {.val {v}}.",
      "i" = "A selector matches a full parameter name, a parameter group or
             a bare parameter name.",
      "i" = "Group{?s}: {.val {grps}}.",
      "i" = "Name{?s}: {.val {unique(param$name)}}."
    ))
  }
  keep
}

#' Turn a parameters-by-variables logical matrix into the dataframe form.
#' @noRd
.pvm_mat_to_df <- function(x, param) {

  if (is.null(colnames(x))) {
    cli::cli_abort("A matrix {.arg param_var_matrix} must have variable names
                   as column names.")
  }
  rn <- rownames(x)
  if (is.null(rn)) {
    if (nrow(x) != nrow(param)) {
      cli::cli_abort("An unnamed matrix {.arg param_var_matrix} must have one
                     row per parameter ({.val {nrow(param)}}), not
                     {.val {nrow(x)}}.")
    }
    rn <- param$name_full
  }
  out <- data.frame(name_full = rn, stringsAsFactors = FALSE)
  for (v in colnames(x)) out[[v]] <- as.logical(x[, v])
  out
}

#' Validate the canonical dataframe form, filling in what can be inferred.
#' @noRd
.pvm_from_df <- function(x, param, vars_sim) {

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  if (!"name_full" %in% names(x)) {
    if ("name" %in% names(x)) {
      grp <- if ("group" %in% names(x)) x$group else NA_character_
      idx <- if ("index" %in% names(x)) x$index else NA
      x$name_full <- encode_param(grp, x$name, idx)
    } else {
      cli::cli_abort("A dataframe {.arg param_var_matrix} needs a
                     {.field name_full} column, or {.field group},
                     {.field name} and {.field index} to build one from.")
    }
  }
  x$name_full <- as.character(x$name_full)

  if (!"model" %in% names(x)) {
    x$model <- param$model[match(x$name_full, param$name_full)]
  }
  if (!"file" %in% names(x)) {
    x$file <- param$file[match(x$name_full, param$name_full)]
  }

  # Rows for models that are not being calibrated here are not this call's
  # business - calib_aeme() hands one model at a time to the PEST engine -
  # so drop them without comment. A row whose parameter is unknown within a
  # model that *is* present is a typo, and worth saying so.
  x <- x[x$model %in% unique(param$model), , drop = FALSE]
  key_x <- .pvm_key(x$model, x$name_full)
  unknown <- setdiff(key_x, .pvm_key(param$model, param$name_full))
  if (length(unknown) > 0) {
    AEME::cli_safe(
      paste0("Dropping {.val ", length(unknown), "} {.arg param_var_matrix} ",
             "row{?s} for parameters that are not being calibrated: {.val ",
             paste(sub("^.*::", "", unknown), collapse = ", "), "}"),
      FUN = cli::cli_alert_warning)
    x <- x[!key_x %in% unknown, , drop = FALSE]
  }

  miss_v <- setdiff(vars_sim, names(x))
  if (length(miss_v) > 0) {
    for (v in miss_v) x[[v]] <- TRUE
    AEME::cli_safe(
      paste0("{.arg param_var_matrix} has no column for {.val ",
             paste(miss_v, collapse = ", "), "}; leaving ",
             if (length(miss_v) == 1) "it" else "them",
             " linked to every parameter."),
      FUN = cli::cli_alert_info)
  }

  extra <- setdiff(names(x), c(.pvm_id_cols, vars_sim))
  if (length(extra) > 0) {
    AEME::cli_safe(
      paste0("Ignoring {.arg param_var_matrix} columns that are not in ",
             "{.arg vars_sim}: {.val ", paste(extra, collapse = ", "), "}"),
      FUN = cli::cli_alert_info)
  }

  out <- x[, c("model", "file", "name_full", vars_sim), drop = FALSE]
  for (v in vars_sim) {
    val <- out[[v]]
    if (is.character(val)) val <- toupper(val) %in% c("TRUE", "T", "1")
    val <- as.logical(val)
    if (anyNA(val)) {
      cli::cli_abort("{.arg param_var_matrix} column {.field {v}} has
                     {.val {sum(is.na(val))}} missing value{?s}; it must be
                     {.code TRUE} or {.code FALSE}.")
    }
    out[[v]] <- val
  }
  rownames(out) <- NULL
  out
}
