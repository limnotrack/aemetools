#' Build a `pestpp-ies` localizer from a parameter/variable matrix
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Turns a `param_var_matrix` (see \code{\link{as_param_var_matrix}}) into the
#' matrix `pestpp-ies` reads as `++ies_localizer(...)`: rows are observation
#' groups, columns are parameters, and a non-zero entry means "this parameter
#' may be updated by this group's observations". Severing a pair stops a
#' spurious ensemble cross-correlation - a light-extinction parameter being
#' pulled around by a bottom-oxygen residual, say - from entering the update
#' at all.
#'
#' The two axes come for free from the tables aemetools already writes.
#' \code{\link{pest_param_table}} gives each aemetools parameter group its own
#' PEST parameter group, and \code{\link{pest_obs_table}} gives each AEME
#' variable its own observation group, so "which parameters inform which
#' variables" is exactly an observation-group by parameter incidence matrix.
#'
#' @section What the entries mean:
#'
#' Localization in `pestpp-ies` is binary in effect: the Hadamard application
#' of fractional localizer weights is no longer live upstream, and what
#' remains is per-case subsetting of parameters and observations. `0.5` and
#' `1.0` would behave identically, so this function writes `1` and `0` only.
#'
#' @section Why the matrix is written dense:
#'
#' Both the ASCII and CSV readers skip zero entries when building the sparse
#' triplets, so an explicit `0` and an omitted name produce an identical
#' sparse structure. Numerically, dense buys nothing. It is written dense for
#' the diagnostics:
#'
#' * every name that *is* in the file is checked against the control file, so
#'   a mistyped group is a hard error at startup rather than a silent
#'   omission (unless `ies_localizer_forgive_missing` is set);
#' * a parameter that appears in no case is never updated - `pestpp-ies` says
#'   so for parameters, but there is no equivalent message on the observation
#'   side. An omitted `obgnme` silently removes that variable's data from the
#'   update; a row that is present and all-zero at least draws a warning;
#' * the solver's own report of what it localized only covers names that are
#'   in the matrix.
#'
#' In short: a name absent from the file is neither validated against the
#' control file nor reported on the observation side; a name present with a
#' zero is both.
#'
#' @section Gotchas:
#'
#' * A localized upgrade is solved once per case rather than once overall, so
#'   it is materially slower than an unlocalized one. `ies_num_threads` (via
#'   `pestpp_options`) multithreads it.
#' * A case whose active observation list or active parameter list is empty
#'   for an iteration is dropped entirely. Parameters tied only to
#'   observations that go inactive - through
#'   `ies_drop_conflicts`, weight adjustment, or all their runs failing -
#'   therefore freeze for that iteration, with no message.
#' * The localizer only applies to `pestpp-ies`. `pestpp-glm` and
#'   `pestpp-sen` ignore it.
#'
#' @param param_var_matrix A `param_var_matrix`, or any form
#'   \code{\link{as_param_var_matrix}} accepts once it has been normalised.
#'   Must contain a row for every parameter in `par_tbl`.
#' @param par_tbl dataframe; from \code{\link{pest_param_table}}, carrying its
#'   `map` attribute.
#' @param obs_tbl dataframe; from \code{\link{pest_obs_table}}, carrying its
#'   `map` attribute.
#' @param file Character or `NULL`. When given, the matrix is also written to
#'   this path in PEST ASCII matrix format, ready for
#'   `++ies_localizer(...)`.
#'
#' @return Invisibly, a numeric 0/1 matrix with observation groups as row
#'   names and `parnme` as column names.
#' @seealso [as_param_var_matrix()], [create_pest_control()],
#'   [pest_param_table()], [pest_obs_table()]
#' @export
pest_localizer <- function(param_var_matrix, par_tbl, obs_tbl, file = NULL) {

  pmap <- attr(par_tbl, "map")
  omap <- attr(obs_tbl, "map")
  if (is.null(pmap)) {
    cli::cli_abort("{.arg par_tbl} has no {.field map} attribute; it must come
                   from {.fn pest_param_table}.")
  }
  if (is.null(omap) || !"var_aeme" %in% names(omap)) {
    cli::cli_abort("{.arg obs_tbl} has no {.field map} attribute; it must come
                   from {.fn pest_obs_table}.")
  }

  pvm <- as.data.frame(param_var_matrix, stringsAsFactors = FALSE)
  vars <- setdiff(names(pvm), .pvm_id_cols)
  if (length(vars) == 0) {
    cli::cli_abort("{.arg param_var_matrix} has no variable columns.")
  }

  # Columns: one per PEST parameter, in control-file order. A parameter with
  # no row here would be left out of every case, which pestpp-ies treats as
  # fixed - so this is an error, not something to default.
  key_par <- .pvm_key(pmap$model, pmap$name_full)
  idx <- match(key_par, .pvm_key(pvm$model, pvm$name_full))
  if (anyNA(idx)) {
    cli::cli_abort(c(
      "{.arg param_var_matrix} has no row for
       {.val {unique(pmap$name_full[is.na(idx)])}}.",
      "i" = "A parameter missing from the localizer is treated as fixed by
             {.val pestpp-ies} and would never be adjusted."
    ))
  }

  # Rows: one per observation group. In both obj_modes an observation group
  # is one AEME variable, but take the union rather than assuming it.
  obg <- unique(obs_tbl$obgnme)
  var_of_obs <- omap$var_aeme[match(obs_tbl$obsnme, omap$obsnme)]

  loc <- matrix(0, nrow = length(obg), ncol = nrow(pmap),
                dimnames = list(obg, pmap$parnme))
  unrestricted <- character()

  for (i in seq_along(obg)) {
    v <- intersect(unique(var_of_obs[obs_tbl$obgnme == obg[i]]), vars)
    if (length(v) == 0) {
      # No opinion was expressed about this variable, so do not restrict it.
      loc[i, ] <- 1
      unrestricted <- c(unrestricted, obg[i])
      next
    }
    linked <- as.matrix(pvm[idx, v, drop = FALSE])
    loc[i, ] <- as.numeric(apply(linked, 1, any))
  }

  if (length(unrestricted) > 0) {
    AEME::cli_safe(
      paste0("{.arg param_var_matrix} says nothing about {.val ",
             paste(unrestricted, collapse = ", "), "}; linking ",
             if (length(unrestricted) == 1) "it" else "them",
             " to every parameter in the localizer."),
      FUN = cli::cli_alert_info)
  }

  # An all-zero column is a parameter pestpp-ies would silently never update;
  # an all-zero row is a variable whose data would never enter the update.
  # Neither is ever what was meant, and neither is reported clearly by the
  # solver, so both stop the run here.
  dead_par <- colSums(loc) == 0
  if (any(dead_par)) {
    cli::cli_abort(c(
      "{.val {sum(dead_par)}} parameter{?s} {?is/are} not linked to any
       variable: {.val {pmap$name_full[dead_par]}}.",
      "i" = "{.val pestpp-ies} treats an unlinked parameter as fixed. Link
             each of them to a variable, or drop them from {.arg param}."
    ))
  }
  dead_obs <- rowSums(loc) == 0
  if (any(dead_obs)) {
    cli::cli_abort(c(
      "Observation group{?s} {.val {obg[dead_obs]}} {?is/are} not linked to
       any parameter.",
      "i" = "Those observations would carry weight in phi but never inform
             the update."
    ))
  }

  if (all(loc == 1)) {
    AEME::cli_safe(
      "The localizer links every parameter to every observation group, so it
       imposes no restriction - but a localized upgrade is still solved case
       by case, which is slower.",
      FUN = cli::cli_alert_warning)
  }

  if (!is.null(file)) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    .pest_write_cov(loc, file)
    n_cut <- sum(loc == 0)
    AEME::cli_safe(
      paste0("Wrote localizer {.file ", file, "} ({cli::qty(", nrow(loc),
             ")}{.val ", nrow(loc), "} observation group{?s} x ",
             "{cli::qty(", ncol(loc), ")}{.val ", ncol(loc),
             "} parameter{?s}, {cli::qty(", n_cut, ")}{.val ", n_cut,
             "} pair{?s} severed)."),
      FUN = cli::cli_alert_success)
  }

  invisible(loc)
}

#' Resolve `ctrl$localizer` into a written localizer file and the matching
#' `++ies_localizer` option.
#'
#' Called from calib_aeme_pest() once the parameter and observation tables
#' exist, in the same spirit as .pest_setup_ensembles().
#' @noRd
.pest_setup_localizer <- function(ctrl, par_tbl, obs_tbl, param, vars_sim) {

  loc <- ctrl$localizer
  if (is.null(loc)) return(ctrl)

  if (!identical(ctrl$exe, "pestpp-ies")) {
    AEME::cli_safe(
      paste0("Ignoring {.arg localizer}: only {.val pestpp-ies} localizes, ",
             "not {.val ", ctrl$exe, "}."),
      FUN = cli::cli_alert_warning)
    return(ctrl)
  }

  opts <- ctrl$pestpp_options %||% list()
  if (!is.null(opts$ies_localizer)) {
    AEME::cli_safe(
      "Keeping the {.code ies_localizer} set through {.arg pestpp_options};
       {.arg localizer} is ignored.",
      FUN = cli::cli_alert_info)
    return(ctrl)
  }

  dest <- file.path(ctrl$pest_dir, paste0(ctrl$case, "_localizer.mat"))

  if (is.character(loc)) {
    # A path is handed to PEST++ as-is, exactly as `prior_cov` is.
    if (!file.exists(loc)) {
      cli::cli_abort("{.arg localizer} file not found: {.file {loc}}")
    }
    file.copy(loc, dest, overwrite = TRUE)
  } else {
    pvm <- as_param_var_matrix(loc, param = param, vars_sim = vars_sim)
    pest_localizer(pvm, par_tbl = par_tbl, obs_tbl = obs_tbl, file = dest)
  }

  opts$ies_localizer <- basename(dest)
  ctrl$pestpp_options <- opts
  ctrl
}
