# Internal engine shared by run_aeme_ensemble()'s two paths: the sampling
# path (draw members from the parameter bounds) and the supplied-ensemble
# path (run a caller-supplied set of parameter vectors). Both hand this a
# list of long `param` dataframes with the `value` column already set.

#' Run one model over a list of parameter sets.
#'
#' @param aeme,model,path,parallel,ncore,na_value as in [run_aeme_ensemble()].
#' @param member_params list; each element a long `param` dataframe for one
#'   ensemble member, `value` already populated. May be named (realisation
#'   ids); names are carried onto the result.
#'
#' @return A list the same length as `member_params`: each element is the
#'   model-output list `AEME::output(a2)$ens_001[[model]]`, or `NULL` when
#'   that member's run failed (`run_aeme_param()` returned `na_value`).
#' @noRd
.run_member_ensemble <- function(aeme, model, member_params, path,
                                 parallel = FALSE, ncore = NULL,
                                 na_value = 999) {

  n <- length(member_params)
  if (n == 0) return(list())

  lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)
  if (is.null(ncore)) ncore <- parallel::detectCores() - 1
  ncore <- max(1L, min(ncore, n))

  run_one <- function(mp, run_path) {
    a2 <- aemetools::run_aeme_param(aeme = aeme, param = mp, model = model,
                                    path = run_path, na_value = na_value,
                                    return_aeme = TRUE)
    # A failed run returns the `na_value` scalar, not an Aeme object.
    if (!methods::is(a2, "Aeme")) return(NULL)
    AEME::output(a2)[["ens_001"]][[model]]
  }

  if (parallel) {
    temp_dirs <- make_temp_dir(model, lake_dir, n = ncore)
    idx_list <- split(seq_len(n), rep(seq_len(ncore), length.out = n))

    cl <- aeme_make_cluster(ncore)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(
      cl, varlist = c("member_params", "idx_list", "aeme", "model",
                      "temp_dirs", "na_value", "run_one"),
      envir = environment())
    message("Running a ", model, " ensemble of ", n, " member",
            if (n == 1) "" else "s", " using ", ncore, " core",
            if (ncore == 1) "" else "s", ". [", format(Sys.time()), "]")

    res_by_worker <- parallel::parLapply(cl, seq_along(idx_list), function(k) {
      stats::setNames(
        lapply(idx_list[[k]], function(j) run_one(member_params[[j]],
                                                  temp_dirs[k])),
        as.character(idx_list[[k]]))
    })
    # Rebuild in the original member order. A pre-sized list starts all-NULL,
    # so failed members (NULL) keep their slot without `[[<-` deleting it.
    out_list <- vector("list", n)
    for (w in res_by_worker) {
      for (nm in names(w)) {
        if (!is.null(w[[nm]])) out_list[[as.integer(nm)]] <- w[[nm]]
      }
    }
  } else {
    out_list <- lapply(seq_len(n), function(j) run_one(member_params[[j]], path))
  }

  names(out_list) <- names(member_params)
  out_list
}

#' Assemble per-member model output into the `AEME::output()$ens_*` structure
#' that [plot_ensemble()] consumes.
#'
#' Members that failed for any requested model are dropped and the survivors
#' renumbered `ens_001 .. ens_00k`; `n_members` is `k`. The surviving
#' realisation ids (when the members were named) are stashed on
#' `attr(., "realisation")` so the `ens_i -> realisation` map is recoverable.
#'
#' @param mod_list named list (one element per model) of the lists returned
#'   by `.run_member_ensemble()`.
#' @noRd
.assemble_ens_output <- function(aeme, mod_list, model) {

  n <- length(mod_list[[1]])
  ok <- vapply(seq_len(n), function(i) {
    all(vapply(model, function(m) !is.null(mod_list[[m]][[i]]), logical(1)))
  }, logical(1))
  keep <- which(ok)

  if (length(keep) < n) {
    cli::cli_warn("{n - length(keep)} of {n} ensemble member{?s} failed to run
                   and {?was/were} dropped.")
  }
  if (length(keep) == 0) {
    cli::cli_abort("No ensemble member ran successfully.")
  }

  outp <- list()
  for (i in seq_along(keep)) {
    ens_lab <- sprintf("ens_%03d", i)
    outp[[ens_lab]] <- stats::setNames(
      lapply(model, function(m) mod_list[[m]][[keep[i]]]), model)
  }
  outp$n_members <- length(keep)

  reals <- names(mod_list[[1]])
  if (!is.null(reals)) attr(outp, "realisation") <- reals[keep]

  AEME::output(aeme) <- outp
  aeme
}

#' Coerce the `param_sets` argument of [run_aeme_ensemble()] to a list of
#' long `param` dataframes.
#'
#' Accepts an `aeme_param_sets` object, a bare list of `param` dataframes, or
#' a single long dataframe carrying an `ensemble` id column plus the `param`
#' columns.
#' @noRd
.normalise_param_sets <- function(x) {

  req <- c("model", "file", "name", "value", "min", "max")

  if (is.data.frame(x)) {
    id <- intersect(c("ensemble", "realisation", ".ens"), names(x))
    if (length(id) == 0) {
      cli::cli_abort(c(
        "A data.frame {.arg param_sets} needs an {.field ensemble} column
         identifying each realisation.",
        "i" = "Columns present: {.val {names(x)}}."
      ))
    }
    id <- id[[1]]
    x <- split(x[, setdiff(names(x), id), drop = FALSE], x[[id]])
  }

  if (!is.list(x) || length(x) == 0) {
    cli::cli_abort("{.arg param_sets} must be a non-empty list of parameter
                   dataframes, or a long data.frame with an {.field ensemble}
                   column.")
  }

  ok <- vapply(x, function(d) is.data.frame(d) && all(req %in% names(d)),
               logical(1))
  if (!all(ok)) {
    cli::cli_abort(c(
      "Every element of {.arg param_sets} must be a data.frame with columns
       {.val {req}}.",
      "x" = "Element{?s} {.val {which(!ok)}} {?is/are} malformed."
    ))
  }

  lapply(x, function(d) {
    if (!"name_full" %in% names(d)) {
      d$name_full <- encode_param(d$group, d$name, d$index)
    }
    d
  })
}
