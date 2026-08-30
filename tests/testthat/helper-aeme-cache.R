# Cache built (+ optionally run) AEME objects for the duration of one test
# session, so tests that just need "a built AEME for model X" don't each pay
# for a fresh build + model run. Keyed by (model, ext_elev, use_bgc, run,
# vars_sim), since those are the inputs that vary across tests using the
# plain/default setup. Cached in-memory only (not persisted to disk across
# sessions), so each `devtools::test()` / `R CMD check` run rebuilds once per
# combination actually used, instead of once per test.
#
# Not used by tests that mutate `AEME::observations()` *before* calling
# `build_aeme()` - build_aeme()'s behaviour could depend on what
# observations are present at build time, and reusing a cached build made
# without that mutation could silently produce a different result. Tests
# that only mutate observations *after* building are safe to use this,
# since that mutation is applied to a copy of the cached aeme object, in
# the exact same order as if it had just been built fresh.
#
# Every call gets its own fresh copy of the cached build's workspace
# directory (see `path` below), so two tests hitting the same cache entry
# back-to-back never collide - unlike two tests both using the literal
# `tempdir()` as their build path, which *is* the same directory for the
# whole R session and will happily let one test's build/run artefacts leak
# into the next test that reuses it.

.aeme_run_cache <- new.env(parent = emptyenv())

#' Get a built (and optionally run) AEME object + a fresh workspace path
#' for `model`, reusing a cached build from earlier in this test session
#' if the same (model, ext_elev, use_bgc, run, vars_sim) combination has
#' already been built.
#'
#' The returned `path` is always a brand new directory containing a copy
#' of the cached build's files, so tests are free to calibrate against it,
#' write new output into it, etc. without affecting the shared cache or
#' any other test that reuses the same cache entry.
#'
#' @param model character; model(s) to build/run, e.g. "glm_aed".
#' @param ext_elev,use_bgc passed to `AEME::build_aeme()`.
#' @param vars_sim character vector; if supplied, applied to
#' `model_controls` via `AEME::set_vars_sim()` before building (and folded
#' into the cache key), for tests that need non-default output variables
#' switched on. Ignored if `model_controls` is supplied directly.
#' @param model_controls passed to `AEME::build_aeme()`. If `NULL` (the
#' default), built from `AEME::get_model_controls()` (+ `vars_sim` if
#' given).
#' @param run logical; also call `AEME::run_aeme()` after building (some
#' tests only need a built, not run, aeme). Default `TRUE`.
#' @return list(aeme = <AEME object>, path = <fresh workspace path>).
get_cached_aeme_run <- function(model, ext_elev = 5, use_bgc = FALSE,
                                vars_sim = NULL, model_controls = NULL,
                                run = TRUE) {
  if (is.null(model_controls)) {
    model_controls <- AEME::get_model_controls()
    if (!is.null(vars_sim)) {
      model_controls <- AEME::set_vars_sim(model_controls, vars_sim = vars_sim)
    }
  }
  key <- paste(c(model, ext_elev, use_bgc, run,
                rlang::`%||%`(paste(vars_sim, collapse = ","), "default")),
              collapse = "_")

  if (is.null(.aeme_run_cache[[key]])) {
    aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
    aeme <- readRDS(aeme_file)
    build_path <- tempfile("aeme_cache_build_")
    dir.create(build_path, recursive = TRUE)

    aeme <- AEME::build_aeme(path = build_path, aeme = aeme, model = model,
                             model_controls = model_controls,
                             ext_elev = ext_elev, use_bgc = use_bgc)

    if (run) {
      aeme <- AEME::run_aeme(aeme = aeme, model = model, path = build_path)
    }
    .aeme_run_cache[[key]] <- list(aeme = aeme, build_path = build_path)
  }

  cached <- .aeme_run_cache[[key]]

  # Fresh copy so a test calibrating/writing into `path` can't corrupt the
  # shared cache or bleed into any other test reusing the same entry. Always
  # a copy - `build_path` only exists on the branch above, so returning it
  # errored on every cache hit, and handing back the shared directory would
  # defeat the isolation this copy is here to provide.
  new_path <- tempfile("aeme_run_")
  dir.create(new_path)
  file.copy(list.files(cached$build_path, full.names = TRUE), new_path,
           recursive = TRUE)

  list(aeme = cached$aeme, path = new_path)
}
