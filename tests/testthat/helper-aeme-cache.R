# Cache built (+ optionally run) AEME objects for the duration of one test
# session, so tests that just need "a built AEME for model X" don't each pay
# for a fresh build + model run. Keyed by (model, ext_elev, use_bgc, run),
# since those are the inputs that vary across tests using the plain/default
# setup. Cached in-memory only (not persisted to disk across sessions), so
# each `devtools::test()` / `R CMD check` run rebuilds once per combination
# actually used, instead of once per test.
#
# Not used by tests that mutate `AEME::observations()` *before* calling
# `build_aeme()` - build_aeme()'s behaviour could depend on what
# observations are present at build time, and reusing a cached build made
# without that mutation could silently produce a different result. Tests
# that only mutate observations *after* building are safe to use this,
# since that mutation is applied to a copy of the cached aeme object, in
# the exact same order as if it had just been built fresh.

.aeme_run_cache <- new.env(parent = emptyenv())

#' Get a built (and optionally run) AEME object + a fresh workspace path
#' for `model`, reusing a cached build from earlier in this test session
#' if the same (model, ext_elev, use_bgc, run) combination has already been
#' built.
#'
#' The returned `path` is always a brand new directory containing a copy
#' of the cached build's files, so tests are free to calibrate against it,
#' write new output into it, etc. without affecting the shared cache or
#' any other test that reuses the same cache entry.
#'
#' @param model character; model(s) to build/run, e.g. "glm_aed".
#' @param ext_elev,use_bgc,inf_factor,model_controls passed to
#' `AEME::build_aeme()`.
#' @param run logical; also call `AEME::run_aeme()` after building (some
#' tests only need a built, not run, aeme). Default `TRUE`.
#' @return list(aeme = <AEME object>, path = <fresh workspace path>).
get_cached_aeme_run <- function(model, ext_elev = 5, use_bgc = FALSE,
                                inf_factor = NULL, model_controls = NULL,
                                run = TRUE) {
  if (is.null(model_controls)) {
    model_controls <- AEME::get_model_controls()
  }
  key <- paste(c(model, ext_elev, use_bgc, run,
                rlang::`%||%`(inf_factor, "default")), collapse = "_")

  if (is.null(.aeme_run_cache[[key]])) {
    aeme_dir <- system.file("extdata/lake/", package = "AEME")
    aeme <- AEME::yaml_to_aeme(path = aeme_dir, "aeme.yaml")
    build_path <- tempfile("aeme_cache_build_")
    dir.create(build_path, recursive = TRUE)

    build_args <- list(path = build_path, aeme = aeme, model = model,
                       model_controls = model_controls, ext_elev = ext_elev,
                       use_bgc = use_bgc)
    if (!is.null(inf_factor)) {
      build_args$inf_factor <- inf_factor
    }
    aeme <- do.call(AEME::build_aeme, build_args)

    if (run) {
      aeme <- AEME::run_aeme(aeme = aeme, model = model, path = build_path)
    }
    .aeme_run_cache[[key]] <- list(aeme = aeme, build_path = build_path)
  }

  cached <- .aeme_run_cache[[key]]

  # Fresh copy so a test calibrating/writing into `path` can't corrupt the
  # shared cache or bleed into any other test reusing the same entry.
  path <- tempfile("aeme_run_")
  dir.create(path)
  file.copy(list.files(cached$build_path, full.names = TRUE), path,
           recursive = TRUE)

  list(aeme = cached$aeme, path = path)
}
