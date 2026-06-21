.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  if (exists(".lt_schema_cache", envir = ns)) {
    unlockBinding(".lt_schema_cache", ns)
  }
  assign(".lt_schema_cache", new.env(parent = emptyenv()), envir = ns)
}
