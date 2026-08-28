# Fixtures shared by the PEST++ test files. Lives in a helper- file so that
# either test file can be run on its own with a testthat filter; defined
# inside one test file, it is invisible to the other.

make_obs_tbl <- function() {
  ot <- data.frame(
    obsnme = sprintf("o%06d", 1:3),
    obsval = c(12.1, 13.4, 9.8),
    weight = 0.4,
    obgnme = "hyd_temp",
    stringsAsFactors = FALSE
  )
  attr(ot, "map") <- data.frame(
    obsnme = ot$obsnme,
    var_aeme = "HYD_temp",
    Date = as.Date(c("2020-01-01", "2020-01-01", "2020-02-01")),
    depth = c(0.5, 5.0, 0.5),
    stringsAsFactors = FALSE
  )
  ot
}

# Stand in for PEST's template substitution: read a .tpl, overwrite each
# ~name~ field with the supplied value, write the model input file. This is
# what lets the tests verify the tpl/forward-run contract without a PEST++
# binary. (Also defined in test-calib_aeme-pest.R for historical reasons;
# the definitions are identical.)
fake_pest_write_pars <- function(tpl, out, values) {
  l <- readLines(tpl)
  stopifnot(identical(l[1], "ptf ~"))
  body <- l[-1]
  for (nm in names(values)) {
    pat <- paste0("~\\s*", nm, "\\s*~")
    body <- sub(pat, formatC(values[[nm]], format = "g", digits = 10), body)
  }
  writeLines(body, out)
  out
}
