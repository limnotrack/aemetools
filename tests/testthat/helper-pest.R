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

# A two-parameter calibration set: one log/indexed (Kw[1]), one plain (ce).
pest_param <- function() {
  data.frame(
    model = "glm_aed", file = "glm4.nml",
    group = c("light", "light"), name = c("Kw", "ce"),
    index = c(1L, NA_integer_), value = c(0.5, 0.0013),
    min = c(0.1, 0.0005), max = c(1.5, 0.005), log = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
}

# Build a synthetic PEST++ run directory with a prior (iteration 0) and
# posterior (iteration 2) parameter and observation ensemble, plus phi
# files. The posterior is deliberately much narrower for p001 and identical
# in width for p002, so variance reduction has a known answer.
make_pest_dir <- function(env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  ctrl <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1,
                              noptmax = 2)
  param <- pest_param()
  pt <- pest_param_table(param)
  ot <- make_obs_tbl()
  write_pest_tpl(pt, ctrl)
  write_pest_ins(ot, ctrl)
  write_pst(pt, ot, ctrl, stats::setNames("aeme_pars.csv", "aeme_pars.csv.tpl"),
            stats::setNames("aeme_sim.out", "aeme_sim.ins"), "cmd")

  reals <- c("real_0", "real_1", "real_2", "base")
  write_ens <- function(it, p1, p2) {
    utils::write.csv(
      data.frame(real_name = reals, p001 = p1, p002 = p2),
      file.path(d, paste0("aeme.", it, ".par.csv")), row.names = FALSE)
  }
  # prior: p001 sd is large; posterior: p001 sd is a tenth of it.
  write_ens(0, c(0.2, 0.6, 1.0, 1.4), c(0.001, 0.002, 0.003, 0.004))
  write_ens(2, c(0.68, 0.72, 0.76, 0.80), c(0.001, 0.002, 0.003, 0.004))

  # Observation ensembles, matching the three obs in make_obs_tbl().
  for (it in c(0, 2)) {
    off <- if (it == 0) 3 else 0.2
    utils::write.csv(
      data.frame(real_name = reals,
                 o000001 = 12.1 + off * c(-1, 0, 1, 0.5),
                 o000002 = 13.4 + off * c(1, -1, 0, -0.5),
                 o000003 = 9.8 + off * c(0, 1, -1, 0.5)),
      file.path(d, paste0("aeme.", it, ".obs.csv")), row.names = FALSE)
  }

  writeLines(c(
    "iteration,total_runs,mean,standard_deviation,min,max,real_0,real_1,real_2,base",
    "0,4,12.0,2.0,8.0,20.0,8.0,12.0,20.0,10.0",
    "1,20,6.0,1.0,3.0,9.0,3.0,6.0,9.0,5.0",
    "2,36,2.0,0.5,1.0,3.0,1.0,2.0,3.0,1.5"
  ), file.path(d, "aeme.phi.actual.csv"))

  # Per-group phi trajectory: one group ("hyd_temp"), one row per iteration.
  writeLines(c(
    "iteration,total_runs,hyd_temp",
    "0,4,12.0",
    "1,20,6.0",
    "2,36,2.0"
  ), file.path(d, "aeme.phi.group.csv"))

  list(d = d, ctrl = ctrl, param = param, pt = pt, ot = ot)
}
