# Tests for the PEST++ calibration engine.
#
# The interface tests below deliberately do NOT require PEST++ to be
# installed and do NOT run AEME: they exercise the file-writing, the
# parameter round-trip and the results import against a stand-in for PEST
# that performs exactly the substitution PEST would. That keeps the bulk of
# the coverage fast and available in CI. The one test that actually solves
# is skipped unless `have_pest()` finds a binary.

# Stand in for PEST's template substitution: read a .tpl, overwrite each
# ~name~ field with the supplied value, write the model input file. This is
# what lets the tests below verify the tpl/forward-run contract without a
# PEST++ binary.
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

make_param <- function() {
  data.frame(
    model = "glm_aed",
    file = c("glm4.nml", "glm4.nml", "met"),
    group = c("light", "light", NA),
    name = c("Kw", "ce", "MET_tmpair"),
    index = c(1, NA, NA),
    value = c(0.5, 0.0013, 1.0),
    min = c(0.1, -0.001, 0.8),
    max = c(1.5, 0.005, 1.2),
    log = c(TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
}

test_that("pest_param_table maps names and respects PEST transform rules", {
  pt <- pest_param_table(make_param())

  expect_equal(pt$parnme, c("p001", "p002", "p003"))
  # Kw has min > 0 and log = TRUE, so log transform is kept.
  expect_equal(pt$partrans, c("log", "none", "none"))
  # ce's bounds straddle zero, so factor change limits are invalid.
  expect_equal(pt$parchglim, c("factor", "relative", "factor"))

  map <- attr(pt, "map")
  expect_equal(map$name_full,
               encode_param(make_param()$group, make_param()$name,
                            make_param()$index))
})

test_that("pest_param_table drops a log transform that PEST cannot honour", {
  p <- make_param()
  p$log[2] <- TRUE  # ce has a negative lower bound
  expect_warning(pt <- pest_param_table(p), NA)  # warns via cli, not warning()
  expect_equal(pt$partrans[2], "none")
})

test_that("pest_param_table rejects degenerate bounds", {
  p <- make_param()
  p$max[1] <- p$min[1]
  expect_error(pest_param_table(p), "min.*max|violated")
})

test_that("template and instruction files match the forward-run contract", {
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1)
  pt <- pest_param_table(make_param())
  ot <- make_obs_tbl()

  tpl <- write_pest_tpl(pt, ctrl)
  ins <- write_pest_ins(ot, ctrl)

  tl <- readLines(file.path(d, names(tpl)))
  expect_equal(tl[1], "ptf ~")
  expect_equal(tl[2], "parnme,value")
  # Every field is exactly `width` characters wide, delimiters included.
  fields <- regmatches(tl[-(1:2)], regexpr("~.*~", tl[-(1:2)]))
  expect_true(all(nchar(fields) == 23L))

  il <- readLines(file.path(d, names(ins)))
  expect_equal(il[1], "pif ~")
  # `w` skips the obsnme column so PEST reads the value, not the name.
  expect_equal(il[-1], paste0("l1 w !", ot$obsnme, "!"))
  expect_length(il, nrow(ot) + 1L)
})

test_that("write_pst emits a well-formed control file", {
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(pest_dir = d, case = "aeme", noptmax = 4,
                              ies_num_reals = 30, ncore = 1)
  pt <- pest_param_table(make_param())
  ot <- make_obs_tbl()

  pst <- write_pst(pt, ot, ctrl, write_pest_tpl(pt, ctrl),
                   write_pest_ins(ot, ctrl), "Rscript forward_run.R")
  l <- readLines(pst)

  expect_equal(l[1], "pcf")
  expect_equal(l[3], "restart estimation")
  # NPAR NOBS NPARGP NPRIOR NOBSGP. Two parameter groups: "light", plus
  # "misc" for the met multiplier, which has no aemetools group.
  expect_equal(l[4], "3 3 2 0 1")
  expect_equal(pt$pargp, c("light", "light", "misc"))
  # One `* parameter groups` line per group, carrying the FD settings.
  gi <- which(l == "* parameter groups")
  expect_equal(l[gi + 1:2], c("light relative 0.01 1e-06 switch 2.0 parabolic",
                              "misc relative 0.01 1e-06 switch 2.0 parabolic"))
  # NTPLFLE NINSFLE
  expect_true(startsWith(l[5], "1 1 "))
  expect_true(any(grepl("^4 0\\.005", l)))            # noptmax
  expect_true(any(grepl("^\\+\\+ies_num_reals\\(30\\)$", l)))

  # Section order matters to PEST's parser.
  secs <- grep("^\\* ", l, value = TRUE)
  expect_equal(secs, c("* control data", "* parameter groups",
                       "* parameter data", "* observation groups",
                       "* observation data", "* model command line",
                       "* model input/output"))

  # The name maps are what lets results be translated back afterwards.
  expect_true(file.exists(file.path(d, "aeme_par_map.csv")))
  expect_true(file.exists(file.path(d, "aeme_obs_map.csv")))
})

test_that("prior information switches the run to regularisation mode", {
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1)
  pt <- pest_param_table(make_param())
  ot <- make_obs_tbl()
  pst <- write_pst(pt, ot, ctrl, write_pest_tpl(pt, ctrl),
                   write_pest_ins(ot, ctrl), "cmd",
                   prior_info = "pi1 1.0 * log(p001) = -0.301 1.0 regul")
  l <- readLines(pst)
  expect_equal(l[3], "restart regularization")
  expect_equal(l[4], "3 3 2 1 1")
  expect_true("* prior information" %in% l)
})

test_that("PEST's template substitution round-trips into param values", {
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1)
  param <- make_param()
  pt <- pest_param_table(param)
  tpl <- write_pest_tpl(pt, ctrl)

  new_vals <- list(p001 = 0.77, p002 = 0.0021, p003 = 1.15)
  csv <- fake_pest_write_pars(file.path(d, names(tpl)),
                              file.path(d, unname(tpl)), new_vals)

  pars <- read.csv(csv, stringsAsFactors = FALSE)
  expect_equal(pars$value, unlist(new_vals), ignore_attr = TRUE)

  spliced <- aemetools:::.pest_apply_params(param, pars, attr(pt, "map"))
  expect_equal(spliced$value, c(0.77, 0.0021, 1.15))
  # The met multiplier is carried through the same path as config params.
  expect_equal(spliced$value[spliced$file == "met"], 1.15)
})

test_that("parameters are matched by name, not row order", {
  param <- make_param()
  pt <- pest_param_table(param)
  pars <- data.frame(parnme = c("p003", "p001", "p002"),
                     value = c(1.15, 0.77, 0.0021))
  spliced <- aemetools:::.pest_apply_params(param, pars, attr(pt, "map"))
  expect_equal(spliced$value, c(0.77, 0.0021, 1.15))
})

test_that("a missing parameter value is an error, not a silent default", {
  param <- make_param()
  pt <- pest_param_table(param)
  pars <- data.frame(parnme = c("p001", "p002"), value = c(0.77, 0.0021))
  expect_error(aemetools:::.pest_apply_params(param, pars, attr(pt, "map")),
               "missing values")
})

test_that("a failed forward run writes no output file", {
  # PEST++ has its own failed-run handling: PANTHER marks the run failed,
  # retries up to max_run_fail, and EnsembleMethod drops that realisation.
  # Verified against pestpp-ies 5.2.16 - of six realisations with one
  # crashing, the prior observation ensemble came back with five and the
  # run completed normally. Writing a penalty value instead would defeat
  # that: PEST cannot tell a sentinel from a simulated value, so the
  # realisation would survive into the ensemble statistics and the
  # covariance that computes the parameter update.
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1,
                              na_value = 999)
  param <- make_param()
  pt <- pest_param_table(param)
  ot <- make_obs_tbl()
  tpl <- write_pest_tpl(pt, ctrl)

  # Payload with no aeme object, so run_and_fit() cannot succeed - standing
  # in for a crashed or timed-out model run.
  saveRDS(list(case = "aeme", obsnme = ot$obsnme, na_value = 999,
               par_map = attr(pt, "map"), obs_map = attr(ot, "map"),
               param = param, obj_mode = "residual", vars_sim = "HYD_temp",
               log_dir = file.path(d, "runlog")),
          file.path(d, "aeme_fwd.rds"))

  fake_pest_write_pars(file.path(d, names(tpl)), file.path(d, unname(tpl)),
                       list(p001 = 0.77, p002 = 0.0021, p003 = 1.15))

  # A stale result from a previous evaluation must not be left behind for
  # PEST to read as though it were this run's.
  writeLines("o000001 1", file.path(d, "aeme_sim.out"))
  withr::with_dir(d, suppressMessages(pest_forward_run("aeme_fwd.rds")))
  expect_false(file.exists(file.path(d, "aeme_sim.out")))

  # The run is still recorded, so it reaches the database as a failure
  # rather than vanishing.
  logs <- list.files(file.path(d, "runlog"), full.names = TRUE)
  expect_length(logs, 1L)
  logged <- read.csv(logs[[1]], check.names = FALSE)
  expect_equal(nrow(logged), 1L)
  expect_equal(logged$fit, 999)
})
test_that("read_pest_results imports the run log into the results shape", {
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1,
                              na_value = 999)
  param <- make_param()
  param$name_full <- encode_param(param$group, param$name, param$index)
  pt <- pest_param_table(param)
  ot <- make_obs_tbl()
  write_pest_tpl(pt, ctrl)

  saveRDS(list(case = "aeme", obsnme = ot$obsnme, na_value = 999,
               par_map = attr(pt, "map"), obs_map = attr(ot, "map"),
               param = param, obj_mode = "residual", vars_sim = "HYD_temp",
               log_dir = file.path(d, "runlog")),
          file.path(d, "aeme_fwd.rds"))

  # Two evaluations, written from two different "agents".
  for (v in list(list(p001 = 0.77, p002 = 0.0021, p003 = 1.15),
                 list(p001 = 0.30, p002 = 0.0040, p003 = 0.95))) {
    fake_pest_write_pars(file.path(d, "aeme_pars.csv.tpl"),
                         file.path(d, "aeme_pars.csv"), v)
    withr::with_dir(d, suppressMessages(pest_forward_run("aeme_fwd.rds")))
  }

  res <- read_pest_results(ctrl = ctrl, param = param, vars_sim = "HYD_temp")

  expect_equal(nrow(res), 2L)
  expect_equal(names(res), c(param$name_full, "HYD_temp", "fit", "gen"))
  expect_equal(res[[param$name_full[1]]], c(0.77, 0.30))
  # No per-iteration ensemble files were written, so every run is gen 1.
  expect_equal(res$gen, c(1L, 1L))
  # A failed run is recorded at the penalty value, not dropped.
  expect_true(all(res$fit == 999))
})

test_that("the generated forward-run script is valid R", {
  # Regression: .libPaths() deparsed to a multi-element vector, which
  # sprintf() vectorised into two syntactically broken lines. The script
  # then failed to parse, wrote no output, and PEST++ reported only
  # "all realizations failed during initial evaluation".
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1)
  param <- make_param()
  pt <- pest_param_table(param)
  ot <- make_obs_tbl()

  script <- suppressMessages(write_pest_forward_run(
    aeme = NULL, param = param, par_tbl = pt, obs_tbl = ot,
    model = "glm_aed", vars_sim = "HYD_temp",
    FUN_list = list(HYD_temp = function(df) 1),
    weights = set_weights("HYD_temp"), ctrl = ctrl
  ))

  expect_true(file.exists(script))
  expect_no_error(parse(script))

  l <- readLines(script)
  lp <- grep("^\\.libPaths\\(", l, value = TRUE)
  expect_length(lp, 1L)
  # Every library path must survive onto that single line.
  for (p in .libPaths()) expect_true(grepl(p, lp, fixed = TRUE))
  # And the script must actually run in a fresh process.
  expect_no_error(parse(text = paste(l, collapse = "\n")))
})

test_that("fatal-error detection ignores the options block PEST++ echoes", {
  # Regression: a loose "error|failed" pattern matched the echoed option
  # name `panther_agent_restart_on_error` near the top of every record and
  # aborted healthy runs on the first poll.
  healthy <- c("pestpp-ies version 5.2.16", "panther_agent_restart_on_error: 0",
               "ies_bad_phi: 1e+300", "ies_no_noise: false",
               "...running initial ensemble of size 6")
  expect_length(grep(aemetools:::.pest_fatal_pattern, healthy,
                     ignore.case = TRUE), 0L)

  # ...while still catching the failure that started this. throw_em_error()
  # formats every one of its ~90 call sites as "<alg_tag> error: <message>",
  # and alg_tag varies by solver, so the detection must not depend on the
  # tag or the message.
  for (fatal in c(
    "    EnsembleMethod error: all realizations failed during initial evaluation",
    "    IES error: control file parameter value run failed",
    "    da error: error in observation ensemble: ",
    "Error processing control file: aeme.pst",
    "Model run failed.  No results were recorded."
  )) {
    expect_match(fatal, aemetools:::.pest_fatal_pattern)
  }
})

test_that(".pest_spawn tracks a process and its real exit status", {
  # The solver runs detached; .pest_wait() distinguishes "still running"
  # from "died" via the live processx handle, and reads the true exit code
  # from it - no sentinel file, no shell wrapper.
  skip_on_cran()
  rscript <- file.path(R.home("bin"), "Rscript")

  for (status in c(0L, 3L)) {
    d <- withr::local_tempdir()
    p <- aemetools:::.pest_spawn(rscript,
                                 c("-e", sprintf("quit(status=%d)", status)),
                                 d, "run_master")
    p$wait(timeout = 60000)
    expect_false(p$is_alive())
    expect_equal(p$get_exit_status(), status)
  }
})

test_that(".pest_wait returns on success and aborts on a non-zero exit", {
  skip_on_cran()
  rscript <- file.path(R.home("bin"), "Rscript")

  ok <- withr::local_tempdir()
  procs_ok <- aemetools:::.pest_procs()
  procs_ok$master <- aemetools:::.pest_spawn(rscript, c("-e", "invisible(1)"),
                                             ok, "run_master")
  ctrl_ok <- create_pest_control(pest_dir = ok, case = "aeme", ncore = 1,
                                 solver_timeout = 60)
  expect_no_error(aemetools:::.pest_wait(procs_ok, ok, "aeme.pst", ctrl_ok,
                                         poll = 1))

  bad <- withr::local_tempdir()
  procs_bad <- aemetools:::.pest_procs()
  procs_bad$master <- aemetools:::.pest_spawn(rscript, c("-e", "quit(status=3)"),
                                              bad, "run_master")
  procs_bad$master$wait(timeout = 30000)
  writeLines(c("   ************   ",
               "    EnsembleMethod error: all realizations failed during initial evaluation"),
             file.path(bad, "aeme.rec"))
  ctrl_bad <- create_pest_control(pest_dir = bad, case = "aeme", ncore = 1,
                                  solver_timeout = 60)
  # The abort must carry the reason from the record, not just the status.
  expect_error(aemetools:::.pest_wait(procs_bad, bad, "aeme.pst", ctrl_bad,
                                      poll = 1),
               "all realizations failed")
})

test_that("calibration metadata is writable for a control lacking search knobs", {
  # Regression: VTR/NP/ngen/itermax/reltol/cutoff/mutate describe the
  # built-in generational search and are absent from a PEST++ control.
  # data.frame() turned those NULLs into zero-length columns and aborted
  # with "differing number of rows", after the solve had already succeeded.
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(file_dir = d, ncore = 2, file_type = "csv")
  ctrl$sim_id <- "LID1_glmaed_C_001"

  expect_no_error(
    aemetools:::write_calib_metadata(ctrl = ctrl, nsim = 6, t0 = Sys.time() - 10)
  )
  m <- read.csv(file.path(d, "calibration_metadata.csv"))
  expect_equal(nrow(m), 1L)
  # Fields describing the built-in generational search are recorded as NA,
  # keeping one schema across methods. `itermax` is deliberately not among
  # them: it means the model-run budget, which a PEST control does know.
  expect_true(all(is.na(m[, c("VTR", "NP", "ngen", "reltol",
                              "cutoff", "mutate")])))
  expect_false(is.na(m$itermax))
  # Fields the PEST control does have must still be real values.
  expect_equal(m$n_sim, 6)
  expect_equal(m$ncore, 2)
  expect_equal(m$c_method, "PESTPP-IES")
})

test_that(".pest_cleanup kills running process trees and spares finished ones", {
  skip_on_cran()
  rscript <- file.path(R.home("bin"), "Rscript")

  procs <- aemetools:::.pest_procs()
  fin <- withr::local_tempdir()
  orph <- withr::local_tempdir()

  # A master that has already exited must not be touched.
  procs$master <- aemetools:::.pest_spawn(rscript, c("-e", "invisible(1)"),
                                          fin, "run_master")
  procs$master$wait(timeout = 30000)

  # An agent still running - what an abort in .pest_wait() leaves behind -
  # must be stopped, tree and all, or it keeps holding the PANTHER port.
  procs$agents[[1]] <- aemetools:::.pest_spawn(rscript, c("-e", "Sys.sleep(600)"),
                                               orph, "run_agent")
  Sys.sleep(1)
  expect_true(procs$agents[[1]]$is_alive())

  # setup.R sets AEME.inform = FALSE, which no-ops AEME::cli_safe() and so
  # never renders the "Stopped {n} process tree{?s}" message. Force it on:
  # the cli plural markup needs a quantity or it aborts with
  # "Cannot pluralize without a quantity" (killed > 0 only, i.e. a parallel
  # run with a lingering agent).
  withr::local_options(AEME.inform = TRUE)
  expect_equal(aemetools:::.pest_cleanup(procs), 1L)
  expect_false(procs$agents[[1]]$is_alive())
})

test_that(".pest_free_port honours a free port and steps past a busy one", {
  # Find a port we can actually hold for the duration of the test.
  held <- NULL
  for (p in 45000:45100) {
    held <- tryCatch(serverSocket(p), error = function(e) NULL)
    if (!is.null(held)) { busy <- p; break }
  }
  skip_if(is.null(held), "no bindable port in the test range")
  on.exit(close(held), add = TRUE)

  # A free preferred port comes back unchanged...
  expect_equal(aemetools:::.pest_free_port(busy + 1L), busy + 1L)

  # ...but a held one is stepped past, to a port that is genuinely free.
  got <- aemetools:::.pest_free_port(busy)
  expect_false(identical(got, busy))
  probe <- serverSocket(got)
  close(probe)
})

test_that("solver progress is read from the phi CSV", {
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1,
                              noptmax = 4)
  phi <- file.path(d, "aeme.phi.actual.csv")
  # Header per L2PhiHandler::prepare_csv() in the PEST++ sources.
  hdr <- "iteration,total_runs,mean,standard_deviation,min,max,real_0,real_1"

  expect_null(aemetools:::.pest_status(d, "aeme.pst", ctrl))

  writeLines(c(hdr, "0,6,12.3456,2.1,8.7654,20.1,8.7654,12.0",
               "1,18,6.5432,1.4,3.21098,9.9,3.21098,7.1"), phi)
  s <- aemetools:::.pest_status(d, "aeme.pst", ctrl)
  # Latest row, not the first.
  expect_match(s, "iteration 1/4")
  expect_match(s, "18 model runs")
  expect_match(s, "best phi 3.211")

  # noptmax <= 0 are the single-run / FOSM / prior-ensemble modes, where a
  # "of M" denominator would be meaningless.
  ctrl0 <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1,
                               noptmax = -1)
  expect_match(aemetools:::.pest_status(d, "aeme.pst", ctrl0), "iteration 1\\b")
  expect_no_match(aemetools:::.pest_status(d, "aeme.pst", ctrl0), "/-1")

  # The solver appends to this file while we read it, so a torn or
  # unparseable read must yield NULL and leave the last status standing,
  # never error out of the wait loop.
  writeLines(c(hdr, "0,6,12.3"), phi)
  expect_no_error(aemetools:::.pest_status(d, "aeme.pst", ctrl))
  writeLines("not a csv at all", phi)
  expect_null(aemetools:::.pest_status(d, "aeme.pst", ctrl))

  # Braces would be evaluated as cli glue by the caller.
  writeLines(c(hdr, "0,6,1,1,1,1,1,1"), phi)
  expect_no_match(aemetools:::.pest_status(d, "aeme.pst", ctrl), "[{}]")
})

test_that("progress is reported once per change, and respects AEME.inform", {
  skip_on_cran()
  rscript <- file.path(R.home("bin"), "Rscript")
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1,
                              noptmax = 4, solver_timeout = 60)
  hdr <- "iteration,total_runs,mean,standard_deviation,min,max,real_0"
  # A single, unchanging phi row: .pest_wait() polls many times over the
  # ~6s master, but .pest_status() returns the same string every time, so
  # exactly one message should be emitted rather than one per poll.
  writeLines(c(hdr, "0,6,12.3,2.1,8.76,20.1,8.76"),
             file.path(d, "aeme.phi.actual.csv"))

  spawn_master <- function(secs) {
    procs <- aemetools:::.pest_procs()
    procs$master <- aemetools:::.pest_spawn(
      rscript, c("-e", sprintf("Sys.sleep(%d)", secs)), d, "run_master")
    procs
  }

  # setup.R sets AEME.inform = FALSE for the whole test session, so turn it
  # back on explicitly here - otherwise the test could never see a message
  # and the AEME.inform = FALSE assertion below would pass vacuously.
  msgs <- withr::with_options(
    list(AEME.inform = TRUE),
    testthat::capture_messages(
      aemetools:::.pest_wait(spawn_master(6), d, "aeme.pst", ctrl, poll = 1))
  )
  expect_length(grep("model runs", msgs), 1L)

  # AEME.inform = FALSE silences it, like every other message in the package.
  quiet <- withr::with_options(
    list(AEME.inform = FALSE),
    testthat::capture_messages(
      aemetools:::.pest_wait(spawn_master(4), d, "aeme.pst", ctrl, poll = 1))
  )
  expect_length(grep("model runs", quiet), 0L)
})

test_that("preflight catches a broken forward run before the solver starts", {
  skip_on_cran()

  # Stand in for forward_run.R. `body` decides what it writes, so each
  # failure mode PEST++ would otherwise report only as "all realizations
  # failed during initial evaluation" can be provoked directly.
  setup_preflight <- function(body) {
    d <- withr::local_tempdir(.local_envir = parent.frame())
    ctrl <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1,
                                na_value = 999)
    pt <- pest_param_table(make_param())
    ot <- make_obs_tbl()
    tpl <- write_pest_tpl(pt, ctrl)
    ins <- write_pest_ins(ot, ctrl)
    writeLines(body, file.path(d, "forward_run.R"))
    list(d = d, ctrl = ctrl, pt = pt, ot = ot, tpl = tpl, ins = ins)
  }

  ok <- setup_preflight(c(
    "dir.create('runlog', showWarnings = FALSE)",
    "write.csv(data.frame(a = 1), 'runlog/runlog_1.csv', row.names = FALSE)",
    "writeLines(c('o000001 12.0', 'o000002 13.0', 'o000003 9.5'),",
    "           'aeme_sim.out')"
  ))
  expect_true(aemetools:::.pest_preflight(ok$ctrl, ok$pt, ok$ot, ok$tpl,
                                          ok$ins))
  # PEST is handed the initial values, read back from the template.
  pars <- read.csv(file.path(ok$d, unname(ok$tpl)))
  expect_equal(pars$value, ok$pt$parval1)
  # The preflight's own run must not survive into the imported results.
  expect_false(dir.exists(file.path(ok$d, "runlog")))

  # No output file at all - what a script that fails to parse produces.
  bad1 <- setup_preflight("stop('boom')")
  expect_error(aemetools:::.pest_preflight(bad1$ctrl, bad1$pt, bad1$ot,
                                           bad1$tpl, bad1$ins),
               "wrote no output file")

  # Short file: PEST++ aborts the iteration on this.
  bad2 <- setup_preflight("writeLines('o000001 12.0', 'aeme_sim.out')")
  expect_error(aemetools:::.pest_preflight(bad2$ctrl, bad2$pt, bad2$ot,
                                           bad2$tpl, bad2$ins),
               "wrote 1 value")

  # A run that fails now writes nothing, which the missing-file check
  # above catches - there is no longer a sentinel-filled file to detect.
  bad3 <- setup_preflight("invisible(NULL)")
  expect_error(aemetools:::.pest_preflight(bad3$ctrl, bad3$pt, bad3$ot,
                                           bad3$tpl, bad3$ins),
               "wrote no output file")
})

test_that("a partially simulated run is failed, not padded", {
  # The instruction file demands a value for every observation, so there is
  # no way to write "missing": anything put there is read as a simulated
  # value. Padding with the observed value would flatter the fit; padding
  # with a sentinel would corrupt the ensemble statistics. Both are silent,
  # so the run is failed instead and PEST++ drops the realisation.
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1,
                              na_value = 999)
  ot <- make_obs_tbl()
  map <- attr(ot, "map")

  # run_and_fit() returns two of the three observations.
  comp <- data.frame(var_aeme = "HYD_temp",
                     Date = map$Date[1:2], depth = map$depth[1:2],
                     model = c(12.0, 13.0), obs = c(12.1, 13.4),
                     stringsAsFactors = FALSE)
  p <- list(obs_map = map, vars_sim = "HYD_temp", na_value = 999,
            weights = c(HYD_temp = 1),
            FUN_list = list(HYD_temp = function(df) 1))

  local_mocked_bindings(run_and_fit = function(...) comp, .package = "aemetools")
  expect_message(out <- aemetools:::.pest_run_residual(p, param = NULL,
                                                       path = "."),
                 "no simulated equivalent")
  expect_null(out)

  # All three present: the run succeeds.
  comp_all <- data.frame(var_aeme = "HYD_temp", Date = map$Date,
                         depth = map$depth, model = c(12, 13, 9.5),
                         obs = ot$obsval, stringsAsFactors = FALSE)
  local_mocked_bindings(run_and_fit = function(...) comp_all, .package = "aemetools")
  out2 <- aemetools:::.pest_run_residual(p, param = NULL, path = ".")
  expect_equal(as.numeric(out2), c(12, 13, 9.5))
})

test_that("itermax reports the expected model-run budget", {
  # Per EnsembleMethod::solve(): prior ensemble of ies_num_reals, then each
  # iteration costs (n_lambda * n_scale * subset) + (num_reals - subset).
  # Defaults are subset = 10% of the ensemble (floor 4), three lambda
  # multipliers {0.1, 1, 10} and three scale factors {0.75, 1.0, 1.1}.
  d <- create_pest_control(ncore = 1)                      # 50 reals, 6 iter
  expect_equal(d$itermax, 50 + 6 * (3 * 3 * 5 + 45))

  # ies_include_base does not enlarge the ensemble: add_bases() drops the
  # last realisation before appending "base".
  expect_equal(d$ies_num_reals, 50)

  # Small ensembles hit the subset floor of 4, which is why they are
  # inefficient: lambda testing costs more runs than there are realisations.
  small <- create_pest_control(ncore = 1, ies_num_reals = 6, noptmax = 1)
  expect_equal(small$itermax, 6 + (3 * 3 * 4 + 2))
  # Pinned against a real run: this configuration was observed to make
  # exactly 44 model runs (6 initial, 36 lambda-test, 2 remainder).
  expect_equal(small$itermax, 44)

  # Overrides passed through pestpp_options must be honoured.
  expect_equal(
    create_pest_control(ncore = 1, ies_num_reals = 40, noptmax = 2,
                        pestpp_options = list(ies_subset_size = 10))$itermax,
    40 + 2 * (3 * 3 * 10 + 30))
  # Cutting to a single lambda multiplier is the main lever on run count.
  expect_equal(
    create_pest_control(ncore = 1, ies_num_reals = 50, noptmax = 1,
                        pestpp_options = list(ies_lambda_mults = 1))$itermax,
    50 + (1 * 3 * 5 + 45))

  # noptmax 0 is a single run; negative values are the FOSM /
  # prior-ensemble-only modes, whose cost the control cannot determine.
  expect_equal(create_pest_control(ncore = 1, noptmax = 0)$itermax, 1)
  expect_true(is.na(create_pest_control(ncore = 1, noptmax = -1)$itermax))

  # glm is driven by the Jacobian, so it needs the parameter count.
  glm <- create_pest_control(ncore = 1, exe = "pestpp-glm", noptmax = 5)
  expect_true(is.na(glm$itermax))
  expect_equal(pest_expected_runs(glm, n_par = 12), 5 * 13)

  # swp sweeps a user-supplied table that the control knows nothing about.
  expect_true(is.na(create_pest_control(ncore = 1,
                                        exe = "pestpp-swp")$itermax))
})

test_that("itermax reaches the calibration metadata", {
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(file_dir = d, ncore = 1, file_type = "csv")
  ctrl$sim_id <- "LID1_glmaed_C_001"
  aemetools:::write_calib_metadata(ctrl = ctrl, nsim = 6, t0 = Sys.time() - 5)

  m <- read.csv(file.path(d, "calibration_metadata.csv"))
  expect_equal(m$itermax, 50 + 6 * (3 * 3 * 5 + 45))
  # The generational knobs remain NA - they describe the built-in search.
  expect_true(is.na(m$NP))
})

test_that("bad ++ option names are caught before the solver is launched", {
  # PEST++ parses ++ options with forgive_unknown_args false, so one wrong
  # name aborts the run - after the control file is written and the solver
  # started. Worse, the name it prints back when rejecting the lambda
  # multipliers is its internal member name (ies_lam_mults), not the
  # keyword the parser accepts (ies_lambda_mults), so the error sends you
  # straight back to the same wrong name.
  expect_error(
    create_pest_control(ncore = 1, pestpp_options = list(ies_lam_mults = 1)),
    "ies_lambda_mults")
  expect_error(
    create_pest_control(ncore = 1, pestpp_options = list(ies_subset = 4)),
    "ies_subset_size")

  # The correct names are accepted and reach the control file.
  ctrl <- create_pest_control(ncore = 1, ies_num_reals = 6, noptmax = 1,
                              pestpp_options = list(ies_lambda_mults = 1,
                                                    lambda_scale_fac = 1))
  pp <- aemetools:::.pest_plusplus_lines(ctrl)
  expect_true("++ies_lambda_mults(1)" %in% pp)
  expect_true("++lambda_scale_fac(1)" %in% pp)

  # ...and are honoured by the run-budget estimate, which reads the same
  # keyword. 6 initial + (1 lambda x 1 scale x 4 subset) + 2 remainder.
  expect_equal(ctrl$itermax, 12)
})

test_that("a solver error message carries the line naming the cause", {
  # "the following '++' args were not accepted:" is useless on its own -
  # the detail is on the next line.
  d <- withr::local_tempdir()
  rec <- file.path(d, "aeme.rec")
  writeLines(c(
    "preamble",
    "control file parsing error:  the following '++' args were not accepted:",
    "ies_lam_mults,",
    "forgive_unknown_args is 'false' so this is treated as an error",
    "",
    "this line is past the blank and must not be included"), rec)

  msg <- aemetools:::.pest_fatal_reason(rec)
  expect_match(msg, "not accepted")
  expect_match(msg, "ies_lam_mults")
  expect_no_match(msg, "past the blank")

  # No fatal line at all still returns NULL.
  writeLines("all is well", rec)
  expect_null(aemetools:::.pest_fatal_reason(rec))
})

test_that("read_pest_results returns nothing when no runs completed", {
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1)
  expect_equal(nrow(read_pest_results(ctrl, make_param(), "HYD_temp")), 0L)
})

test_that("control-object guards catch incompatible combinations", {
  # A finite-difference Jacobian over one aggregated fit value is useless.
  expect_error(create_pest_control(exe = "pestpp-glm", obj_mode = "fit"),
               "not compatible")
  expect_error(create_pest_control(exe = "pestpp-nope"), "must be one of")
  expect_error(create_pest_control(pestpp_options = list(1, 2)), "named list")

  ctrl <- create_pest_control(ncore = 1)
  expect_s3_class(ctrl, "calib_sa_control")
  # A PEST++ run is a calibration run with a different search engine. The
  # rest of the package dispatches on `method`, so giving PEST its own
  # value made read_simulation_output() skip calibration_metadata and
  # write_simulation_output() stem the sim_id as "S" (sensitivity).
  expect_equal(ctrl$method, "calib")
  expect_equal(ctrl$engine, "pest")
  expect_equal(ctrl$c_method, "PESTPP-IES")
  # `parallel` alone selects PANTHER vs serial. A separate `panther` flag
  # was duplication: PEST++ parallelises only through a run manager, so
  # there is no "parallel but not PANTHER" mode, and two booleans could be
  # set to contradict each other with the control then misreporting itself.
  expect_true(ctrl$parallel)
  expect_null(ctrl$panther)
  expect_false(create_pest_control(ncore = 1, parallel = FALSE)$parallel)
})

test_that("a PEST control loads the same result tables as a built-in run", {
  # Regression for the dispatch above: exercise the branch in
  # read_simulation_output() that decides which metadata tables to read.
  pest <- create_pest_control(ncore = 1)
  calib <- create_calib_control(NP = 4, itermax = 8)
  expect_equal(pest$method, calib$method)
  expect_null(calib$engine)
})

test_that("water level becomes per-observation rows in residual mode", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  obs <- AEME::observations(aeme)
  skip_if(is.null(obs$level) || nrow(obs$level) == 0,
          "test aeme lacks observed lake level")

  w <- set_weights(c("HYD_temp", "LKE_lvlwtr"))
  ot <- suppressMessages(
    pest_obs_table(aeme, vars_sim = c("HYD_temp", "LKE_lvlwtr"), weights = w,
                   obj_mode = "residual"))
  map <- attr(ot, "map")

  lvl_i <- which(map$var_aeme == "LKE_lvlwtr")
  expect_gt(length(lvl_i), 0)
  expect_true("HYD_temp" %in% map$var_aeme)         # still there alongside
  # Level obs are keyed with no depth, as .pest_run_residual() matches them.
  expect_true(all(is.na(map$depth[lvl_i])))

  # obsval is on the modelled datum - observed surface elevation minus the
  # deepest bed, matching .raf_wlev() - and restricted to the sim window.
  datum <- min(AEME::input(aeme)$hypsograph$elev)
  tme <- AEME::time(aeme)
  src <- obs$level[obs$level$Date >= as.Date(tme$start) &
                     obs$level$Date <= as.Date(tme$stop) &
                     !is.na(obs$level$value), ]
  expect_equal(sort(ot$obsval[lvl_i]), sort(src$value - datum),
               tolerance = 1e-9)
  expect_setequal(as.Date(map$Date[lvl_i]), as.Date(src$Date))
})

test_that("residual mode needs observed level when LKE_lvlwtr is requested", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  obs <- AEME::observations(aeme)
  obs$level <- NULL
  AEME::observations(aeme) <- obs

  expect_error(
    pest_obs_table(aeme, vars_sim = c("HYD_temp", "LKE_lvlwtr"),
                   weights = set_weights(c("HYD_temp", "LKE_lvlwtr")),
                   obj_mode = "residual"),
    "level"
  )
})

test_that("residual mode reports a simulated value for each water-level obs", {
  # The forward run must emit one value per observation - gridded and level
  # alike - in obs_map order, and record a LKE_lvlwtr fit component.
  ot <- data.frame(
    obsnme = sprintf("o%06d", 1:3), obsval = c(12.9, 13.0, 9.8), weight = 1,
    obgnme = c("hyd_temp", "hyd_temp", "lke_lvlwtr"), stringsAsFactors = FALSE)
  attr(ot, "map") <- data.frame(
    obsnme = ot$obsnme,
    var_aeme = c("HYD_temp", "HYD_temp", "LKE_lvlwtr"),
    Date = as.Date("2020-09-01"),
    depth = c(0.5, 5.0, NA), stringsAsFactors = FALSE)
  map <- attr(ot, "map")

  comp <- data.frame(
    var_aeme = c("HYD_temp", "HYD_temp", "LKE_lvlwtr"),
    Date = map$Date, depth = map$depth,
    model = c(12.5, 13.1, 9.6), obs = ot$obsval,
    diff = c(-0.4, 0.1, -0.2), stringsAsFactors = FALSE)

  p <- list(obs_map = map, vars_sim = c("HYD_temp", "LKE_lvlwtr"),
            na_value = 999, include_wlev = TRUE,
            weights = c(HYD_temp = 1, LKE_lvlwtr = 1),
            FUN_list = list(HYD_temp = function(df) 1,
                            LKE_lvlwtr = function(df) 1))

  local_mocked_bindings(run_and_fit = function(...) comp, .package = "aemetools")
  out <- aemetools:::.pest_run_residual(p, param = NULL, path = ".")
  expect_equal(as.numeric(out), c(12.5, 13.1, 9.6))
  expect_true("LKE_lvlwtr" %in% names(attr(out, "fits")))
})

test_that("residual mode defaults the LKE_lvlwtr fit fn when the caller omits it", {
  ot_map <- data.frame(
    obsnme = sprintf("o%06d", 1:2),
    var_aeme = c("HYD_temp", "LKE_lvlwtr"),
    Date = as.Date("2020-09-01"), depth = c(0.5, NA),
    stringsAsFactors = FALSE)
  comp <- data.frame(
    var_aeme = c("HYD_temp", "LKE_lvlwtr"), Date = as.Date("2020-09-01"),
    depth = c(0.5, NA), model = c(12.5, 9.6), obs = c(12.9, 9.8),
    diff = c(-0.4, -0.2), stringsAsFactors = FALSE)

  # No LKE_lvlwtr entry in FUN_list / weights - run_and_fit() would add one,
  # and .pest_run_residual() must do the same for the run-log component.
  p <- list(obs_map = ot_map, vars_sim = c("HYD_temp", "LKE_lvlwtr"),
            na_value = 999, include_wlev = TRUE, weights = c(HYD_temp = 1),
            FUN_list = list(HYD_temp = function(df) mean(abs(df$diff))))

  local_mocked_bindings(run_and_fit = function(...) comp, .package = "aemetools")
  expect_no_error(
    out <- aemetools:::.pest_run_residual(p, param = NULL, path = "."))
  expect_equal(as.numeric(out), c(12.5, 9.6))
  expect_true(is.finite(attr(out, "fits")[["LKE_lvlwtr"]]))
})

test_that("balanced weighting equalises each variable's contribution to phi", {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  obs <- AEME::observations(aeme)
  vars <- intersect(c("HYD_temp", "CHM_oxy"), unique(obs$lake$var_aeme))
  skip_if(length(vars) < 2, "test aeme lacks two observed variables")

  w <- set_weights(vars)
  ot <- pest_obs_table(aeme, vars_sim = vars, weights = w,
                       obj_mode = "residual")

  # phi contribution of a group at its own mean is sum((weight * (obsval -
  # mean))^2); with balanced weights that is ~weights[v] for every group,
  # regardless of observation count or units.
  phi <- tapply(seq_len(nrow(ot)), ot$obgnme, function(i) {
    sum((ot$weight[i] * (ot$obsval[i] - mean(ot$obsval[i])))^2)
  })
  expect_equal(as.numeric(phi), rep(phi[[1]], length(phi)), tolerance = 0.05)
})

test_that("calib_aeme dispatches to PEST++ and imports the results", {
  skip_on_cran()
  skip_if_not(have_pest(), "PEST++ not installed; run install_pest()")
  install_pest()

  cached <- get_cached_aeme_run(model = "glm_aed", vars_sim = "HYD_temp")
  aeme <- cached$aeme
  path <- cached$path

  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters |>
    dplyr::filter(model == "glm_aed", !duplicated(name)) |>
    head(5) |>
    as.data.frame()

  ctrl <- create_pest_control(
    exe = "pestpp-ies", noptmax = 1, ies_num_reals = 6,
    ncore = parallel::detectCores() - 1,
    pestpp_options = list(ies_lambda_mults = 1, lambda_scale_fac = 1)
  )
  
  sim_id <- calib_aeme(aeme = aeme, param = param,
                       model = "glm_aed", vars_sim = "HYD_temp",
                       FUN_list = list(HYD_temp = kge_loss),
                       weights = set_weights("HYD_temp"), ctrl = ctrl)

  expect_type(sim_id, "character")

  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  expect_true(is.list(calib))
  expect_gt(nrow(calib$simulation_data), 0)
  
  plot_calib_convergence(calib)
  
  # Read through `calib`, not `ctrl`: pest_dir is resolved against the lake
  # directory when the run starts, so the control still holds the relative
  # path. The resolved one is in the metadata, which is what calib carries.
  expect_error(read_pest_phi(ctrl), "lake directory")

  phi <- read_pest_phi(calib)
  expect_gt(nrow(phi), 0)
  expect_true(all(c("iteration", "total_runs", "min") %in% names(phi)))

  resid <- pest_residuals(calib)
  expect_gt(nrow(resid), 0)
  expect_equal(resid$residual, resid$model - resid$obs)

  # calibration_metadata must come back, or every helper that calls
  # resolve_na_value() fails with a zero-length na_value.
  expect_gt(nrow(calib$calibration_metadata), 0)
  expect_equal(calib$calibration_metadata$na_value[1], ctrl$na_value)
  expect_equal(calib$calibration_metadata$c_method[1], "PESTPP-IES")
  # A calibration, not a sensitivity run: sim_id is stemmed "C".
  expect_match(sim_id, "_C_\\d+$")

  # The imported runs must be usable by the existing downstream helpers.
  best <- get_best_params(calib = calib, fit_col = "fit")
  expect_true(is.data.frame(best))
  expect_gt(nrow(best), 0)

  updated <- update_param(calib = calib, param = param)
  expect_true(is.data.frame(updated))
})

test_that("a localizer splits GLM-AED parameters between variables", {
  # The localizer only earns its keep when there is more than one variable
  # to separate, so this is the GLM-AED biogeochemical case: temperature is
  # fitted by the hydrodynamic parameters, oxygen by the biogeochemical
  # ones, and pestpp-ies never lets an oxygen residual move a light or
  # mixing parameter.
  skip_on_cran()
  skip_if_not(have_pest(), "PEST++ not installed; run install_pest()")
  install_pest()

  vars_sim <- c("HYD_temp", "CHM_oxy")
  cached <- get_cached_aeme_run(model = "glm_aed", use_bgc = TRUE,
                                vars_sim = vars_sim)
  aeme <- cached$aeme
  path <- cached$path

  # Residual mode needs observations for both variables inside the
  # simulation window, or pest_obs_table() has nothing to build.
  obs <- AEME::observations(aeme)
  tme <- AEME::time(aeme)
  in_window <- obs$lake[!is.na(obs$lake$value) &
                          obs$lake$Date >= as.Date(tme$start) &
                          obs$lake$Date <= as.Date(tme$stop), ]
  skip_if(!all(vars_sim %in% unique(in_window$var_aeme)),
          "test aeme lacks in-window observations for both variables")

  # Take the selectors from the parameter tables themselves rather than
  # hard-coding group names, so the test does not break when AEME's
  # parameter set changes - and an unmatched selector is a hard error.
  data("aeme_parameters", package = "AEME")
  data("aeme_parameters_bgc", package = "AEME")
  hyd_param <- aeme_parameters |>
    dplyr::filter(model == "glm_aed", !duplicated(name)) |>
    head(3) |>
    as.data.frame() |> 
    dplyr::mutate(file = "glm4.nml")
  bgc_param <- aeme_parameters_bgc |>
    dplyr::filter(model == "glm_aed", !name %in% hyd_param$name,
                  grepl("CHM_oxy", var_sim)) |>
    head(3) |>
    as.data.frame()
  skip_if(nrow(bgc_param) < 2,
          "no glm_aed biogeochemical parameters to split on")
  param <- dplyr::bind_rows(hyd_param, bgc_param)

  localizer <- list(HYD_temp = hyd_param$name, CHM_oxy = bgc_param$name)

  # Deliberately tiny, as in the serial test above: the point is the
  # localizer, not the convergence.
  #
  # `noise_sd` is about getting past iteration 0, not about localization:
  # without an obs-noise-specific option pestpp-ies reports "no
  # obs-noise-specific options have been passed, resetting to
  # `ies_no_noise` to true" and builds an observation ensemble with no
  # spread at all, against which the prior-data-conflict test flags
  # essentially every observation.
  #
  # The test lake's AED oxygen comes out around 0.1 mg/L against
  # observations near 10 mg/L, so CHM_oxy stays in conflict whatever noise
  # is assumed. That is fine here - `ies_drop_conflicts` is off by default,
  # so the conflicts are reported and the run continues. This test asserts
  # that the localizer is written, referenced and correctly structured, not
  # that wainamu's oxygen is calibratable.
  ctrl <- create_pest_control(
    exe = "pestpp-ies", noptmax = 1, ies_num_reals = 4,
    parallel = FALSE, ncore = 1,
    file_dir = file.path(path, "calib_sa"), pest_dir = "pest",
    localizer = localizer,
    # `seed` also makes the parameter ensemble be drawn in R, so the
    # observation ensemble's realisation names have something to align
    # with rather than being generated against pestpp-ies's own draw.
    noise_sd = c(HYD_temp = 0.5, CHM_oxy = 1), seed = 42,
    pestpp_options = list(ies_lambda_mults = 1, lambda_scale_fac = 1))

  # The specification is kept as given; it is resolved against the
  # parameter and observation tables once the run starts.
  expect_equal(ctrl$localizer, localizer)

  sim_id <- calib_aeme(aeme = aeme, path = path, param = param,
                       model = "glm_aed", vars_sim = vars_sim,
                       FUN_list = list(HYD_temp = kge_loss, CHM_oxy = kge_loss),
                       weights = set_weights(vars_sim), ctrl = ctrl)

  expect_type(sim_id, "character")
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)

  # Use the resolved directory from the metadata: ctrl$pest_dir is still
  # the unresolved relative path.
  run_dir <- calib$calibration_metadata$pest_dir[1]
  mat_file <- file.path(run_dir, "aeme_localizer.mat")
  expect_true(file.exists(mat_file))

  # ...and the control file points the solver at it, so the run actually
  # localized rather than quietly writing an unused matrix.
  pst <- readLines(file.path(run_dir, "aeme.pst"))
  expect_true(any(grepl("++ies_localizer(aeme_localizer.mat)", pst,
                        fixed = TRUE)))

  # Rows are observation groups, columns are parameters, written dense so
  # every name is validated against the control file.
  mat <- aemetools:::.pest_read_cov(mat_file)
  expect_setequal(rownames(mat), c("hyd_temp", "chm_oxy"))
  expect_equal(ncol(mat), nrow(param))
  # Binary: pestpp-ies localization is per-case subsetting, not a weighting.
  expect_true(all(mat %in% c(0, 1)))
  expect_equal(sum(mat["hyd_temp", ]), nrow(hyd_param))
  expect_equal(sum(mat["chm_oxy", ]), nrow(bgc_param))
  # Disjoint and complete: every parameter is updated by exactly one group,
  # so none is left in no case at all (which pestpp-ies would treat as
  # fixed, without saying so).
  expect_true(all(mat["hyd_temp", ] + mat["chm_oxy", ] == 1))

  # A localized run still imports like any other.
  expect_gt(nrow(calib$simulation_data), 0)
  expect_equal(calib$calibration_metadata$c_method[1], "PESTPP-IES")
  phi <- read_pest_phi(calib)
  expect_gt(nrow(phi), 0)
})

test_that("a stalled solver is capped rather than waited on for a day", {
  # Observed in the wild: pestpp-ies completed every model run, terminated
  # all its agents cleanly, then failed to exit - spinning at 100% CPU with
  # its record file unflushed for four hours. Watching only for the exit
  # sentinel cannot tell that apart from "still working", and
  # solver_timeout alone would have waited a further 24 hours.
  mk <- function(nruns, itermax, stall_secs = 3) {
    d <- withr::local_tempdir(.local_envir = parent.frame(2))
    dir.create(file.path(d, "runlog"), recursive = TRUE)
    writeLines(c("a,fit", rep("1,1", nruns)),
               file.path(d, "runlog", "runlog_1.csv"))
    writeLines("stale record", file.path(d, "aeme.rec"))
    ctrl <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1)
    ctrl$itermax <- itermax
    ctrl$stall_minutes <- stall_secs / 60
    ctrl
  }

  expect_equal(aemetools:::.pest_logged_runs(mk(12, 12)), 12L)

  # A master that stays alive throughout, so .pest_wait() reaches the stall
  # logic rather than returning because the process has exited.
  live_master <- function(pest_dir) {
    p <- aemetools:::.pest_procs()
    p$master <- aemetools:::.pest_spawn(file.path(R.home("bin"), "Rscript"),
                                        c("-e", "Sys.sleep(600)"), pest_dir,
                                        "run_master")
    p
  }

  # Every expected run is already logged, so the results are complete and
  # discarding them because the master would not exit would be perverse.
  ctrl_ok <- mk(12, 12)
  p_ok <- live_master(ctrl_ok$pest_dir)
  expect_no_error(
    aemetools:::.pest_wait(p_ok, ctrl_ok$pest_dir, "aeme.pst", ctrl_ok, poll = 1))
  p_ok$master$kill_tree()

  # Runs still missing: this is a genuine failure and must abort, saying
  # how far it got.
  ctrl_bad <- mk(3, 12)
  p_bad <- live_master(ctrl_bad$pest_dir)
  expect_error(
    aemetools:::.pest_wait(p_bad, ctrl_bad$pest_dir, "aeme.pst", ctrl_bad,
                           poll = 1),
    "stopped making progress")
  expect_error(
    aemetools:::.pest_wait(p_bad, ctrl_bad$pest_dir, "aeme.pst", ctrl_bad,
                           poll = 1),
    "3 model runs logged of 12 expected")
  p_bad$master$kill_tree()
})

test_that("progress resets the stall clock", {
  skip_on_cran()
  d <- withr::local_tempdir()
  dir.create(file.path(d, "runlog"), recursive = TRUE)
  log <- file.path(d, "runlog", "runlog_1.csv")
  writeLines(c("a,fit", "1,1"), log)
  writeLines("stale", file.path(d, "aeme.rec"))
  ctrl <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1)
  ctrl$itermax <- 99
  ctrl$stall_minutes <- 6 / 60

  p <- aemetools:::.pest_procs()
  p$master <- aemetools:::.pest_spawn(file.path(R.home("bin"), "Rscript"),
                                      c("-e", "Sys.sleep(600)"), d, "run_master")

  # A run lands 3s in; the wait must then survive past the original window.
  writer <- processx::process$new(
    file.path(R.home("bin"), "Rscript"),
    c("-e", sprintf("Sys.sleep(3); cat('1,1\\n', file='%s', append=TRUE)",
                    gsub("\\\\", "/", log))))
  t0 <- Sys.time()
  expect_error(aemetools:::.pest_wait(p, d, "aeme.pst", ctrl, poll = 1))
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_gt(elapsed, 6)
  p$master$kill_tree()
  try(writer$kill(), silent = TRUE)
})

test_that("the serial path runs the solver synchronously and checks status", {
  # `parallel = FALSE` runs PEST++ with no run manager: the master evaluates
  # the model itself, `.pest_launch()` blocks on `processx::run()`, and the
  # exit status is available directly. A stub stands in for the solver -
  # Rscript running a `.pst` that is really just `quit(status = N)`, so this
  # needs neither PEST++ nor a model run.
  skip_on_cran()
  rscript <- file.path(R.home("bin"), "Rscript")

  mk <- function(status) {
    d <- withr::local_tempdir(.local_envir = parent.frame(2))
    writeLines(sprintf("quit(save = 'no', status = %d)", status),
               file.path(d, "aeme.pst"))
    list(d = d, ctrl = create_pest_control(pest_dir = d, case = "aeme",
                                           ncore = 4, parallel = FALSE))
  }

  ok <- mk(0)
  expect_no_error(
    aemetools:::.pest_launch(file.path(ok$d, "aeme.pst"), rscript, ok$ctrl,
                             lake_dir = ok$d, m = "glm_aed"))
  # No agents and no detached-master files in serial mode.
  expect_length(list.files(ok$d, pattern = "^agent_"), 0L)
  expect_length(list.files(ok$d, pattern = "^run_master"), 0L)

  bad <- mk(3)
  expect_error(
    aemetools:::.pest_launch(file.path(bad$d, "aeme.pst"), rscript, bad$ctrl,
                             lake_dir = bad$d, m = "glm_aed"),
    "exited with status")
})

test_that("calib_aeme completes a serial PEST++ run end to end", {
  # The serial branch had never actually been executed against PEST++.
  # Worth real coverage: it is the fallback when the PANTHER run manager
  # misbehaves, which has now happened more than once.
  skip_on_cran()
  skip_if_not(have_pest(), "PEST++ not installed; run install_pest()")

  cached <- get_cached_aeme_run(model = "gotm_wet", vars_sim = "HYD_temp")
  aeme <- cached$aeme
  path <- cached$path

  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters |>
    dplyr::filter(model == "gotm_wet", !duplicated(name)) |>
    head(3) |>
    as.data.frame()

  # Deliberately tiny: 4 realisations, one iteration, a single lambda and
  # scale factor, so the serial run stays short.
  ctrl <- create_pest_control(
    exe = "pestpp-ies", noptmax = 1, ies_num_reals = 4,
    parallel = FALSE, ncore = 1,
    file_dir = file.path(path, "calib_sa"), pest_dir = "pest",
    pestpp_options = list(ies_lambda_mults = 1, lambda_scale_fac = 1))

  sim_id <- calib_aeme(aeme = aeme, path = path, param = param,
                       model = "gotm_wet", vars_sim = "HYD_temp",
                       FUN_list = list(HYD_temp = kge_loss),
                       weights = set_weights("HYD_temp"), ctrl = ctrl)

  expect_type(sim_id, "character")
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  expect_gt(nrow(calib$simulation_data), 0)
  # Serial mode must leave no agent directories behind. Use the resolved
  # directory from the metadata, not ctrl$pest_dir: that is still the
  # unresolved relative path, which does not exist, so list.files() would
  # return nothing and this would pass no matter what the code did.
  run_dir <- calib$calibration_metadata$pest_dir[1]
  expect_true(dir.exists(run_dir))
  expect_length(list.files(run_dir, pattern = "^agent_"), 0L)
  # Each model gets its own subdirectory under pest_dir.
  expect_equal(basename(run_dir), "gotm_wet")
})

test_that("a frozen parameter is held fixed, not dropped, in a PEST run", {
  skip_on_cran()
  skip_if_not(have_pest(), "PEST++ not installed; run install_pest()")

  cached <- get_cached_aeme_run(model = "gotm_wet", vars_sim = "HYD_temp")
  aeme <- cached$aeme
  path <- cached$path

  data("aeme_parameters", package = "AEME")
  param <- aeme_parameters |>
    dplyr::filter(model == "gotm_wet", !duplicated(name)) |>
    head(4) |>
    as.data.frame()
  # Freeze the 4th: value == min == max.
  param$min[4] <- param$value[4]
  param$max[4] <- param$value[4]
  frozen_name <- param$name[4]

  ctrl <- create_pest_control(
    exe = "pestpp-ies", noptmax = 1, ies_num_reals = 4,
    parallel = FALSE, ncore = 1,
    file_dir = file.path(path, "calib_sa"), pest_dir = "pest",
    pestpp_options = list(ies_lambda_mults = 1, lambda_scale_fac = 1))

  sim_id <- calib_aeme(aeme = aeme, path = path, param = param,
                       model = "gotm_wet", vars_sim = "HYD_temp",
                       FUN_list = list(HYD_temp = kge_loss),
                       weights = set_weights("HYD_temp"), ctrl = ctrl)
  calib <- read_calib(ctrl = ctrl, sim_id = sim_id)
  run_dir <- calib$calibration_metadata$pest_dir[1]

  # The .pst carries it as `partrans = fixed`, not as one fewer parameter.
  pst <- readLines(file.path(run_dir, "aeme.pst"))
  expect_true(any(grepl("^p0\\d\\d\\s+fixed\\b", pst)))
  pm <- utils::read.csv(file.path(run_dir, "aeme_par_map.csv"))
  expect_equal(nrow(pm), 4)
  expect_true(any(grepl(frozen_name, pm$name_full, fixed = TRUE)))

  # It survives into the metadata and the posterior sets, as a constant.
  expect_true(frozen_name %in% calib$parameter_metadata$name)
  post <- pest_posterior_params(calib)
  s1 <- post[[1]]
  expect_true(frozen_name %in% s1$name)
  across_sets <- vapply(post, function(s) s$value[s$name == frozen_name],
                        numeric(1))
  expect_equal(stats::sd(across_sets), 0)
})

test_that("each model gets its own PEST directory", {
  # calib_aeme() passes one control to every model in turn. Without a
  # per-model subdirectory both would run in the same place and, with
  # overwrite = TRUE, the second would delete the first's files - leaving
  # the first model's recorded pest_dir pointing at the second model's
  # ensembles, so its posterior would read back silently wrong.
  d <- withr::local_tempdir()
  lake_dir <- file.path(d, "LID1_test")
  dir.create(lake_dir, recursive = TRUE)

  # normalizePath so the comparisons below are not defeated by Windows
  # backslashes from tempdir() meeting forward slashes from file.path().
  norm <- function(x) normalizePath(x, winslash = "/", mustWork = FALSE)
  resolve <- function(pest_dir, m) {
    p <- if (grepl("^(/|~|[A-Za-z]:)", pest_dir)) pest_dir else
      file.path(lake_dir, pest_dir)
    norm(file.path(p, m))
  }

  a <- resolve("pest", "glm_aed")
  b <- resolve("pest", "gotm_wet")
  expect_false(identical(a, b))
  expect_equal(basename(a), "glm_aed")
  expect_equal(dirname(a), dirname(b))

  # An absolute pest_dir is separated per model too - the collision is the
  # same whether the path was given relative or absolute.
  abs_a <- resolve(d, "glm_aed")
  abs_b <- resolve(d, "gotm_wet")
  expect_false(identical(abs_a, abs_b))
  expect_equal(dirname(abs_a), norm(d))
})

test_that("observations the model cannot simulate are excluded", {
  # An observation outside the simulation period still carries a non-zero
  # weight in the control file, so it contributes to phi - but the model
  # will never produce a value for it. Left in, the objective being
  # minimised is dominated by residuals against observations the model was
  # never asked to reproduce. In the AEME test lake that is 99 of 224:
  # observations span 2019-08-07 to 2021-06-10, the model runs 2020-08-01
  # to 2021-06-30.
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
  tme <- AEME::time(aeme)
  w <- set_weights("HYD_temp")

  # setup.R sets AEME.inform = FALSE for the session, so this must be
  # switched back on or the message can never be observed and the
  # expectation would be untestable.
  # expect_message() returns the condition, not the value, so capture the
  # table separately.
  ot <- withr::with_options(list(AEME.inform = TRUE), {
    expect_message(x <- pest_obs_table(aeme, vars_sim = "HYD_temp",
                                       weights = w),
                   "does not simulate")
    x
  })
  map <- attr(ot, "map")
  expect_gt(nrow(ot), 0)
  expect_true(all(map$Date >= as.Date(tme$start)))
  expect_true(all(map$Date <= as.Date(tme$stop)))

  # Every observation kept must be one the model could produce, so the
  # forward run's per-observation match cannot come up short.
  obs <- AEME::observations(aeme)$lake
  o <- obs[obs$var_aeme == "HYD_temp" & !is.na(obs$value), ]
  inside <- sum(o$Date >= as.Date(tme$start) & o$Date <= as.Date(tme$stop))
  expect_equal(nrow(ot), inside)

  # var_indices is the exact answer when available: only the dates the
  # model actually wrote are kept.
  vi <- list(HYD_temp = list(dates = unique(map$Date)[1:3]))
  ot2 <- suppressMessages(
    pest_obs_table(aeme, vars_sim = "HYD_temp", weights = w,
                   var_indices = vi))
  expect_setequal(attr(ot2, "map")$Date, unique(map$Date)[1:3])
})
