# Tests for the prior / noise ensemble builders and the binary ensemble
# reader. None of these need a PEST++ binary or a model run.

pp_param <- function() {
  data.frame(
    model = "glm_aed", file = "glm3.nml",
    group = c("light", "light", NA),
    name  = c("Kw", "ce", "MET_tmpair"),
    index = c(1L, NA_integer_, NA_integer_),
    value = c(0.5, 0.0013, 1.0),
    min   = c(0.1, 0.0005, 0.8),
    max   = c(1.5, 0.005, 1.2),
    log   = c(TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
}

# --- pest_prior_cov -------------------------------------------------------

test_that("pest_prior_cov is diagonal and log-aware", {
  cov <- pest_prior_cov(pp_param(), sigma_range = 4)

  expect_equal(dim(cov), c(3, 3))
  expect_equal(rownames(cov), c("p001", "p002", "p003"))
  expect_true(all(cov[upper.tri(cov)] == 0))

  # p001 is log-transformed: sd is in log10 space.
  sd1 <- (log10(1.5) - log10(0.1)) / 4
  expect_equal(sqrt(cov[1, 1]), sd1)
  # p002 is native.
  expect_equal(sqrt(cov[2, 2]), (0.005 - 0.0005) / 4)

  expect_s3_class(attr(cov, "map"), "data.frame")
})

test_that("pest_prior_cov round-trips through the PEST matrix file", {
  d <- withr::local_tempdir()
  f <- file.path(d, "prior.cov")
  cov <- pest_prior_cov(pp_param(), file = f)

  expect_true(file.exists(f))
  back <- .pest_read_cov(f)
  expect_equal(unname(back), unname(unclass(cov)), tolerance = 1e-6,
               ignore_attr = TRUE)
  expect_equal(rownames(back), rownames(cov))
})

# --- pest_prior_ensemble ----------------------------------------------------

test_that("pest_prior_ensemble: uniform draw respects bounds and seed", {
  e1 <- pest_prior_ensemble(pp_param(), n = 40, seed = 1)
  e2 <- pest_prior_ensemble(pp_param(), n = 40, seed = 1)

  expect_equal(e1, e2)                                   # reproducible
  expect_equal(names(e1)[1], "real_name")
  expect_equal(names(e1)[-1], c("p001", "p002", "p003"))
  expect_equal(nrow(e1), 40)

  # last realisation is the base (initial values) by default
  expect_equal(e1$real_name[40], "base")
  expect_equal(as.numeric(e1[40, -1]), c(0.5, 0.0013, 1.0))

  # every non-base draw is inside [min, max]
  body <- e1[e1$real_name != "base", -1]
  expect_true(all(body$p001 >= 0.1 & body$p001 <= 1.5))
  expect_true(all(body$p002 >= 0.0005 & body$p002 <= 0.005))
  expect_true(all(body$p001 > 0))                        # log param stays positive
})

test_that("pest_prior_ensemble: normal + triangular stay in bounds", {
  en <- pest_prior_ensemble(pp_param(), n = 50, dist = "normal", seed = 2)
  et <- pest_prior_ensemble(pp_param(), n = 50, dist = "triangular", seed = 2)

  for (e in list(en, et)) {
    b <- e[e$real_name != "base", -1]
    expect_true(all(b$p001 >= 0.1 & b$p001 <= 1.5))
    expect_true(all(b$p002 >= 0.0005 & b$p002 <= 0.005))
    expect_true(all(b$p003 >= 0.8 & b$p003 <= 1.2))
  }
})

test_that("pest_prior_ensemble: correlated normal via a covariance matrix", {
  cov <- pest_prior_cov(pp_param())
  cov[1, 2] <- cov[2, 1] <- 0.4 * sqrt(cov[1, 1] * cov[2, 2])
  e <- pest_prior_ensemble(pp_param(), n = 200, dist = "normal", cov = cov,
                           seed = 3, include_base = FALSE)
  expect_equal(nrow(e), 200)
  expect_true(all(e$p001 >= 0.1 & e$p001 <= 1.5))
})

test_that("pest_prior_ensemble: custom real_names and no base", {
  rn <- paste0("r", 1:10)
  e <- pest_prior_ensemble(pp_param(), n = 10, seed = 1, include_base = FALSE,
                           real_names = rn)
  expect_equal(e$real_name, rn)
  expect_error(
    pest_prior_ensemble(pp_param(), n = 10, real_names = rn[1:5]),
    "length"
  )
})

# --- pest_obs_ensemble ----------------------------------------------------

test_that("pest_obs_ensemble applies per-variable noise and keeps zero-weight obs clean", {
  ot <- make_obs_tbl()          # 3 obs, all HYD_temp, weight 0.4
  ot$weight[3] <- 0             # obs 3 carries no noise

  e <- pest_obs_ensemble(ot, n = 100, noise_sd = c(HYD_temp = 0.5), seed = 7)

  expect_equal(names(e), c("real_name", ot$obsnme))
  expect_equal(nrow(e), 100)
  expect_equal(e$real_name[100], "base")
  # base row is the observed values
  expect_equal(as.numeric(e[100, -1]), ot$obsval)
  # zero-weight obs identical across every realisation
  expect_equal(length(unique(e[[ot$obsnme[3]]])), 1L)
  # noisy obs actually varies, and near the observed mean
  expect_gt(stats::sd(e[[ot$obsnme[1]]][-100]), 0)
  expect_equal(mean(e[[ot$obsnme[1]]][-100]), ot$obsval[1], tolerance = 0.3)
})

test_that("pest_obs_ensemble errors on a missing variable", {
  ot <- make_obs_tbl()
  expect_error(pest_obs_ensemble(ot, n = 10, noise_sd = c(DO = 1)), "no entry")
})

test_that("pest_obs_ensemble falls back to 1/weight when noise_sd is absent", {
  ot <- make_obs_tbl()               # weight 0.4 -> sd 2.5
  e <- pest_obs_ensemble(ot, n = 800, seed = 11)
  expect_equal(stats::sd(e[[ot$obsnme[1]]][e$real_name != "base"]),
               1 / 0.4, tolerance = 0.4)
})

# --- binary ensemble reader --------------------------------------------------

# Write a dense-format PEST binary matrix, matching pyemu Matrix.write_dense:
#   header int32 x3 = (0, -ncol, -ncol)
#   int32 x ncol    = column-name lengths
#   raw             = each column name
#   per row: int32 namelen, raw name, float64 x ncol
write_dense_bin <- function(path, row_names, col_names, mat) {
  con <- file(path, "wb")
  on.exit(close(con))
  nc <- length(col_names)
  writeBin(as.integer(c(0L, -nc, -nc)), con, size = 4L, endian = "little")
  writeBin(as.integer(nchar(col_names)), con, size = 4L, endian = "little")
  for (nm in col_names) writeBin(charToRaw(nm), con)
  for (i in seq_along(row_names)) {
    writeBin(nchar(row_names[i]), con, size = 4L, endian = "little")
    writeBin(charToRaw(row_names[i]), con)
    writeBin(as.double(mat[i, ]), con, size = 8L, endian = "little")
  }
}

test_that(".pest_read_ensemble_bin reads the dense format like the CSV reader", {
  d <- withr::local_tempdir()
  rn <- c("real_0", "real_1", "base")
  cn <- c("p001", "p002")
  m  <- matrix(c(0.11, 0.0021,
                 0.98, 0.0044,
                 0.50, 0.0013), nrow = 3, byrow = TRUE)
  f <- file.path(d, "aeme.0.par.jcb")
  write_dense_bin(f, rn, cn, m)

  got <- .pest_read_ensemble_bin(f)
  expect_equal(names(got), c("real_name", "p001", "p002"))
  expect_equal(got$real_name, rn)
  expect_equal(as.matrix(got[, -1]), m, tolerance = 1e-9,
               ignore_attr = TRUE)
})

test_that(".pest_read_ensemble_bin reads the classic sparse format", {
  d <- withr::local_tempdir()
  rn <- c("real_0", "real_1")
  cn <- c("p001", "p002")
  m  <- matrix(c(0.2, 0.003,
                 0.9, 0.004), nrow = 2, byrow = TRUE)

  f <- file.path(d, "aeme.1.par.jcb")
  con <- file(f, "wb")
  nr <- nrow(m); nc <- ncol(m)
  writeBin(as.integer(c(-nc, -nr, nr * nc)), con, size = 4L, endian = "little")
  for (icol in seq_len(nc)) for (irow in seq_len(nr)) {
    lin <- irow + (icol - 1L) * nr            # 1-based, column-major
    writeBin(as.integer(lin), con, size = 4L, endian = "little")
    writeBin(as.double(m[irow, icol]), con, size = 8L, endian = "little")
  }
  for (nm in cn) writeBin(charToRaw(sprintf("%-12s", nm)), con)
  for (nm in rn) writeBin(charToRaw(sprintf("%-20s", nm)), con)
  close(con)

  got <- .pest_read_ensemble_bin(f)
  expect_equal(got$real_name, rn)
  expect_equal(as.matrix(got[, -1]), m, tolerance = 1e-9, ignore_attr = TRUE)
})

test_that(".pest_ensemble_files prefers csv but falls back to jcb", {
  d <- withr::local_tempdir()
  file.create(file.path(d, c("aeme.0.par.csv", "aeme.1.par.jcb",
                             "aeme.2.par.csv", "aeme.2.par.jcb")))
  ef <- .pest_ensemble_files(d, "aeme", "par")
  expect_equal(ef$iteration, c(0, 1, 2))
  expect_equal(basename(ef$path),
               c("aeme.0.par.csv", "aeme.1.par.jcb", "aeme.2.par.csv"))
})

# --- ies_save_binary guard -----------------------------------------------

test_that("the ++ block pins ies_save_binary to false", {
  ctrl <- create_pest_control(exe = "pestpp-ies", ncore = 1)
  lines <- .pest_plusplus_lines(ctrl)
  expect_true(any(grepl("^\\+\\+ies_save_binary\\(false\\)$", lines)))
})

# --- ies_drop_conflicts default ------------------------------------------

test_that("conflict dropping is off by default and the user can turn it on", {
  # pestpp-ies runs with no observation noise unless an obs-noise-specific
  # option is supplied, and against noiseless observations the conflict
  # test flags nearly everything - dropping which aborts the run with "all
  # non-zero weighted observations in conflict state".
  ctrl <- create_pest_control(exe = "pestpp-ies", ncore = 1)
  lines <- .pest_plusplus_lines(ctrl)
  expect_true(any(grepl("^\\+\\+ies_drop_conflicts\\(false\\)$", lines)))

  on <- create_pest_control(
    exe = "pestpp-ies", ncore = 1, noise_sd = c(HYD_temp = 0.5),
    pestpp_options = list(ies_drop_conflicts = TRUE))
  expect_true(any(grepl("^\\+\\+ies_drop_conflicts\\(true\\)$",
                        .pest_plusplus_lines(on))))
})

test_that("an obscov still forces conflict dropping off when it was on", {
  # pestpp-ies refuses a user-supplied obscov alongside ies_drop_conflicts,
  # so the standard_deviation branch overrides it - but only when the user
  # actually asked for it, now that the default is already off.
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(exe = "pestpp-ies", pest_dir = d, ncore = 1,
                              noise_sd = c(HYD_temp = 0.5),
                              noise_method = "standard_deviation",
                              pestpp_options = list(ies_drop_conflicts = TRUE))
  ctrl2 <- .pest_setup_ensembles(ctrl, pest_param_table(pp_param()),
                                 make_obs_tbl(), pp_param())
  expect_equal(ctrl2$pestpp_options$ies_drop_conflicts, "false")

  # Left alone when the user never set it: there is nothing to disable.
  plain <- create_pest_control(exe = "pestpp-ies", pest_dir = d, ncore = 1,
                               noise_sd = c(HYD_temp = 0.5),
                               noise_method = "standard_deviation")
  plain2 <- .pest_setup_ensembles(plain, pest_param_table(pp_param()),
                                  make_obs_tbl(), pp_param())
  expect_null(plain2$pestpp_options$ies_drop_conflicts)
})

test_that(".pest_opt_true reads both PEST++ strings and R logicals", {
  expect_true(.pest_opt_true(TRUE))
  expect_true(.pest_opt_true("true"))
  expect_true(.pest_opt_true("TRUE"))
  expect_true(.pest_opt_true(" True "))
  expect_false(.pest_opt_true(FALSE))
  expect_false(.pest_opt_true("false"))
  expect_false(.pest_opt_true(NULL))
  expect_false(.pest_opt_true(NA))
  expect_false(.pest_opt_true(character(0)))
})

# --- create_pest_control validation ------------------------------------------

test_that("create_pest_control validates the new prior/noise args", {
  expect_error(create_pest_control(prior_dist = "lognormal", ncore = 1),
               "prior_dist")
  expect_error(create_pest_control(noise_sd = 0.5, ncore = 1), "named numeric")
  expect_error(create_pest_control(seed = "abc", ncore = 1), "whole number")

  # a covariance matrix flips prior_dist to normal
  cv <- pest_prior_cov(pp_param())
  ctrl <- create_pest_control(prior_cov = cv, ncore = 1)
  expect_equal(ctrl$prior_dist, "normal")
})

test_that(".pest_setup_ensembles wires the ++ options and aligns realisations", {
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(exe = "pestpp-ies", pest_dir = d, case = "aeme",
                              ncore = 1, ies_num_reals = 20, seed = 42,
                              noise_sd = c(HYD_temp = 0.5))
  par_tbl <- pest_param_table(pp_param())
  obs_tbl <- make_obs_tbl()

  ctrl2 <- .pest_setup_ensembles(ctrl, par_tbl = par_tbl, obs_tbl = obs_tbl,
                                 param = pp_param())

  opts <- ctrl2$pestpp_options
  expect_equal(opts$ies_parameter_ensemble, "prior_par_en.csv")
  expect_equal(opts$ies_observation_ensemble, "obs_en.csv")
  expect_equal(opts$ies_include_base, "true")

  pe <- utils::read.csv(file.path(d, "prior_par_en.csv"), check.names = FALSE)
  oe <- utils::read.csv(file.path(d, "obs_en.csv"), check.names = FALSE)
  expect_equal(nrow(pe), 20)
  expect_equal(pe[[1]], oe[[1]])                       # realisation names align
  expect_equal(pe[[1]][20], "base")
})

test_that(".pest_setup_ensembles: standard_deviation mode writes an obscov", {
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(exe = "pestpp-ies", pest_dir = d, ncore = 1,
                              noise_sd = c(HYD_temp = 0.5),
                              noise_method = "standard_deviation")
  ctrl2 <- .pest_setup_ensembles(ctrl, pest_param_table(pp_param()),
                                 make_obs_tbl(), pp_param())

  expect_equal(ctrl2$pestpp_options$obscov, "aeme_obscov.unc")
  expect_null(ctrl2$pestpp_options$ies_observation_ensemble)
  sd <- .pest_read_unc_sd(file.path(d, "aeme_obscov.unc"))
  expect_equal(length(sd), 3L)
  expect_equal(unname(sd), rep(0.5, 3))
})

test_that(".pest_setup_ensembles: restart_from resumes from the last ensembles", {
  # A prior run directory with iteration 0 and 1 ensembles.
  src <- withr::local_tempdir()
  pt <- pest_param_table(pp_param())
  ot <- make_obs_tbl()
  prev <- create_pest_control(exe = "pestpp-ies", pest_dir = src, case = "aeme",
                              ncore = 1)
  write_pest_tpl(pt, prev); write_pest_ins(ot, prev)
  write_pst(pt, ot, prev, stats::setNames("aeme_pars.csv", "aeme_pars.csv.tpl"),
            stats::setNames("aeme_sim.out", "aeme_sim.ins"), "cmd")
  rn <- paste0("real_", 0:3)
  for (it in 0:1) {
    utils::write.csv(data.frame(real_name = rn, p001 = it + (1:4) / 10,
                                p002 = 0.001 * (1:4), p003 = 0.9 + (1:4) / 100),
                     file.path(src, sprintf("aeme.%d.par.csv", it)),
                     row.names = FALSE)
    utils::write.csv(data.frame(real_name = rn, o000001 = 12 + (1:4) / 10,
                                o000002 = 13 + (1:4) / 10, o000003 = 9 + (1:4) / 10),
                     file.path(src, sprintf("aeme.%d.obs.csv", it)),
                     row.names = FALSE)
  }

  d <- withr::local_tempdir()
  ctrl <- create_pest_control(exe = "pestpp-ies", pest_dir = d, ncore = 1,
                              restart_from = src)
  ctrl2 <- .pest_setup_ensembles(ctrl, pt, ot, pp_param())

  opts <- ctrl2$pestpp_options
  expect_equal(opts$ies_parameter_ensemble, "restart_par.csv")
  expect_equal(opts$ies_restart_observation_ensemble, "restart_obs.csv")
  expect_equal(opts$ies_num_reals, 4)
  expect_true(file.exists(file.path(d, "restart_par.csv")))

  # a restart whose ensemble lacks a parameter the current problem needs
  src2 <- withr::local_tempdir()
  writeLines("pcf", file.path(src2, "aeme.pst"))
  utils::write.csv(data.frame(real_name = rn, p001 = 1:4 / 10, p002 = 1:4 / 10),
                   file.path(src2, "aeme.0.par.csv"), row.names = FALSE)
  utils::write.csv(data.frame(real_name = rn, o000001 = 1:4, o000002 = 1:4,
                              o000003 = 1:4),
                   file.path(src2, "aeme.0.obs.csv"), row.names = FALSE)
  bad <- create_pest_control(exe = "pestpp-ies", pest_dir = withr::local_tempdir(),
                             ncore = 1, restart_from = src2)
  expect_error(.pest_setup_ensembles(bad, pt, ot, pp_param()), "does not match")
})

test_that(".pest_setup_ensembles is a no-op for the default control", {
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(exe = "pestpp-ies", pest_dir = d, ncore = 1)
  ctrl2 <- .pest_setup_ensembles(ctrl, pest_param_table(pp_param()),
                                 make_obs_tbl(), pp_param())
  expect_null(ctrl2$pestpp_options$ies_parameter_ensemble)
  expect_null(ctrl2$pestpp_options$ies_observation_ensemble)
  expect_false(file.exists(file.path(d, "prior_par_en.csv")))
})

# --- live checks: does pestpp-ies accept what we generate? -----------------

# Build a throwaway interface with a constant-output "model" so a real
# pestpp-ies run exercises the generated files without needing AEME.
pest_stub_dir <- function(ctrl) {
  d <- ctrl$pest_dir
  pt <- pest_param_table(pp_param())
  ot <- make_obs_tbl()
  tpl <- write_pest_tpl(pt, ctrl)
  ins <- write_pest_ins(ot, ctrl)
  # A parameter-responsive "model": output tracks the parameter sum so the
  # ensemble has spread (a constant model breaks IES conflict detection).
  writeLines(c(
    "p <- utils::read.csv('aeme_pars.csv')",
    "d <- sum(p$value) - 1.5",
    sprintf("writeLines(c(%s), 'aeme_sim.out')",
            paste(sprintf('sprintf("%s %%g", %g + d * 0.1)',
                          ot$obsnme, ot$obsval),
                  collapse = ", "))),
    file.path(d, "run_model.R"))
  ctrl <- .pest_setup_ensembles(ctrl, pt, ot, pp_param())
  rscript <- file.path(R.home("bin"), "Rscript")
  write_pst(pt, ot, ctrl, tpl, ins,
            model_command = paste0("\"", rscript, "\" run_model.R"))
  list(d = d, ctrl = ctrl)
}

test_that("pestpp-ies accepts a generated obscov (live)", {
  skip_if(!have_pest())
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(exe = "pestpp-ies", pest_dir = d, case = "aeme",
                              ncore = 1, noptmax = 1, ies_num_reals = 6,
                              seed = 1, noise_sd = c(HYD_temp = 0.5),
                              noise_method = "standard_deviation")
  s <- pest_stub_dir(ctrl)
  exe <- pest_exe_path("pestpp-ies")

  st <- withr::with_dir(d, system2(exe, "aeme.pst", stdout = FALSE,
                                   stderr = FALSE))
  expect_equal(st, 0)
  expect_true(file.exists(file.path(d, "aeme.0.par.csv")))
  # the obscov we wrote was actually loaded
  rec <- readLines(file.path(d, "aeme.rec"), warn = FALSE)
  expect_true(any(grepl("obscov", rec, ignore.case = TRUE)))
})

test_that("pestpp-ies resumes from a restart_from directory (live)", {
  skip_if(!have_pest())
  first <- withr::local_tempdir()
  c1 <- create_pest_control(exe = "pestpp-ies", pest_dir = first, case = "aeme",
                            ncore = 1, noptmax = 1, ies_num_reals = 6, seed = 1)
  s1 <- pest_stub_dir(c1)
  exe <- pest_exe_path("pestpp-ies")
  st1 <- withr::with_dir(first, system2(exe, "aeme.pst", stdout = FALSE,
                                        stderr = FALSE))
  skip_if(st1 != 0, "first pestpp-ies run did not complete")
  expect_true(file.exists(file.path(first, "aeme.1.par.csv")))

  second <- withr::local_tempdir()
  c2 <- create_pest_control(exe = "pestpp-ies", pest_dir = second, case = "aeme",
                            ncore = 1, noptmax = 1, ies_num_reals = 6,
                            restart_from = first)
  s2 <- pest_stub_dir(c2)
  expect_equal(s2$ctrl$pestpp_options$ies_restart_observation_ensemble,
               "restart_obs.csv")
  st2 <- withr::with_dir(second, system2(exe, "aeme.pst", stdout = FALSE,
                                         stderr = FALSE))
  expect_equal(st2, 0)
})

test_that(".pest_want_par_en: opt-in triggers only", {
  base <- create_pest_control(exe = "pestpp-ies", ncore = 1)
  expect_false(.pest_want_par_en(base))

  expect_true(.pest_want_par_en(create_pest_control(seed = 1, ncore = 1)))
  expect_true(.pest_want_par_en(
    create_pest_control(prior_dist = "normal", ncore = 1)))
  expect_true(.pest_want_par_en(
    create_pest_control(noise_sd = c(HYD_temp = 0.5), ncore = 1)))
  expect_false(.pest_want_par_en(
    create_pest_control(prior_par_ensemble = FALSE, seed = 1, ncore = 1)))
})
