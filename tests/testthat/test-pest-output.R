# Tests for reading, interrogating and plotting PEST++ output.
#
# All of these build a synthetic PEST++ run directory, so they need neither
# a solver binary nor a model run. The file formats are taken from the
# PEST++ sources: Ensemble::to_csv() writes "real_name" then lowercased
# parameter/observation columns, and L2PhiHandler::prepare_csv() writes
# "iteration,total_runs,mean,standard_deviation,min,max" then one column
# per realisation.
#
# `pest_param()` and `make_pest_dir()` now live in helper-pest.R so
# test-pest-posterior-params.R can share them.

test_that("read_pest_phi reads the objective-function trajectory", {
  s <- make_pest_dir()
  phi <- read_pest_phi(s$ctrl)

  expect_equal(nrow(phi), 3L)
  expect_true(all(c("iteration", "total_runs", "mean", "standard_deviation",
                    "min", "max") %in% names(phi)))
  expect_equal(phi$iteration, c(0, 1, 2))
  # total_runs is cumulative, not per-iteration.
  expect_true(all(diff(phi$total_runs) > 0))
  expect_equal(phi$min, c(8, 3, 1))

  expect_error(read_pest_phi(s$ctrl, type = "composite"), "No phi file")
  expect_error(read_pest_phi(s$ctrl, type = "nope"), "must be one of")
})

test_that("read_pest_ensemble maps PEST names back to aemetools identities", {
  s <- make_pest_dir()

  # Default is the last iteration written: the posterior.
  post <- read_pest_ensemble(s$ctrl)
  expect_equal(unique(post$iteration), 2)
  expect_setequal(unique(post$name_full),
                  encode_param(s$param$group, s$param$name, s$param$index))
  expect_equal(nrow(post), 4L * 2L)
  expect_setequal(unique(post$realisation),
                  c("real_0", "real_1", "real_2", "base"))

  prior <- read_pest_ensemble(s$ctrl, iteration = 0)
  expect_equal(unique(prior$iteration), 0)
  # Prior for p001 is wide, posterior narrow.
  kw <- encode_param("light", "Kw", 1L)
  expect_gt(stats::sd(prior$value[prior$name_full == kw]),
            stats::sd(post$value[post$name_full == kw]))

  obs <- read_pest_ensemble(s$ctrl, type = "obs")
  expect_true(all(c("obsnme", "var_aeme", "Date", "depth", "model") %in%
                    names(obs)))
  expect_equal(unique(obs$var_aeme), "HYD_temp")
  expect_s3_class(obs$Date, "Date")

  expect_error(read_pest_ensemble(s$ctrl, iteration = 99), "available")
})

test_that("pest_param_summary reports variance reduction", {
  s <- make_pest_dir()
  sm <- pest_param_summary(s$ctrl, s$param)

  expect_equal(nrow(sm), 2L)
  expect_true(all(c("prior_sd", "post_sd", "variance_reduction",
                    "bound_frac") %in% names(sm)))

  kw <- encode_param("light", "Kw", 1L)
  ce <- encode_param("light", "ce", NA_integer_)

  # Kw's posterior is much narrower than its prior: strongly informed.
  expect_gt(sm$variance_reduction[sm$name_full == kw], 0.8)
  # ce's posterior is identical to its prior: the data said nothing.
  expect_equal(sm$variance_reduction[sm$name_full == ce], 0)

  # Ordered most-informed first, which is how it is meant to be read.
  expect_equal(sm$name_full[1], kw)
  # Posterior spread as a fraction of the allowed range.
  expect_lt(sm$bound_frac[sm$name_full == kw], 0.2)
})

test_that("pest_residuals joins simulated to observed values", {
  s <- make_pest_dir()

  # Observed values recovered from the .pst when obs_tbl is not supplied.
  r <- pest_residuals(s$ctrl)
  expect_equal(nrow(r), 4L * 3L)
  expect_true(all(c("obs", "model", "residual") %in% names(r)))
  expect_equal(r$residual, r$model - r$obs)
  expect_setequal(unique(r$obs), s$ot$obsval)

  # Supplying obs_tbl must give the same answer.
  r2 <- pest_residuals(s$ctrl, obs_tbl = s$ot)
  expect_equal(r2$residual, r$residual)

  # The prior should fit worse than the posterior.
  prior <- pest_residuals(s$ctrl, iteration = 0, obs_tbl = s$ot)
  expect_gt(mean(abs(prior$residual)), mean(abs(r$residual)))
})

test_that("plotting functions return ggplot objects", {
  s <- make_pest_dir()

  expect_true(ggplot2::is_ggplot(plot_pest_phi(s$ctrl)))
  expect_true(ggplot2::is_ggplot(plot_pest_phi(s$ctrl, log_y = FALSE)))
  expect_true(ggplot2::is_ggplot(plot_pest_ensemble(s$ctrl, s$param)))
  expect_true(ggplot2::is_ggplot(plot_pest_ensemble(s$ctrl, s$param,
                                                    scaled = TRUE)))
  expect_true(ggplot2::is_ggplot(plot_pest_residuals(s$ctrl,
                                                     obs_tbl = s$ot)))
  expect_true(ggplot2::is_ggplot(plot_pest_residuals(s$ctrl, obs_tbl = s$ot,
                                                     type = "time")))

  # Building is what catches bad aes/facet references; is_ggplot alone does
  # not evaluate the plot.
  expect_no_error(ggplot2::ggplot_build(plot_pest_phi(s$ctrl)))
  expect_no_error(ggplot2::ggplot_build(plot_pest_ensemble(s$ctrl, s$param)))
  expect_no_error(ggplot2::ggplot_build(plot_pest_residuals(s$ctrl,
                                                            obs_tbl = s$ot)))
})

test_that("a zero phi falls back from the log axis rather than dropping it", {
  s <- make_pest_dir()
  writeLines(c("iteration,total_runs,mean,standard_deviation,min,max,real_0",
               "0,4,12.0,2.0,0.0,20.0,0.0"),
             file.path(s$d, "aeme.phi.actual.csv"))
  p <- plot_pest_phi(s$ctrl, log_y = TRUE)
  # log10(0) is -Inf, so the point would be silently discarded.
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("pest_posterior_runs finds the final ensemble, not the last gen", {
  s <- make_pest_dir()
  param <- s$param
  param$name_full <- encode_param(param$group, param$name, param$index)

  # A run log containing the posterior members plus extra rejected
  # candidates - which is what the lambda-testing runs actually are.
  post_vals <- data.frame(p001 = c(0.68, 0.72, 0.76, 0.80),
                          p002 = c(0.001, 0.002, 0.003, 0.004))
  res <- data.frame(
    a = c(0.99, post_vals$p001, 0.11),
    b = c(0.0009, post_vals$p002, 0.0044)
  )
  names(res) <- param$name_full
  res$HYD_temp <- 1
  res$fit <- 1

  pr <- pest_posterior_runs(s$ctrl, param, res)

  # Exactly the four posterior members, and not the two rejected rows.
  expect_equal(nrow(pr), 4L)
  expect_equal(sort(pr$run), 2:5)
  expect_setequal(pr$realisation, c("real_0", "real_1", "real_2", "base"))
  expect_equal(unique(pr$iteration), 2L)

  # Filtering the results by final generation is NOT the posterior - that
  # is the whole reason this function exists.
  expect_lt(nrow(pr), nrow(res))
})

test_that("pest_posterior_runs warns rather than silently returning empty", {
  s <- make_pest_dir()
  param <- s$param
  param$name_full <- encode_param(param$group, param$name, param$index)

  # A run log that shares no parameter vector with the ensemble.
  res <- data.frame(a = c(9, 9), b = c(9, 9))
  names(res) <- param$name_full

  expect_warning(pr <- pest_posterior_runs(s$ctrl, param, res),
                 regexp = NA)  # cli warning, not a base warning
  expect_equal(nrow(pr), 0L)
})

test_that("posterior membership is stored and read back", {
  s <- make_pest_dir()
  out_dir <- withr::local_tempdir()
  ctrl <- s$ctrl
  ctrl$file_dir <- out_dir
  ctrl$file_type <- "csv"

  post <- data.frame(run = 2:5, realisation = c("real_0", "real_1", "real_2",
                                                "base"),
                     iteration = 2L, stringsAsFactors = FALSE)
  aemetools:::.pest_write_posterior(ctrl, sim_id = "LID1_glmaed_C_001",
                                    post = post)

  f <- file.path(out_dir, "pest_posterior.csv")
  expect_true(file.exists(f))
  stored <- read.csv(f, stringsAsFactors = FALSE)
  expect_equal(nrow(stored), 4L)
  expect_equal(stored$sim_id[1], "LID1_glmaed_C_001")
  expect_setequal(stored$run, 2:5)

  # An empty posterior must not create the table at all.
  out2 <- withr::local_tempdir()
  ctrl2 <- ctrl; ctrl2$file_dir <- out2
  aemetools:::.pest_write_posterior(ctrl2, "x", post[0, ])
  expect_false(file.exists(file.path(out2, "pest_posterior.csv")))
})

test_that("calibration metadata records where the PEST artefacts are", {
  d <- withr::local_tempdir()
  ctrl <- create_pest_control(file_dir = d, ncore = 1, file_type = "csv",
                              pest_dir = "some/pest/dir")
  ctrl$sim_id <- "LID1_glmaed_C_001"
  aemetools:::write_calib_metadata(ctrl = ctrl, nsim = 6, t0 = Sys.time() - 5)

  m <- read.csv(file.path(d, "calibration_metadata.csv"),
                stringsAsFactors = FALSE)
  # .rei residuals, the Jacobian and FOSM covariances live only on disk, so
  # a sim_id that cannot be traced back to its directory loses them.
  expect_equal(m$pest_dir, "some/pest/dir")
  expect_equal(m$engine, "pest")

  # A built-in control has neither, and must still write.
  d2 <- withr::local_tempdir()
  c2 <- create_calib_control(NP = 4, itermax = 8, file_dir = d2,
                             file_type = "csv")
  c2$sim_id <- "LID1_glmaed_C_002"
  expect_no_error(
    aemetools:::write_calib_metadata(ctrl = c2, nsim = 8, t0 = Sys.time() - 5))
  m2 <- read.csv(file.path(d2, "calibration_metadata.csv"),
                 stringsAsFactors = FALSE)
  expect_true(is.na(m2$pest_dir))
  expect_true(is.na(m2$engine))
})

# Values taken verbatim from a real pestpp-ies 5.2.16 run: the ensemble
# CSVs carry ~6 significant digits while the run log holds full double
# precision. Matching them is the whole job, and comparing exactly (or on a
# rounded string) matches nothing - which failed silently, leaving an empty
# posterior and every run filed under generation 1.
real_ens_0 <- c(
  "real_name,p001,p002,p003",
  "0,0.603455,0.137693,0.542435",
  "1,0.1,0.136919,0.792653",
  "2,0.756923,0.14397,0.8",
  "3,0.1,0.163254,0.757934",
  "4,0.736387,0.101703,0.721456",
  "base,0.58,0.14,0.74")
real_ens_1 <- c(
  "real_name,p001,p002,p003",
  "0,0.600038,0.137808,0.542875",
  "1,0.103761,0.136791,0.792175",
  "2,0.753399,0.14409,0.8",
  "3,0.103766,0.163126,0.757454",
  "4,0.732889,0.101821,0.721906",
  "base,0.57659,0.140115,0.74044")
real_runs <- rbind(
  c(0.603454746720440, 0.137692729043984, 0.542434608877816),
  c(0.100000000000000, 0.136919141489198, 0.792653418039881),
  c(0.756923489592606, 0.143970115239905, 0.800000000000000),
  c(0.100000000000000, 0.163253913238678, 0.757933755926205),
  c(0.736386602663662, 0.101702929876503, 0.721455625957213),
  c(0.580000000000000, 0.140000000000000, 0.740000000000000),
  c(0.600037982032610, 0.137808451809438, 0.542875353443461),
  c(0.103761478931134, 0.136791286929546, 0.792174573311806),
  c(0.753399343801413, 0.144089515365206, 0.800000000000000),
  c(0.103765900796581, 0.163125908887157, 0.757454338202297),
  c(0.732889147786733, 0.101821482113301, 0.721905876285865),
  c(0.576589749907079, 0.140115471864490, 0.740439942928034))

setup_real_dir <- function(env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  ctrl <- create_pest_control(pest_dir = d, case = "aeme", ncore = 1,
                              ies_num_reals = 6, noptmax = 1)
  param <- data.frame(
    model = "glm_aed", file = "glm4.nml", group = NA_character_,
    name = c("light/Kw", "mixing/coef_mix_conv", "mixing/coef_mix_hyp"),
    index = NA_integer_, value = 0.5, min = 0.1, max = 0.8,
    stringsAsFactors = FALSE)
  param$name_full <- encode_param(param$group, param$name, param$index)

  utils::write.csv(data.frame(parnme = c("p001", "p002", "p003"),
                              name_full = param$name_full,
                              model = "glm_aed", file = "glm4.nml"),
                   file.path(d, "aeme_par_map.csv"), row.names = FALSE)
  writeLines(real_ens_0, file.path(d, "aeme.0.par.csv"))
  writeLines(real_ens_1, file.path(d, "aeme.1.par.csv"))

  res <- as.data.frame(real_runs)
  names(res) <- param$name_full
  list(d = d, ctrl = ctrl, param = param, res = res)
}

test_that("ensemble rows match runs despite PEST++'s 6-digit CSV precision", {
  s <- setup_real_dir()
  idx <- aemetools:::.pest_match_rows(
    utils::read.csv(file.path(s$d, "aeme.1.par.csv"), check.names = FALSE) |>
      stats::setNames(c("real_name", s$param$name_full)),
    s$res, s$param$name_full)

  # All six posterior realisations resolve, to the second block of runs.
  expect_false(anyNA(idx))
  expect_setequal(idx, 7:12)
  # ...and each maps to a distinct run, not all to the same nearest one.
  expect_equal(length(unique(idx)), 6L)
})

test_that("generations are recovered, not silently defaulted to 1", {
  s <- setup_real_dir()
  gen <- aemetools:::.pest_assign_gen(s$res, s$ctrl, s$param)

  # PEST iteration 0 -> generation 1, iteration 1 -> generation 2.
  expect_equal(gen, c(rep(1L, 6), rep(2L, 6)))
  # The bug this guards: everything collapsing into a single generation.
  expect_gt(length(unique(gen)), 1L)
})

test_that("the posterior is the final ensemble, matched at real precision", {
  s <- setup_real_dir()
  pr <- pest_posterior_runs(s$ctrl, s$param, s$res)

  expect_equal(nrow(pr), 6L)
  expect_equal(unique(pr$iteration), 1L)
  expect_setequal(pr$realisation, c("0", "1", "2", "3", "4", "base"))
  # Posterior members are the later runs, not the prior ensemble.
  expect_setequal(pr$run, 7:12)
})

test_that("iteration ensembles are ordered numerically, not lexically", {
  s <- setup_real_dir()
  # aeme.10 must not be treated as earlier than aeme.2.
  file.copy(file.path(s$d, "aeme.1.par.csv"), file.path(s$d, "aeme.10.par.csv"))
  its <- vapply(strsplit(c("aeme.0.par.csv", "aeme.1.par.csv",
                           "aeme.2.par.csv", "aeme.10.par.csv"),
                         ".", fixed = TRUE),
                function(x) as.integer(x[2]), integer(1))
  expect_equal(order(its), c(1L, 2L, 3L, 4L))
  # The final ensemble is now iteration 10.
  pr <- pest_posterior_runs(s$ctrl, s$param, s$res)
  expect_equal(unique(pr$iteration), 10L)
})

test_that("readers find the run directory the calibration actually used", {
  # `pest_dir` defaults to a relative path and is resolved against the lake
  # directory when the run starts, so the control the caller still holds
  # points at "pest" while the files are under <lake_dir>/pest. The
  # resolved path is recorded in calibration_metadata, which is how the
  # readers are meant to find it.
  s <- make_pest_dir()

  # A plain path works.
  expect_equal(nrow(read_pest_phi(s$d)), 3L)

  # The calib object from read_calib() is the intended route.
  calib <- list(calibration_metadata = data.frame(pest_dir = s$d,
                                                  stringsAsFactors = FALSE))
  expect_equal(nrow(read_pest_phi(calib)), 3L)
  expect_gt(nrow(read_pest_ensemble(calib)), 0L)
  expect_true(ggplot2::is_ggplot(plot_pest_phi(calib)))

  # A control whose pest_dir is absolute still works, since nothing needs
  # resolving.
  expect_equal(nrow(read_pest_phi(s$ctrl)), 3L)

  # ...but an unresolved relative one must say why it cannot find them,
  # rather than reporting a missing file.
  bad <- create_pest_control(pest_dir = "pest", case = "aeme", ncore = 1)
  expect_error(read_pest_phi(bad), "lake directory")
  expect_error(read_pest_phi(bad), "read_calib")

  # The case name is inferred from the .pst when only a path is given.
  expect_equal(aemetools:::.pest_locate(s$d)$case, "aeme")

  # A calib from a non-PEST run has no directory to offer.
  expect_error(
    read_pest_phi(list(calibration_metadata = data.frame(pest_dir = NA))),
    "No PEST.. directory recorded")
})

# --- batch 2: phi-by-group, is_base, adjust weights, timeseries -------------

test_that("read_pest_phi_group breaks phi down by variable", {
  s <- make_pest_dir()
  g <- read_pest_phi_group(s$ctrl)

  expect_true(all(c("iteration", "total_runs", "obgnme", "var_aeme", "phi")
                  %in% names(g)))
  expect_equal(g$iteration, c(0, 1, 2))
  expect_equal(g$var_aeme, rep("HYD_temp", 3))   # mapped back from "hyd_temp"
  expect_equal(g$phi, c(12, 6, 2))
})

test_that("plot_pest_phi_group returns a ggplot", {
  s <- make_pest_dir()
  expect_true(ggplot2::is_ggplot(plot_pest_phi_group(s$ctrl)))
})

test_that("read_pest_ensemble flags the base realisation", {
  s <- make_pest_dir()
  post <- read_pest_ensemble(s$ctrl)
  expect_true("is_base" %in% names(post))
  expect_true(all(post$is_base[post$realisation == "base"]))
  expect_false(any(post$is_base[post$realisation != "base"]))

  obs <- read_pest_ensemble(s$ctrl, type = "obs")
  expect_true("is_base" %in% names(obs))
})

test_that("realisation is character even when a CSV has all-numeric labels", {
  # PEST++ quotes the realisation labels in some ensemble files and not
  # others, so read.csv types the first column as character for one
  # iteration (it contains "base") and integer for another (it does not).
  # Binding prior + posterior then aborted on the type mismatch.
  s <- make_pest_dir()
  ef <- aemetools:::.pest_ensemble_files(s$d, "aeme", "par")
  prior_f <- ef$path[ef$iteration == 0]
  ens <- utils::read.csv(prior_f, check.names = FALSE, stringsAsFactors = FALSE)
  ens[[1]] <- seq_len(nrow(ens)) - 1L          # 0, 1, 2, ... : reads as integer
  utils::write.csv(ens, prior_f, row.names = FALSE, quote = FALSE)

  prior <- read_pest_ensemble(s$ctrl, iteration = 0)
  post <- read_pest_ensemble(s$ctrl)
  expect_type(prior$realisation, "character")
  expect_type(post$realisation, "character")
  expect_s3_class(dplyr::bind_rows(prior, post), "data.frame")
  expect_no_error(ggplot2::ggplot_build(plot_pest_ensemble(s$ctrl, s$param)))
})

test_that("pest_param_summary reports the base realisation's posterior value", {
  s <- make_pest_dir()
  sm <- pest_param_summary(s$ctrl, s$param)
  expect_true("post_base" %in% names(sm))
  kw <- encode_param("light", "Kw", 1L)
  # base is the 4th realisation of the posterior ensemble for p001 -> 0.80
  expect_equal(sm$post_base[sm$name_full == kw], 0.80)
})

test_that("pest_adjust_weights rescales group weights toward n_obs", {
  s <- make_pest_dir()
  adj <- pest_adjust_weights(s$ot, s$ctrl)

  expect_equal(nrow(adj), nrow(s$ot))
  expect_s3_class(attr(adj, "map"), "data.frame")
  # single group, so all weights scale by the same factor
  expect_equal(length(unique(adj$weight / s$ot$weight)), 1L)
  # posterior misfit in the fixture is small, so weights go UP
  expect_gt(unique(adj$weight / s$ot$weight), 1)
})

test_that("plot_pest_timeseries returns a ggplot", {
  s <- make_pest_dir()
  p <- plot_pest_timeseries(s$ctrl, obs_tbl = s$ot)
  expect_true(ggplot2::is_ggplot(p))
  expect_error(plot_pest_timeseries(s$ctrl, ci = c(0.9, 0.1)), "increasing")
})
