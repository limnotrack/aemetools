# Tests for the PEST++ diagnostic readers. Synthetic run directories, so no
# solver binary or model run is needed.

test_that("pest_prior_data_conflict joins the pdc file to the observation map", {
  d <- withr::local_tempdir()

  # Minimal run directory: a .pst so .pest_locate() can infer the case,
  # plus the obs map write_pst() leaves beside it.
  writeLines("pcf", file.path(d, "aeme.pst"))
  utils::write.csv(
    data.frame(obsnme = sprintf("o%06d", 1:3),
               var_aeme = "HYD_temp",
               Date = as.Date(c("2020-01-01", "2020-01-01", "2020-02-01")),
               depth = c(0.5, 5.0, 0.5)),
    file.path(d, "aeme_obs_map.csv"), row.names = FALSE)

  # pestpp-ies writes <case>.pdc.csv with the observation name first.
  utils::write.csv(
    data.frame(name = c("o000002", "o000003"),
               group = "hyd_temp",
               mean = c(13.4, 9.8),
               stdev = c(0.2, 0.3),
               distance = c(3.1, 4.2)),
    file.path(d, "aeme.pdc.csv"), row.names = FALSE)

  pdc <- pest_prior_data_conflict(d)

  expect_equal(nrow(pdc), 2)
  expect_equal(pdc$obsnme, c("o000002", "o000003"))
  expect_equal(pdc$var_aeme, c("HYD_temp", "HYD_temp"))
  expect_equal(pdc$Date, as.Date(c("2020-01-01", "2020-02-01")))
  expect_equal(pdc$depth, c(5.0, 0.5))
  expect_true(all(c("mean", "stdev", "distance") %in% names(pdc)))
})

test_that("pest_prior_data_conflict returns an empty frame when no file was written", {
  d <- withr::local_tempdir()
  writeLines("pcf", file.path(d, "aeme.pst"))

  res <- pest_prior_data_conflict(d)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})

test_that("read_pest_phi_group reads the real per-realisation layout", {
  # Regression: pestpp-ies writes one row per realisation per iteration,
  # identified by obs_realization/par_realization. Those columns are read as
  # character, and pivoting them alongside the numeric group columns aborted
  # with "Can't combine `obs_realization` <character> and `chm_oxy`
  # <double>" - on every run with more than one observation group. Header
  # and values below are copied from a real pestpp-ies 5.2.16 run.
  d <- withr::local_tempdir()
  writeLines("pcf", file.path(d, "aeme.pst"))
  utils::write.csv(
    data.frame(obsnme = sprintf("o%06d", 1:2),
               var_aeme = c("HYD_temp", "CHM_oxy"),
               Date = as.Date("2020-01-01"), depth = c(0.5, 5)),
    file.path(d, "aeme_obs_map.csv"), row.names = FALSE)

  writeLines(c(
    "iteration,total_runs,obs_realization,par_realization,chm_oxy,hyd_temp",
    "0,8,0,0,3.1794,1.11345",
    "0,8,1,1,3.17727,0.589116",
    "1,16,0,0,3.10000,0.900000"),
    file.path(d, "aeme.phi.group.csv"))

  pg <- read_pest_phi_group(d)

  expect_s3_class(pg, "data.frame")
  # Two groups x three rows, and only the two groups become groups.
  expect_equal(nrow(pg), 6)
  expect_setequal(unique(pg$obgnme), c("chm_oxy", "hyd_temp"))
  expect_type(pg$phi, "double")
  expect_false(any(c("obs_realization", "par_realization") %in% pg$obgnme))

  # The realisation identifiers survive as metadata - per-realisation group
  # phi is the point of the file.
  expect_true(all(c("obs_realization", "par_realization") %in% names(pg)))
  expect_equal(pg$phi[pg$obgnme == "hyd_temp" & pg$iteration == 0],
               c(1.11345, 0.589116))

  # Group names map back to the AEME variables they came from.
  expect_equal(pg$var_aeme[pg$obgnme == "hyd_temp"][1], "HYD_temp")
  expect_equal(pg$var_aeme[pg$obgnme == "chm_oxy"][1], "CHM_oxy")
})
