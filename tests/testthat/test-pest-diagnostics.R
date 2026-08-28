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
