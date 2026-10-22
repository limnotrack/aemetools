# Tests for small internal helpers in R/utils.R.

test_that("normalise_lake_obs collapses either depth schema to `depth`", {
  expect_null(normalise_lake_obs(NULL))

  new <- data.frame(depth = c("0", "5"), var_aeme = "HYD_temp")
  expect_type(normalise_lake_obs(new)$depth, "double")

  old <- data.frame(depth_from = c(0, 4), depth_to = c(2, 6))
  expect_equal(normalise_lake_obs(old)$depth, c(1, 5))
})

test_that("aeme_make_cluster raises the worker-startup timeout", {
  skip_on_cran()

  # Default: 600 s, well past parallel::makeCluster()'s own 120 s, so a
  # worker still building the renv sandbox is not mistaken for a dead one.
  cl <- aeme_make_cluster(1L, outfile = nullfile())
  on.exit(parallel::stopCluster(cl), add = TRUE)
  expect_s3_class(cl, "cluster")
  expect_equal(parallel::clusterEvalQ(cl, 1 + 1)[[1]], 2)

  # A garbage override falls back to the default rather than erroring.
  withr::local_envvar(AEMETOOLS_CLUSTER_SETUP_TIMEOUT = "not-a-number")
  cl2 <- aeme_make_cluster(1L, outfile = nullfile())
  on.exit(parallel::stopCluster(cl2), add = TRUE)
  expect_s3_class(cl2, "cluster")
})
