# Unit tests for the goodness-of-fit functions in R/fit_functions.R.
#
# The key contract: nse()/kge()/kge_prime()/log_kge() return the conventional
# statistic (1 = perfect fit, higher is better), while their *_loss companions
# return -1 * that value for use as minimise-oriented FUN_list objectives.
# mae()/rmse()/pbias() are already 0-is-best and have no _loss companion.

perfect <- data.frame(obs = c(1, 2, 3, 4), model = c(1, 2, 3, 4))
df <- data.frame(obs = c(1, 2, 3, 4), model = c(1.1, 2.1, 2.9, 4.2))
worse <- data.frame(obs = c(1, 2, 3, 4), model = c(1.5, 2.6, 2.2, 5.1))

test_that("maximise-oriented metrics are 1 for a perfect fit", {
  expect_equal(nse(perfect), 1)
  expect_equal(kge(perfect), 1)
  expect_equal(kge_prime(perfect), 1)
  expect_equal(log_kge(perfect), 1)
})

test_that("error metrics are 0 for a perfect fit", {
  expect_equal(mae(perfect), 0)
  expect_equal(rmse(perfect), 0)
  expect_equal(pbias(perfect), 0)
})

test_that("nse() matches its hand-computed definition", {
  # sum((obs - sim)^2) = 0.07 ; sum((obs - mean(obs))^2) = 5
  expect_equal(nse(df), 1 - 0.07 / 5)
})

test_that("kge() matches its hand-computed definition", {
  r <- stats::cor(df$obs, df$model)
  alpha <- stats::sd(df$model) / stats::sd(df$obs)
  beta <- mean(df$model) / mean(df$obs)
  expect_equal(kge(df), 1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2))
})

test_that("_loss companions return -1 * the base statistic", {
  expect_equal(nse_loss(df), -1 * nse(df))
  expect_equal(kge_loss(df), -1 * kge(df))
  expect_equal(kge_prime_loss(df), -1 * kge_prime(df))
  expect_equal(log_kge_loss(df), -1 * log_kge(df))
})

test_that("orientation: a closer fit scores higher for nse(), lower for nse_loss()", {
  expect_gt(nse(df), nse(worse))
  expect_lt(nse_loss(df), nse_loss(worse))
  expect_lt(kge_loss(df), kge_loss(worse))
})

test_that("rows with NA in obs or model are dropped before calculating", {
  df_na <- rbind(df,
                 data.frame(obs = c(NA, 5), model = c(5, NA)))
  expect_equal(nse(df_na), nse(df))
  expect_equal(kge(df_na), kge(df))
  expect_equal(mae(df_na), mae(df))
  expect_equal(rmse(df_na), rmse(df))
})

test_that("bias() returns a signed value in native units", {
  under <- data.frame(obs = c(2, 3, 4), model = c(1, 2, 3))
  expect_equal(bias(under), -1)
  over <- data.frame(obs = c(2, 3, 4), model = c(3, 4, 5))
  expect_equal(bias(over), 1)
})

test_that("pbias() is the absolute percent bias", {
  # sum(sim - obs) = 0.3 ; sum(obs) = 10 -> 3 %
  expect_equal(pbias(df), 3)
  # equally-sized under-estimate gives the same (absolute) value
  df_under <- data.frame(obs = df$obs, model = df$obs - (df$model - df$obs))
  expect_equal(pbias(df_under), pbias(df))
})
