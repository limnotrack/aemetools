# Synthetic annual cycles: bottom band (depth 7-13 m) mean 11, amplitude 2,
# peak day 90; top band (0-7 m) mean 16, amplitude 5, peak day 40
make_obs <- function(n_per = 24, months_only = NULL) {
  doy <- round(seq(5, 360, length.out = n_per))
  dates <- as.Date("2020-01-01") + doy - 1
  w <- 2 * pi / 365.25
  d <- rbind(
    data.frame(depth = 11, Date = dates,
               value = 11 + 2 * cos(w * (doy - 90))),
    data.frame(depth = 2, Date = dates,
               value = 16 + 5 * cos(w * (doy - 40)))
  )
  d$var_aeme <- "HYD_temp"
  if (!is.null(months_only)) {
    d <- d[as.integer(format(d$Date, "%m")) %in% months_only, ]
  }
  d
}

temp_param <- function(name = "sediment/sed_temp_mean", value = 12, min = 5,
                       max = 25, index = 1L) {
  data.frame(model = "glm_aed", file = "glm4.nml", name = name, value = value,
             min = min, max = max, group = NA_character_, index = index,
             stringsAsFactors = FALSE)
}

est <- function(p, obs = make_obs(), ...) {
  estimate_zone_start(aeme = NULL, param = p, obs = obs,
                      zone_heights = c(6, 14), max_depth = 13, ...)
}

test_that("harmonic fit recovers mean, amplitude and peak day per zone", {
  p <- rbind(
    temp_param("sediment/sed_temp_mean", index = 1:2),
    temp_param("sediment/sed_temp_amplitude", value = 8, min = 0.1, max = 12,
               index = 1:2),
    temp_param("sediment/sed_temp_peak_doy", value = 30, min = 1, max = 365,
               index = 1:2)
  )
  r <- est(p)
  e <- attr(r, "zone_estimates")
  expect_equal(e$method, c("harmonic", "harmonic"))
  expect_equal(e$mean, c(11, 16), tolerance = 1e-6)
  expect_equal(e$amplitude, c(2, 5), tolerance = 1e-6)
  expect_equal(e$peak_doy, c(90, 40), tolerance = 0.5)
  expect_equal(r$value[r$name == "sediment/sed_temp_mean"], c(11, 16),
               tolerance = 1e-6)
  expect_equal(r$value[r$name == "sediment/sed_temp_amplitude"], c(2, 5),
               tolerance = 1e-6)
  expect_equal(r$value[r$name == "sediment/sed_temp_peak_doy"], c(90, 40),
               tolerance = 0.5)
})

test_that("zone depth bands follow zone_heights", {
  e <- attr(est(temp_param(index = 1L)), "zone_estimates")
  expect_equal(e$depth_top, c(7, 0))
  expect_equal(e$depth_bottom, c(13, 7))
  expect_equal(e$n_obs, c(24L, 24L))
})

test_that("anchor + offset rows take zone 1 and the difference to the next", {
  p <- zone_offset_param(temp_param(), n_zones = 2, lower = 0, upper = 10)
  r <- est(p)
  expect_equal(r$value, c(11, 5), tolerance = 1e-6)
})

test_that("offsets are clipped to their bounds with a warning", {
  p <- zone_offset_param(temp_param(), n_zones = 2, lower = 0, upper = 3)
  expect_warning(r <- est(p), "clipped")
  expect_equal(r$value, c(11, 3), tolerance = 1e-6)
})

test_that("anchor + ratio rows take the ratio between zones", {
  # zone 2 / zone 1 = 16 / 11 is above the ratio bound of 1, so it is clipped
  p <- zone_ratio_param(temp_param(), n_zones = 2)
  expect_warning(r <- est(p), "clipped")
  expect_equal(r$value, c(11, 1), tolerance = 1e-6)
  # a ratio inside the bounds is used as is
  obs <- make_obs()
  obs$value[obs$depth == 2] <- obs$value[obs$depth == 2] - 8
  r <- est(p, obs = obs)
  expect_equal(r$value, c(11, 8 / 11), tolerance = 1e-6)
})

test_that("a scalar row takes the mean over zones", {
  r <- est(temp_param(index = NA_integer_))
  expect_equal(r$value, 13.5, tolerance = 1e-6)
})

test_that("sparse seasonal coverage falls back to the plain mean", {
  p <- temp_param(index = 1:2)
  r <- est(p, obs = make_obs(48, months_only = 6:8))
  e <- attr(r, "zone_estimates")
  expect_equal(e$method, c("mean", "mean"))
  expect_equal(e$amplitude, c(NA_real_, NA_real_))
  expect_equal(r$value, e$mean)
})

test_that("zones with too few observations are left unchanged", {
  obs <- make_obs()
  obs <- obs[!(obs$depth == 2 & seq_len(nrow(obs)) %% 24 != 0), ]
  r <- est(temp_param(index = 1:2), obs = obs)
  expect_equal(r$value[1], 11, tolerance = 1e-6)
  expect_equal(r$value[2], 12)
})

test_that("width re-centres the bounds on the estimate", {
  r <- est(temp_param(index = 1:2), width = c(sed_temp_mean = 3))
  expect_equal(r$min, c(8, 13), tolerance = 1e-6)
  expect_equal(r$max, c(14, 19), tolerance = 1e-6)
})

test_that("other parameters are untouched and inputs are validated", {
  p <- rbind(temp_param(index = 1:2),
             temp_param("light/Kw", value = 0.4, min = 0.1, max = 1,
                        index = NA_integer_))
  r <- est(p)
  expect_equal(r$value[3], 0.4)
  expect_error(estimate_zone_start(NULL, p, obs = make_obs(),
                                   max_depth = 13), "zone_heights")
  aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
  aeme <- readRDS(aeme_file)
  path <- withr::local_tempdir()
  aeme <- AEME::build_aeme(aeme, "glm_aed", path = path, ext_elev = 3)
  est <- estimate_zone_start(aeme, p)
  testthat::expect_true(inherits(est, "data.frame"))
  bt <- est$value[est$name == "sediment/sed_temp_mean" & est$index == 1] 
  st <- est$value[est$name == "sediment/sed_temp_mean" & est$index == 2]
  testthat::expect_true(bt < st)
})


# ---- estimate_zone_flux_start ------------------------------------------------
flux_est <- list(fsed_oxy = c(-39, -19.5), fsed_amm = c(5.7, 0.5),
                 fsed_nit = c(-0.4, 0.1), fsed_frp = c(0.107, 0.027))

flux_param <- function(key = "fsed_oxy", value = -25, min = -60, max = -5,
                       index = 1L, block = "aed_sed_const2d") {
  data.frame(model = "glm_aed", file = "aed.nml",
             name = paste0(block, "/", key), value = value, min = min,
             max = max, group = NA_character_, index = index,
             stringsAsFactors = FALSE)
}

test_that("flux estimates fill independent per-zone rows", {
  p <- flux_param(index = 1:2)
  r <- estimate_zone_flux_start(NULL, p, fluxes = flux_est)
  expect_equal(r$value, c(-39, -19.5))
  expect_equal(attr(r, "zone_fluxes")$fsed_amm, c(5.7, 0.5))
})

test_that("flux estimates fill an anchor + ratio set", {
  p <- zone_ratio_param(flux_param(), n_zones = 2)
  r <- estimate_zone_flux_start(NULL, p, fluxes = flux_est)
  expect_equal(r$value, c(-39, 0.5))
})

test_that("flux estimates fill an anchor + signed offset set", {
  p <- zone_offset_param(flux_param("fsed_amm", 2, 0, 10), n_zones = 2,
                         lower = -8, upper = 0)
  r <- estimate_zone_flux_start(NULL, p, fluxes = flux_est)
  expect_equal(r$value, c(5.7, -5.2))
  # signed offset takes the value through zone 1 to zone 2
  expect_equal(expand_zone_ratios(r)$value, c(5.7, 0.5))
})

test_that("a ratio across a sign change is clipped with a warning", {
  p <- zone_ratio_param(flux_param("fsed_nit", -0.2, -1, 0), n_zones = 2)
  expect_warning(r <- estimate_zone_flux_start(NULL, p, fluxes = flux_est),
                 "clipped")
  expect_equal(r$value[2], 0.1)  # ratio 0.1/-0.4 < 0 clipped to 0.1
})

test_that("only the zoned aed_sed_const2d rows are changed", {
  p <- rbind(flux_param(index = 1:2),
             flux_param("fsed_oxy", -25, -60, -5, index = NA_integer_,
                        block = "aed_oxygen"))
  r <- estimate_zone_flux_start(NULL, p, fluxes = flux_est)
  expect_equal(r$value, c(-39, -19.5, -25))
})

test_that("flux width re-centres bounds and indices past the zones are skipped", {
  p <- flux_param(index = 1:3)
  r <- estimate_zone_flux_start(NULL, p, fluxes = flux_est,
                                width = c(fsed_oxy = 5))
  expect_equal(r$value[1:2], c(-39, -19.5))
  expect_equal(r$min[1:2], c(-44, -24.5))
  expect_equal(r$value[3], -25)
})

test_that("flux estimation validates its inputs", {
  expect_error(estimate_zone_flux_start(NULL, flux_param()), "fluxes")
  expect_error(estimate_zone_flux_start(NULL, flux_param(),
                                        fluxes = list(fsed_oxy = 1)),
               "must contain")
})

test_that("flux estimation calls AEME::estimate_zone_fluxes when not supplied", {
  called <- NULL
  testthat::local_mocked_bindings(
    estimate_zone_fluxes = function(...) {
      called <<- list(...)
      c(flux_est, list(zone_summary = NULL, method = "baseline_scaled"))
    },
    .package = "AEME"
  )
  r <- estimate_zone_flux_start("fake", flux_param(index = 1:2),
                                ref_depth = 4)
  expect_false(called$verbose)
  expect_equal(called$ref_depth, 4)
  expect_equal(r$value, c(-39, -19.5))
})
