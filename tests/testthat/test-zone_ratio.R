make_flux <- function(value = -30, name = "aed_sed_const2d/fsed_oxy") {
  data.frame(model = "glm_aed", file = "aed.nml", name = name, value = value,
             min = value * 2, max = value / 6, group = NA_character_,
             index = 1L, stringsAsFactors = FALSE)
}

test_that("zone_ratio_param builds an anchor plus ratio rows", {
  p <- zone_ratio_param(make_flux(), n_zones = 3)
  expect_equal(nrow(p), 3)
  expect_equal(p$index, 1:3)
  expect_equal(p$name, c("aed_sed_const2d/fsed_oxy",
                         "aed_sed_const2d/fsed_oxy_zratio",
                         "aed_sed_const2d/fsed_oxy_zratio"))
  expect_equal(p$min[2:3], c(0.1, 0.1))
  expect_equal(p$max[2:3], c(1, 1))
  expect_equal(p$value[2:3], c(0.5, 0.5))
})

test_that("zone_ratio_param derives start ratios from a per-zone block", {
  blk <- do.call(rbind, lapply(1:3, function(i) {
    x <- make_flux(c(-30, -15, -3)[i]); x$index <- i; x
  }))
  p <- zone_ratio_param(blk, n_zones = 3)
  expect_equal(p$value[2:3], c(0.5, 0.2))
  # clipped to bounds
  blk$value <- c(-30, -1, -29)
  p <- zone_ratio_param(blk, n_zones = 3)
  expect_equal(p$value[2:3], c(0.1, 1))
})

test_that("zone_ratio_param validates inputs", {
  expect_error(zone_ratio_param(make_flux(), n_zones = 1))
  expect_error(zone_ratio_param(make_flux(), n_zones = 3, lower = 0))
  expect_error(zone_ratio_param(make_flux(), n_zones = 3, upper = 2))
})

test_that("expand_zone_ratios gives monotone per-zone values", {
  p <- zone_ratio_param(make_flux(-30), n_zones = 3)
  p$value[2:3] <- c(0.5, 0.2)
  e <- expand_zone_ratios(p)
  expect_equal(nrow(e), 3)
  expect_false(any(grepl("zratio", e$name)))
  expect_equal(e$index, 1:3)
  expect_equal(e$value, c(-30, -15, -3))
  expect_true(all(diff(abs(e$value)) <= 0))
})

test_that("expand_zone_ratios is monotone for any ratios in bounds", {
  set.seed(1)
  p <- zone_ratio_param(make_flux(20), n_zones = 4)
  for (i in 1:50) {
    p$value[2:4] <- runif(3, 0.1, 1)
    e <- expand_zone_ratios(p)
    expect_true(all(diff(e$value) <= 0))
  }
})

test_that("expand_zone_ratios handles several parameters and leaves others", {
  a <- zone_ratio_param(make_flux(-30, "aed_sed_const2d/fsed_oxy"), 2)
  b <- zone_ratio_param(make_flux(-4, "aed_sed_const2d/fsed_amm"), 3)
  other <- make_flux(1, "aed_oxygen/ksed_oxy"); other$index <- NA_integer_
  p <- rbind(a, b, other)
  p$value[p$name == "aed_sed_const2d/fsed_oxy_zratio"] <- 0.5
  p$value[p$name == "aed_sed_const2d/fsed_amm_zratio"] <- c(0.5, 0.5)
  e <- expand_zone_ratios(p)
  expect_equal(nrow(e), 2 + 3 + 1)
  expect_equal(e$value[e$name == "aed_sed_const2d/fsed_amm"], c(-4, -2, -1))
  expect_equal(e$value[e$name == "aed_oxygen/ksed_oxy"], 1)
})

test_that("expand_zone_ratios returns param unchanged without ratio rows", {
  p <- make_flux()
  expect_identical(expand_zone_ratios(p), p)
})

test_that("expand_zone_ratios errors without an anchor", {
  p <- zone_ratio_param(make_flux(), n_zones = 3)[-1, ]
  expect_error(expand_zone_ratios(p), "anchor")
})

test_that("run_aeme_param expands zone ratios before writing parameters", {
  path <- tempdir()
  captured <- NULL
  testthat::local_mocked_bindings(
    configuration = function(...) list(model_controls = NULL,
                                       path = normalizePath(path)),
    get_lake_dir = function(...) path,
    input = function(...) list(),
    input_model_parameters = function(aeme, model, param, path) {
      captured <<- param
      stop("stop after capture")
    },
    .package = "AEME"
  )
  zp <- zone_ratio_param(make_flux(-30), n_zones = 3)
  zp$value <- c(-30, 0.5, 0.2)
  expect_error(
    run_aeme_param(aeme = NULL, param = zp, model = "glm_aed", path = path),
    "stop after capture"
  )
  expect_equal(captured$index, 1:3)
  expect_equal(captured$value, c(-30, -15, -3))
  expect_false(any(grepl("zratio", captured$name)))
})

test_that("check_param_targets treats zone-ratio rows as their anchor", {
  aeme <- list(glm_aed = list(aed_sed_const2d = list(fsed_oxy = 1)))
  testthat::local_mocked_bindings(
    configuration = function(...) aeme, .package = "AEME"
  )
  zp <- zone_ratio_param(make_flux(-30), n_zones = 3)
  expect_silent(check_param_targets(zp, aeme = NULL, model = "glm_aed"))
})

make_temp <- function(value = 12) {
  data.frame(model = "glm_aed", file = "glm4.nml",
             name = "sediment/sed_temp_mean", value = value, min = 5,
             max = 25, group = NA_character_, index = 1L,
             stringsAsFactors = FALSE)
}

test_that("zone_offset_param builds an anchor plus offset rows", {
  p <- zone_offset_param(make_temp(), n_zones = 3)
  expect_equal(nrow(p), 3)
  expect_equal(p$index, 1:3)
  expect_equal(p$name, c("sediment/sed_temp_mean",
                         "sediment/sed_temp_mean_zoffset",
                         "sediment/sed_temp_mean_zoffset"))
  expect_equal(p$min[2:3], c(0, 0))
  expect_equal(p$max[2:3], c(5, 5))
  expect_equal(p$value[2:3], c(1, 1))
})

test_that("zone_offset_param derives start offsets from a per-zone block", {
  blk <- do.call(rbind, lapply(1:3, function(i) {
    x <- make_temp(c(10, 12, 20)[i]); x$index <- i; x
  }))
  p <- zone_offset_param(blk, n_zones = 3)
  expect_equal(p$value[2:3], c(2, 5))  # 8 clipped to the upper bound
  blk$value <- c(10, 9, 11)
  p <- zone_offset_param(blk, n_zones = 3)
  expect_equal(p$value[2:3], c(0, 2))  # -1 clipped to the lower bound
})

test_that("zone_offset_param validates inputs", {
  expect_error(zone_offset_param(make_temp(), n_zones = 1))
  expect_error(zone_offset_param(make_temp(), n_zones = 3, lower = 5, upper = 1))
})

test_that("expand_zone_ratios adds offsets cumulatively", {
  p <- zone_offset_param(make_temp(10), n_zones = 4)
  p$value[2:4] <- c(1, 0, 2.5)
  e <- expand_zone_ratios(p)
  expect_equal(e$index, 1:4)
  expect_false(any(grepl("zoffset", e$name)))
  expect_equal(e$value, c(10, 11, 11, 13.5))
  expect_true(all(diff(e$value) >= 0))
})

test_that("expand_zone_ratios handles ratios and offsets together", {
  temp <- zone_offset_param(make_temp(10), n_zones = 3)
  temp$value[2:3] <- c(1, 2)
  oxy <- zone_ratio_param(make_flux(-40), n_zones = 3)
  oxy$value[2:3] <- 0.5
  e <- expand_zone_ratios(rbind(temp, oxy))
  expect_equal(e$value[e$name == "sediment/sed_temp_mean"], c(10, 11, 13))
  expect_equal(e$value[e$name == "aed_sed_const2d/fsed_oxy"], c(-40, -20, -10))
})

test_that("expand_zone_ratios rejects mixed ratio and offset for one parameter", {
  p <- zone_offset_param(make_temp(10), n_zones = 3)
  r <- zone_ratio_param(make_temp(10), n_zones = 3)
  expect_error(expand_zone_ratios(rbind(p, r[r$index == 3, ])), "mixes")
})
