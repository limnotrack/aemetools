# Characterisation + behaviour tests for run_and_fit(). Kept deliberately
# close to the integration level of the rest of this suite: a real build and
# a real model run, since run_and_fit()'s whole job is to turn a model run
# into a fit value and the failure modes only show up against real output.
#
# Two models are built once each and memoised for the file: glm_aed (netCDF
# output, the common path) and simstrat_aed (writes *_out.dat text which AEME
# converts to netCDF - exercised here through the same netCDF path).

.raf_test_cache <- new.env(parent = emptyenv())

# Build (and run once) an AEME object for `model`, memoised per file run.
# Built inline rather than via get_cached_aeme_run() so this file does not
# depend on that helper (which currently errors on a cache hit with
# run = TRUE).
raf_fixture <- function(model, vars_sim = "HYD_temp") {
  key <- paste(model, paste(vars_sim, collapse = ","), sep = "|")
  if (is.null(.raf_test_cache[[key]])) {
    aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
    mc <- AEME::set_vars_sim(AEME::get_model_controls(), vars_sim = vars_sim)
    path <- tempfile(paste0("raf_", model, "_"))
    dir.create(path, recursive = TRUE)
    aeme <- AEME::build_aeme(aeme = aeme, model = model, path = path,
                             model_controls = mc, ext_elev = 5,
                             use_bgc = FALSE)
    aeme <- AEME::run_aeme(aeme = aeme, model = model, path = path)
    .raf_test_cache[[key]] <- list(aeme = aeme, path = path, mc = mc)
  }
  .raf_test_cache[[key]]
}

raf_params <- function(model) {
  utils::data("aeme_parameters", package = "AEME", envir = environment())
  models <- if (model == "simstrat_aed") {
    c("simstrat_aed", "simstrat_aed2")
  } else {
    model
  }
  aeme_parameters[aeme_parameters$model %in% models &
                    aeme_parameters$name != "outflow", ]
}


test_that("calib mode returns one weighted fit component per variable", {
  fx <- raf_fixture("glm_aed")

  res <- run_and_fit(aeme = fx$aeme, param = raf_params("glm_aed"),
                     model = "glm_aed", vars_sim = "HYD_temp", path = fx$path,
                     model_controls = fx$mc, FUN_list = list(HYD_temp = mae),
                     weights = c(HYD_temp = 1), method = "calib")

  expect_type(res, "list")
  expect_named(res, "HYD_temp")
  expect_true(is.finite(res$HYD_temp))
  expect_gt(res$HYD_temp, 0)          # MAE of a real run is strictly positive
})

test_that("calib weight scales the fit component linearly", {
  fx <- raf_fixture("glm_aed")
  args <- list(aeme = fx$aeme, param = raf_params("glm_aed"), model = "glm_aed",
               vars_sim = "HYD_temp", path = fx$path, model_controls = fx$mc,
               FUN_list = list(HYD_temp = mae), method = "calib")

  r1 <- do.call(run_and_fit, c(args, list(weights = c(HYD_temp = 1))))
  r3 <- do.call(run_and_fit, c(args, list(weights = c(HYD_temp = 3))))

  expect_equal(r3$HYD_temp, r1$HYD_temp * 3, tolerance = 1e-6)
})

test_that("return_df gives the obs-vs-model comparison frame", {
  fx <- raf_fixture("glm_aed")

  df <- run_and_fit(aeme = fx$aeme, param = raf_params("glm_aed"),
                    model = "glm_aed", vars_sim = "HYD_temp", path = fx$path,
                    model_controls = fx$mc, FUN_list = list(HYD_temp = mae),
                    weights = c(HYD_temp = 1), method = "calib",
                    return_df = TRUE)

  expect_s3_class(df, "data.frame")
  expect_true(all(c("Date", "depth", "var_aeme", "obs", "model", "diff")
                  %in% names(df)))
  expect_gt(nrow(df), 0)
  expect_equal(df$diff, df$model - df$obs, tolerance = 1e-9)
  expect_setequal(unique(df$var_aeme), "HYD_temp")
})

test_that("return_indices returns reusable date/depth indices", {
  fx <- raf_fixture("glm_aed")

  idx <- run_and_fit(aeme = fx$aeme, param = raf_params("glm_aed"),
                     model = "glm_aed", vars_sim = "HYD_temp", path = fx$path,
                     model_controls = fx$mc, FUN_list = list(HYD_temp = mae),
                     weights = c(HYD_temp = 1), method = "calib",
                     return_indices = TRUE, fit = FALSE)

  expect_type(idx, "list")
  expect_true("HYD_temp" %in% names(idx))
  expect_true(all(c("date_index", "depths", "dates") %in% names(idx$HYD_temp)))
  expect_gt(length(idx$HYD_temp$date_index), 0)

  # Feeding them back in reproduces the plain fit.
  plain <- run_and_fit(aeme = fx$aeme, param = raf_params("glm_aed"),
                       model = "glm_aed", vars_sim = "HYD_temp", path = fx$path,
                       model_controls = fx$mc, FUN_list = list(HYD_temp = mae),
                       weights = c(HYD_temp = 1), method = "calib")
  reused <- run_and_fit(aeme = fx$aeme, param = raf_params("glm_aed"),
                        model = "glm_aed", vars_sim = "HYD_temp", path = fx$path,
                        model_controls = fx$mc, FUN_list = list(HYD_temp = mae),
                        weights = c(HYD_temp = 1), method = "calib",
                        var_indices = idx)
  expect_equal(reused$HYD_temp, plain$HYD_temp, tolerance = 1e-6)
})

test_that("a failed model run returns na_value for every variable", {
  fx <- raf_fixture("glm_aed")

  res <- suppressWarnings(run_and_fit(
    aeme = fx$aeme, param = raf_params("glm_aed"), model = "glm_aed",
    vars_sim = "HYD_temp", path = fx$path, model_controls = fx$mc,
    FUN_list = list(HYD_temp = mae), weights = c(HYD_temp = 1),
    method = "calib", na_value = 999, timeout = 1e-6))

  expect_type(res, "list")
  expect_named(res, "HYD_temp")
  expect_equal(res$HYD_temp, 999)
})

test_that("sa mode returns one fit component per named sub-region", {
  fx <- raf_fixture("glm_aed")
  init_depth <- AEME::input(fx$aeme)$init_depth

  ctrl <- create_sa_control(
    N = 2, file_type = "db", na_value = 999, ncore = 1L,
    file_dir = file.path(fx$path, "calib_sa"),
    vars_sim = list(
      surf_temp = list(var = "HYD_temp", month = c(12, 1, 2),
                       depth_range = c(0, 2)),
      bot_temp  = list(var = "HYD_temp", month = c(12, 1, 2),
                       depth_range = c(init_depth - 2, init_depth))
    ))

  res <- run_and_fit(aeme = fx$aeme, param = raf_params("glm_aed"),
                     model = "glm_aed", vars_sim = "HYD_temp", path = fx$path,
                     model_controls = fx$mc,
                     FUN_list = list(HYD_temp = function(df) mean(df$model)),
                     weights = c(HYD_temp = 1),
                     method = "sa", sa_ctrl = ctrl)

  expect_type(res, "list")
  expect_true(all(c("surf_temp", "bot_temp") %in% names(res)))
  expect_true(all(vapply(res[c("surf_temp", "bot_temp")],
                         function(x) is.finite(x), logical(1))))
})

test_that(".region_var_indices substitutes a finite depth_range so a depth-less variable does not crash AEME::get_var_indices()", {
  # AEME::get_var_indices()'s month/depth_range branch does
  # seq(min(depth_range), max(depth_range), by = 0.5) unconditionally, which
  # errors ("'from' must be a finite number") when depth_range is NULL - the
  # only sensible spec for a genuinely depth-less (not derived) variable
  # like LKE_vol/LKE_lvlwtr. Capture the call AEME actually sees.
  captured <- NULL
  local_mocked_bindings(
    get_var_indices = function(nc, model, aeme, path, vars_sim, month = NULL,
                               depth_range = NULL, use_obs = TRUE) {
      captured <<- list(month = month, depth_range = depth_range)
      stats::setNames(list(list(date_index = 1:2, depths = NA_real_,
                                dates = as.Date(c("2020-01-15", "2020-02-15")))),
                      vars_sim)
    },
    .package = "AEME")

  region <- list(var = "LKE_vol", month = c(1, 2, 12), depth_range = NULL)
  idx <- aemetools:::.region_var_indices(nc = NULL, model = "glm_aed",
                                         aeme = NULL, path = NULL,
                                         region = region)

  expect_equal(captured$month, c(1, 2, 12))
  expect_true(all(is.finite(captured$depth_range)))
  expect_equal(idx$dates, as.Date(c("2020-01-15", "2020-02-15")))
})

test_that(".region_var_indices refuses depth_range = NULL for a derived variable", {
  # HYD_schstb and LKE_nrgtot are computed from HYD_temp's full depth
  # profile (AEME::key_naming$derived is TRUE, AEME::get_deriv_inputs()
  # names "HYD_temp"), even though their own output is one value per
  # timestep - unlike LKE_vol/LKE_lvlwtr, `depth_range = NULL` is wrong for
  # them and used to fail deep inside rLakeAnalyzer instead of here.
  region <- list(var = "HYD_schstb", month = c(1, 2, 12), depth_range = NULL)
  expect_error(
    aemetools:::.region_var_indices(nc = NULL, model = "glm_aed", aeme = NULL,
                                    path = NULL, region = region),
    "derived variable"
  )
})

test_that(".region_var_indices defaults month to every month when only depth_range is given", {
  # The same branch filters dates on `dates %in% month`, silently always
  # FALSE (zero dates, not "no month filter") when month is NULL but
  # depth_range is given.
  captured <- NULL
  local_mocked_bindings(
    get_var_indices = function(nc, model, aeme, path, vars_sim, month = NULL,
                               depth_range = NULL, use_obs = TRUE) {
      captured <<- list(month = month, depth_range = depth_range)
      stats::setNames(list(list(date_index = 1L, depths = c(0, 0.5, 1),
                                dates = as.Date("2020-01-15"))), vars_sim)
    },
    .package = "AEME")

  region <- list(var = "HYD_temp", depth_range = c(0, 1))
  aemetools:::.region_var_indices(nc = NULL, model = "glm_aed", aeme = NULL,
                                  path = NULL, region = region)

  expect_equal(captured$month, 1:12)
  expect_equal(captured$depth_range, c(0, 1))
})

test_that(".region_var_indices passes month and depth_range through unchanged when both are given", {
  captured <- NULL
  local_mocked_bindings(
    get_var_indices = function(nc, model, aeme, path, vars_sim, month = NULL,
                               depth_range = NULL, use_obs = TRUE) {
      captured <<- list(month = month, depth_range = depth_range)
      stats::setNames(list(list(date_index = 1L, depths = c(10, 11, 12),
                                dates = as.Date("2020-01-15"))), vars_sim)
    },
    .package = "AEME")

  region <- list(var = "LKE_lvlwtr", month = c(12, 1, 2), depth_range = c(10, 12))
  aemetools:::.region_var_indices(nc = NULL, model = "glm_aed", aeme = NULL,
                                  path = NULL, region = region)

  expect_equal(captured$month, c(12, 1, 2))
  expect_equal(captured$depth_range, c(10, 12))
})

test_that(".region_var_indices substitutes placeholders for both when neither month nor depth_range is given", {
  # Deliberately never takes AEME::get_var_indices()'s observation-driven
  # default (month and depth_range both absent): that default derives dates
  # from observations(aeme)$lake rows for this exact variable, which a
  # region is just as often built around a variable that has none of at
  # all (a Morris-screening target like HYD_schstb, or LKE_vol/LKE_nrgtot),
  # and even when observations do exist, gating a `method = "sa"` region's
  # dates on them silently changes what it aggregates over - Morris
  # screening never joins to observations at all (see run_and_fit()'s
  # "Score" section).
  captured <- NULL
  local_mocked_bindings(
    get_var_indices = function(nc, model, aeme, path, vars_sim, month = NULL,
                               depth_range = NULL, use_obs = TRUE) {
      captured <<- list(month = month, depth_range = depth_range)
      stats::setNames(list(list(date_index = 1L, depths = 0,
                                dates = as.Date("2020-01-15"))), vars_sim)
    },
    .package = "AEME")

  region <- list(var = "LKE_vol")
  aemetools:::.region_var_indices(nc = NULL, model = "glm_aed", aeme = NULL,
                                  path = NULL, region = region)

  expect_equal(captured$month, 1:12)
  expect_true(all(is.finite(captured$depth_range)))
})

# Regression test for a real staged-calibration failure: a Morris-screening
# region set mixing depth-resolved HYD_temp windows with three whole-lake
# targets. HYD_schstb (Schmidt stability) and LKE_nrgtot (total energy) are
# *derived* from HYD_temp's full depth profile (AEME::key_naming$derived is
# TRUE for both, AEME::get_deriv_inputs() names "HYD_temp") even though
# their own output is a single value per timestep - `depth_range = NULL`
# starves the derived calculation of a profile and used to fail deep inside
# rLakeAnalyzer's safe_apply() ("argument must be coercible to non-negative
# integer") rather than with anything actionable. LKE_vol is a genuine
# native scalar (not derived) and is the one target `depth_range = NULL` is
# actually meant for.
sen2_regions <- function(z) {
  list(
    surf_temp    = list(var = "HYD_temp",   month = 1:12, depth_range = c(0, 2),
                        weight = 1.0),
    meta_temp    = list(var = "HYD_temp",   month = 1:12, depth_range = c(2.5, z - 4.5),
                        weight = 0.5),
    bot_temp     = list(var = "HYD_temp",   month = 1:12, depth_range = c(z - 4, z),
                        weight = 2.0),
    schmidt_stab = list(var = "HYD_schstb", month = NULL, depth_range = NULL,
                        weight = 1.0),
    whole_lvl    = list(var = "LKE_vol",    month = NULL, depth_range = NULL,
                        weight = 1.0),
    whole_nrg    = list(var = "LKE_nrgtot", month = NULL, depth_range = NULL,
                        weight = 1.0)
  )
}

test_that("a NULL depth_range on a derived variable aborts with an actionable message", {
  fx <- raf_fixture("glm_aed", vars_sim = c("HYD_temp", "HYD_schstb", "LKE_vol",
                                            "LKE_nrgtot"))
  z <- AEME::input(fx$aeme)$init_depth
  fit <- function(df) mean(df$model)
  FUN_list <- list(HYD_temp = fit, HYD_schstb = fit, LKE_vol = fit,
                   LKE_nrgtot = fit)

  expect_error(
    run_and_fit(aeme = fx$aeme, param = raf_params("glm_aed"),
               model = "glm_aed", vars_sim = "HYD_temp", path = fx$path,
               model_controls = fx$mc, FUN_list = FUN_list,
               weights = c(HYD_temp = 1), method = "sa",
               sa_ctrl = list(vars_sim = sen2_regions(z))),
    "derived variable"
  )
})

test_that("a region set mixing depth-resolved, derived and scalar targets scores cleanly", {
  fx <- raf_fixture("glm_aed", vars_sim = c("HYD_temp", "HYD_schstb", "LKE_vol",
                                            "LKE_nrgtot"))
  z <- AEME::input(fx$aeme)$init_depth
  fit <- function(df) mean(df$model)
  FUN_list <- list(HYD_temp = fit, HYD_schstb = fit, LKE_vol = fit,
                   LKE_nrgtot = fit)

  regions <- sen2_regions(z)
  # HYD_schstb and LKE_nrgtot need the full water column as input, not NULL.
  regions$schmidt_stab$depth_range <- c(0, z)
  regions$whole_nrg$depth_range <- c(0, z)

  res <- run_and_fit(aeme = fx$aeme, param = raf_params("glm_aed"),
                     model = "glm_aed", vars_sim = "HYD_temp", path = fx$path,
                     model_controls = fx$mc, FUN_list = FUN_list,
                     weights = c(HYD_temp = 1), method = "sa",
                     sa_ctrl = list(vars_sim = regions))

  expect_type(res, "list")
  expect_setequal(names(res), c(names(regions), "failed"))
  expect_false(res$failed)
  expect_true(all(vapply(res[names(regions)], is.finite, logical(1))))
})

test_that("calib mode with regions scores each sub-region independently", {
  fx <- raf_fixture("glm_aed")

  # Observation depths in the test lake are whole metres (0..12); the
  # region's depth grid (AEME::get_var_indices(), 0.5 m steps from
  # depth_range's own endpoints) only lands on them when the endpoints
  # themselves are whole metres too.
  regions <- list(
    surf_temp = list(var = "HYD_temp", month = 1:12, depth_range = c(0, 2)),
    bot_temp  = list(var = "HYD_temp", month = 1:12, depth_range = c(10, 12))
  )

  res <- run_and_fit(aeme = fx$aeme, param = raf_params("glm_aed"),
                     model = "glm_aed", vars_sim = "HYD_temp", path = fx$path,
                     model_controls = fx$mc, FUN_list = list(HYD_temp = mae),
                     weights = c(HYD_temp = 1), method = "calib",
                     regions = regions)

  expect_type(res, "list")
  expect_true(all(c("surf_temp", "bot_temp") %in% names(res)))
  expect_false("HYD_temp" %in% names(res))
  expect_true(all(vapply(res[c("surf_temp", "bot_temp")], is.finite,
                         logical(1))))
})

test_that("a region's weight scales its fit component linearly", {
  fx <- raf_fixture("glm_aed")
  args <- list(aeme = fx$aeme, param = raf_params("glm_aed"), model = "glm_aed",
               vars_sim = "HYD_temp", path = fx$path, model_controls = fx$mc,
               FUN_list = list(HYD_temp = mae), weights = c(HYD_temp = 1),
               method = "calib")

  regions1 <- list(surf_temp = list(var = "HYD_temp", month = 1:12,
                                    depth_range = c(0, 2)))
  regions3 <- list(surf_temp = list(var = "HYD_temp", month = 1:12,
                                    depth_range = c(0, 2), weight = 3))

  r1 <- do.call(run_and_fit, c(args, list(regions = regions1)))
  r3 <- do.call(run_and_fit, c(args, list(regions = regions3)))

  expect_equal(r3$surf_temp, r1$surf_temp * 3, tolerance = 1e-6)
})

test_that("include_wlev adds a LKE_lvlwtr fit component", {
  fx <- raf_fixture("glm_aed", vars_sim = c("HYD_temp", "LKE_lvlwtr"))

  res <- run_and_fit(aeme = fx$aeme, param = raf_params("glm_aed"),
                     model = "glm_aed",
                     vars_sim = c("HYD_temp", "LKE_lvlwtr"), path = fx$path,
                     model_controls = fx$mc,
                     FUN_list = list(HYD_temp = mae, LKE_lvlwtr = mae),
                     weights = c(HYD_temp = 1, LKE_lvlwtr = 1),
                     method = "calib", include_wlev = TRUE)

  expect_type(res, "list")
  expect_true(all(c("HYD_temp", "LKE_lvlwtr") %in% names(res)))
  expect_true(is.finite(res$LKE_lvlwtr))
})

test_that("return_df carries water-level rows when include_wlev is set", {
  fx <- raf_fixture("glm_aed", vars_sim = c("HYD_temp", "LKE_lvlwtr"))

  df <- run_and_fit(aeme = fx$aeme, param = raf_params("glm_aed"),
                    model = "glm_aed",
                    vars_sim = c("HYD_temp", "LKE_lvlwtr"), path = fx$path,
                    model_controls = fx$mc,
                    FUN_list = list(HYD_temp = mae, LKE_lvlwtr = mae),
                    weights = c(HYD_temp = 1, LKE_lvlwtr = 1),
                    method = "calib", include_wlev = TRUE, return_df = TRUE)

  expect_s3_class(df, "data.frame")
  expect_true(all(c("Date", "depth", "var_aeme", "obs", "model", "diff")
                  %in% names(df)))
  expect_setequal(unique(df$var_aeme), c("HYD_temp", "LKE_lvlwtr"))

  lvl <- df[df$var_aeme == "LKE_lvlwtr", ]
  expect_gt(nrow(lvl), 0)
  # Level rows are keyed with no depth, exactly as .pest_run_residual()
  # matches them back to obs_map.
  expect_true(all(is.na(lvl$depth)))
  expect_equal(lvl$diff, lvl$model - lvl$obs, tolerance = 1e-9)
  # The gridded rows still satisfy the same identity.
  hyd <- df[df$var_aeme == "HYD_temp", ]
  expect_equal(hyd$diff, hyd$model - hyd$obs, tolerance = 1e-9)
})

test_that("return_df works for a water-level-only calibration", {
  fx <- raf_fixture("glm_aed", vars_sim = c("HYD_temp", "LKE_lvlwtr"))

  df <- run_and_fit(aeme = fx$aeme, param = raf_params("glm_aed"),
                    model = "glm_aed", vars_sim = "LKE_lvlwtr", path = fx$path,
                    model_controls = fx$mc, FUN_list = list(LKE_lvlwtr = mae),
                    weights = c(LKE_lvlwtr = 1), method = "calib",
                    include_wlev = TRUE, return_df = TRUE)

  expect_s3_class(df, "data.frame")
  expect_setequal(unique(df$var_aeme), "LKE_lvlwtr")
  expect_gt(nrow(df), 0)
  expect_equal(df$diff, df$model - df$obs, tolerance = 1e-9)
})

test_that("simstrat_aed runs through run_and_fit via the netCDF path", {
  skip_if_slow()
  fx <- raf_fixture("simstrat_aed")

  fit <- run_and_fit(aeme = fx$aeme, param = raf_params("simstrat_aed"),
                     model = "simstrat_aed", vars_sim = "HYD_temp",
                     path = fx$path, model_controls = fx$mc,
                     FUN_list = list(HYD_temp = mae), weights = c(HYD_temp = 1),
                     method = "calib")
  expect_true(is.finite(fit$HYD_temp))
  expect_gt(fit$HYD_temp, 0)

  df <- run_and_fit(aeme = fx$aeme, param = raf_params("simstrat_aed"),
                    model = "simstrat_aed", vars_sim = "HYD_temp",
                    path = fx$path, model_controls = fx$mc,
                    FUN_list = list(HYD_temp = mae), weights = c(HYD_temp = 1),
                    method = "calib", return_df = TRUE)
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0)
  expect_equal(df$diff, df$model - df$obs, tolerance = 1e-9)
})

test_that("run_and_fit()'s summed fit differs from assess_model()'s diagnostics", {
  fx <- raf_fixture("glm_aed", vars_sim = c("HYD_temp", "LKE_lvlwtr"))

  # Multi-variable, weighted, water-level included: the case
  # `assess_aeme()`/`assess_model()` cannot reproduce (no weighting, no
  # cross-variable summation) - this is the documented "Reproducing the
  # calibration fit value" pattern from vignette("calibrate-aeme").
  vars_sim <- c("HYD_temp", "LKE_lvlwtr")
  weights <- c("HYD_temp" = 1, "LKE_lvlwtr" = 0.5)
  FUN_list <- list(HYD_temp = mae, LKE_lvlwtr = mae)
  na_value <- 999

  res <- run_and_fit(aeme = fx$aeme, param = raf_params("glm_aed"),
                     model = "glm_aed", vars_sim = vars_sim, path = fx$path,
                     model_controls = fx$mc, FUN_list = FUN_list,
                     weights = weights, na_value = na_value,
                     include_wlev = TRUE, method = "calib", fit = TRUE)
  fit <- if (any(is.na(unlist(res)))) na_value else sum(unlist(res))

  # Same aggregation `eval_param_chunk()` performs on every calibration
  # candidate (`sum(unlist(res))`, with `na_value` substituted on any NA
  # component) - this is what a `calib_aeme()`-recorded `fit_value` for this
  # parameter set equals.
  expect_equal(fit, sum(unlist(res)))
  expect_false(anyNA(unlist(res)))

  # And it must differ from assess_aeme()/assess_model()'s per-variable,
  # unweighted diagnostics for the same run - the two answer different
  # questions and are not interchangeable.
  aeme_run <- run_aeme_param(aeme = fx$aeme, param = raf_params("glm_aed"),
                             model = "glm_aed", path = fx$path,
                             return_aeme = TRUE)
  diag <- AEME::assess_model(aeme = aeme_run, model = "glm_aed",
                             var_sim = "HYD_temp")
  expect_false(isTRUE(all.equal(fit, diag$mae)))
})
