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
