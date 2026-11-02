# Unit tests for the internal helpers that let calib_aeme() accept the
# sa_aeme()-style named-list `vars_sim` (calibration sub-regions).

test_that(".as_calib_regions leaves a flat character vector unchanged", {
  out <- aemetools:::.as_calib_regions(c("HYD_temp", "LKE_lvlwtr"))
  expect_null(out$regions)
  expect_setequal(out$flat, c("HYD_temp", "LKE_lvlwtr"))
})

test_that(".as_calib_regions parses a named-list region spec", {
  vs <- list(
    surf_temp = list(var = "HYD_temp", month = c(12, 1, 2),
                     depth_range = c(0, 2)),
    bot_temp  = list(var = "HYD_temp", depth_range = c(10, 12))
  )
  out <- aemetools:::.as_calib_regions(vs)
  expect_equal(out$flat, "HYD_temp")
  expect_named(out$regions, c("surf_temp", "bot_temp"))
  # weight defaults to 1 when not supplied
  expect_equal(out$regions$surf_temp$weight, 1)
  expect_equal(out$regions$bot_temp$weight, 1)
})

test_that(".as_calib_regions keeps an explicit region weight", {
  vs <- list(surf_temp = list(var = "HYD_temp", depth_range = c(0, 2),
                              weight = 2))
  out <- aemetools:::.as_calib_regions(vs)
  expect_equal(out$regions$surf_temp$weight, 2)
})

test_that(".as_calib_regions rejects a region name without an underscore", {
  vs <- list(surftemp = list(var = "HYD_temp", depth_range = c(0, 2)))
  expect_error(aemetools:::.as_calib_regions(vs), "underscore")
})

test_that(".as_calib_regions rejects LKE_lvlwtr inside a region", {
  vs <- list(surf_lvl = list(var = "LKE_lvlwtr"))
  expect_error(aemetools:::.as_calib_regions(vs), "LKE_lvlwtr")
})

test_that(".as_calib_regions rejects a malformed region entry", {
  vs <- list(surf_temp = "HYD_temp")
  expect_error(aemetools:::.as_calib_regions(vs), "var")
})

test_that(".check_region_overlap passes non-overlapping regions", {
  regions <- list(
    surf_temp = list(var = "HYD_temp", depth_range = c(0, 2)),
    bot_temp  = list(var = "HYD_temp", depth_range = c(10, 12))
  )
  expect_no_error(aemetools:::.check_region_overlap(regions))
})

test_that(".check_region_overlap catches overlapping depth+month windows", {
  regions <- list(
    surf_temp = list(var = "HYD_temp", depth_range = c(0, 3)),
    top_temp  = list(var = "HYD_temp", depth_range = c(2, 5))
  )
  expect_error(aemetools:::.check_region_overlap(regions), "overlap")
})

test_that(".check_region_overlap allows overlapping depths in different months", {
  regions <- list(
    surf_temp_summer = list(var = "HYD_temp", month = c(12, 1, 2),
                            depth_range = c(0, 3)),
    surf_temp_winter = list(var = "HYD_temp", month = c(6, 7, 8),
                            depth_range = c(0, 3))
  )
  expect_no_error(aemetools:::.check_region_overlap(regions))
})

test_that(".check_region_overlap ignores regions on different variables", {
  regions <- list(
    surf_temp = list(var = "HYD_temp", depth_range = c(0, 3)),
    surf_oxy  = list(var = "CHM_oxy", depth_range = c(0, 3))
  )
  expect_no_error(aemetools:::.check_region_overlap(regions))
})
