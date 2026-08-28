library(testthat)

BASE_URL <- "https://api.limnotrack.com/postgrest"

# ---- lt_tables() --------------------------------------------------------

test_that("lt_tables() returns a character vector", {
  skip_if_offline()
  result <- lt_tables()
  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("lt_tables() returns sorted table names", {
  skip_if_offline()
  result <- lt_tables()
  expect_identical(result, sort(result))
})

test_that("lt_tables() uses cache on second call", {
  skip_if_offline()
  lt_tables()
  cache     <- getFromNamespace(".lt_schema_cache", "aemetools")
  cache_key <- paste0(BASE_URL, ":spec")
  expect_true(exists(cache_key, envir = cache))
})

# ---- lt_schema() --------------------------------------------------------

test_that("lt_schema() returns a tibble with expected columns", {
  skip_if_offline()
  result <- lt_schema("lake_contours")
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("column", "type", "format", "description"))
})

test_that("lt_schema() returns correct column types", {
  skip_if_offline()
  result <- lt_schema("lake_contours")
  expect_type(result$column,      "character")
  expect_type(result$type,        "character")
  expect_type(result$format,      "character")
  expect_type(result$description, "character")
})

test_that("lt_schema() detects geometry column", {
  skip_if_offline()
  result <- lt_schema("lake_contours")
  expect_true(any(grepl("geometry", result$format, ignore.case = TRUE)))
})

test_that("lt_schema() errors informatively for unknown table", {
  skip_if_offline()
  expect_error(
    lt_schema("nonexistent_table"),
    regexp = "not found.*lt_tables\\(\\)",
    info   = "Should suggest lt_tables() in the error message"
  )
})

# ---- lt_fetch() ---------------------------------------------------------

test_that("lt_fetch() returns a tibble for non-geometry table", {
  skip_if_offline()
  result <- lt_fetch("aeme_output", limit = 5)
  expect_s3_class(result, "tbl_df")
  expect_false(inherits(result, "sf"))
  expect_lte(nrow(result), 5)
})

test_that("lt_fetch() returns an sf object for geometry table", {
  skip_if_offline()
  result <- lt_fetch("lake_contours", limit = 5)
  expect_s3_class(result, "sf")
  expect_lte(nrow(result), 5)
})

test_that("lt_fetch() respects limit argument", {
  skip_if_offline()
  result <- lt_fetch("lake_contours", limit = 3)
  expect_lte(nrow(result), 3)
})

test_that("lt_fetch() respects select argument", {
  skip_if_offline()
  result <- lt_fetch("lake_contours",
                     select = c("contour_id", "depth_m"),
                     limit  = 5
  )
  expect_named(result, c("contour_id", "depth_m", "geometry"))
})

test_that("lt_fetch() respects filter argument", {
  skip_if_offline()
  result <- lt_fetch("lake_contours",
                     filter = lt_filter(lernzmp_id == "LID40188"),
                     limit  = 10
  )
  expect_true(all(result$lernzmp_id == "LID40188"))
})

test_that("lt_fetch() returns correct CRS for geometry table", {
  skip_if_offline()
  result <- lt_fetch("lake_contours", limit = 5)
  expect_equal(sf::st_crs(result)$epsg, 2193)
})

test_that("lt_fetch() returns empty sf with no matching rows", {
  skip_if_offline()
  result <- lt_fetch("lake_contours",
                     filter = lt_filter(lernzmp_id == "DOESNOTEXIST")
  )
  expect_equal(nrow(result), 0)
})

# ---- lt_filter() --------------------------------------------------------
# Pure R expression parsing — no HTTP needed

test_that("lt_filter() handles == operator", {
  result <- lt_filter(site_id == 5)
  expect_equal(result$site_id, "eq.5")
})

test_that("lt_filter() handles != operator", {
  result <- lt_filter(status != "inactive")
  expect_equal(result$status, "neq.inactive")
})

test_that("lt_filter() handles >= operator", {
  result <- lt_filter(depth_m >= 10)
  expect_equal(result$depth_m, "gte.10")
})

test_that("lt_filter() handles <= operator", {
  result <- lt_filter(depth_m <= 10)
  expect_equal(result$depth_m, "lte.10")
})

test_that("lt_filter() handles > operator", {
  result <- lt_filter(depth_m > 10)
  expect_equal(result$depth_m, "gt.10")
})

test_that("lt_filter() handles < operator", {
  result <- lt_filter(depth_m < 10)
  expect_equal(result$depth_m, "lt.10")
})

test_that("lt_filter() handles %in% operator", {
  result <- lt_filter(lake %in% c("Taupo", "Rotorua"))
  expect_equal(result$lake, "in.(Taupo,Rotorua)")
})

test_that("lt_filter() handles is.na()", {
  result <- lt_filter(is.na(value))
  expect_equal(result$value, "is.null")
})

test_that("lt_filter() handles !is.na()", {
  result <- lt_filter(!is.na(value))
  expect_equal(result$value, "not.is.null")
})

test_that("lt_filter() handles multiple expressions", {
  result <- lt_filter(site_id == 5, depth_m >= 10)
  expect_length(result, 2)
  expect_equal(result$site_id, "eq.5")
  expect_equal(result$depth_m, "gte.10")
})

test_that("lt_filter() errors on unsupported operator", {
  expect_error(lt_filter(site_id %% 2), "Unsupported operator")
})

# ---- cache ---------------------------------------------------------------

test_that("lt_cache_reset() clears the schema cache", {
  skip_if_offline()
  lt_tables()  # populate cache
  lt_cache_reset()
  cache <- getFromNamespace(".lt_schema_cache", "aemetools")
  expect_equal(length(ls(envir = cache)), 0)
})
