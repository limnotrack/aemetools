# Functions that live in AEME but are re-exported so `library(aemetools)`
# scripts and vignettes keep working.

test_that("AEME helpers are re-exported unchanged", {
  fns <- c("zone_ratio_param", "zone_offset_param", "expand_zone_ratios",
           "is_zone_ratio", "zone_base_name", "encode_param", "decode_param",
           "decode_param_full", "ensemble_summary", "plot_ensemble",
           "score_ensemble")
  for (f in fns) {
    expect_true(f %in% getNamespaceExports("aemetools"), info = f)
    expect_identical(getExportedValue("aemetools", f),
                     getExportedValue("AEME", f), info = f)
  }
})

test_that("encoded parameter names keep the stored format", {
  # name_full is written to PEST control files and results databases
  expect_equal(encode_param("light", "Kw", 1L), "light/Kw[1]")
  expect_equal(encode_param(NA, "ce", NA_integer_), "NA/ce[NA]")
  expect_equal(decode_param("light/Kw[1]"), "Kw")
})
