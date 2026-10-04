# Gate for tests that build/run a real lake model (GLM/GOTM/DYRESM/Simstrat)
# or a full calib_aeme()/sa_aeme() calibration. These dominate the suite's
# wall time (the full suite is >1.5h) and are skipped by default so
# `devtools::test()` / PR CI stay fast; set AEMETOOLS_RUN_SLOW_TESTS=true
# (as the scheduled full-suite CI workflow does) to run them.
skip_if_slow <- function() {
  testthat::skip_if_not(
    identical(Sys.getenv("AEMETOOLS_RUN_SLOW_TESTS"), "true"),
    "slow test skipped (set AEMETOOLS_RUN_SLOW_TESTS=true to run)"
  )
}
