# Tests for the observation-driven calibration/validation split.
#
# These build their own Aeme object from the AEME test lake rather than
# using the shared cache: the split is a pure function of the observation
# and forcing tables, so the tests set those directly to pin down the
# behaviour (a forcing record shorter than the observations, a variable too
# sparse to split, and so on) rather than depending on whatever the test
# lake happens to carry.

make_cp_aeme <- function(obs_dates = seq(as.Date("2020-01-01"),
                                         by = "month", length.out = 12),
                         vars = "HYD_temp", n_depth = 3,
                         met_range = c("2019-01-01", "2023-12-31")) {
  aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))

  lake <- expand.grid(Date = obs_dates, var_aeme = vars,
                      depth = seq_len(n_depth),
                      stringsAsFactors = FALSE)
  lake$value <- seq_len(nrow(lake))
  lake$depth_from <- lake$depth
  lake$depth_to <- lake$depth
  lake$depth <- NULL

  obs <- AEME::observations(aeme)
  obs$lake <- lake
  obs$level <- NULL
  AEME::observations(aeme) <- obs

  inp <- AEME::input(aeme)
  inp$meteo <- data.frame(
    Date = seq(as.Date(met_range[1]), as.Date(met_range[2]), by = "day"),
    MET_tmpair = 15)
  AEME::input(aeme) <- inp

  aeme
}

test_that("the split lands on observation dates and covers the record", {
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 12)
  p <- get_calib_periods(make_cp_aeme(dates), vars_sim = "HYD_temp")

  expect_s3_class(p, "aeme_calib_periods")
  expect_equal(p$periods$period, c("calib", "valid"))
  # Every boundary is a date that actually carries data, so neither period
  # opens or closes on a stretch with nothing to compare against.
  for (d in c(p$periods$start, p$periods$stop)) expect_true(d %in% dates)
  # Contiguous and non-overlapping: calib ends before valid starts.
  expect_lt(p$periods$stop[1], p$periods$start[2])
  expect_equal(p$split_date, p$periods$start[2])
  # The whole record is used - first and last observation are the outer edges.
  expect_equal(p$periods$start[1], min(dates))
  expect_equal(p$periods$stop[2], max(dates))
  # No observation is lost or double-counted across the two periods.
  expect_equal(sum(p$periods$n_obs), 12 * 3)
  expect_equal(sum(p$periods$n_dates), 12)
})

test_that("frac moves the split and is measured on observations", {
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 12)
  aeme <- make_cp_aeme(dates)

  share <- function(f) {
    p <- get_calib_periods(aeme, vars_sim = "HYD_temp", frac = f)
    p$periods$n_obs[1] / sum(p$periods$n_obs)
  }
  # Evenly-spaced dates, so the achievable share tracks the target closely.
  expect_equal(share(0.5), 0.5, tolerance = 0.05)
  expect_equal(share(0.75), 0.75, tolerance = 0.05)
  # Monotone: asking for more calibration never yields less.
  expect_gt(share(0.8), share(0.3))
})

test_that("weight_by = 'dates' counts sampling days, not observations", {
  # Deep profiles early, single surface samples later: counting rows and
  # counting visits disagree, which is the whole reason for the argument.
  early <- seq(as.Date("2020-01-01"), by = "month", length.out = 4)
  late <- seq(as.Date("2021-01-01"), by = "month", length.out = 8)

  aeme <- make_cp_aeme(early, n_depth = 20)
  obs <- AEME::observations(aeme)
  thin <- expand.grid(Date = late, var_aeme = "HYD_temp", depth_from = 1,
                      depth_to = 1, stringsAsFactors = FALSE)
  thin$value <- 1
  obs$lake <- rbind(obs$lake, thin)
  AEME::observations(aeme) <- obs

  by_obs <- get_calib_periods(aeme, vars_sim = "HYD_temp", frac = 0.5,
                              weight_by = "obs")
  by_date <- get_calib_periods(aeme, vars_sim = "HYD_temp", frac = 0.5,
                               weight_by = "dates")

  # 80 early observations over 4 visits vs 8 later observations over 8
  # visits: balancing rows keeps the split early, balancing visits pushes
  # it later.
  expect_lt(by_obs$split_date, by_date$split_date)
  expect_equal(by_date$periods$n_dates[1], 6)
})

test_that("every variable is represented in both periods", {
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 12)
  aeme <- make_cp_aeme(dates, vars = c("HYD_temp", "CHM_oxy"))
  p <- get_calib_periods(aeme, vars_sim = c("HYD_temp", "CHM_oxy"))

  expect_setequal(p$coverage$var_aeme, c("HYD_temp", "CHM_oxy"))
  expect_true(all(p$coverage$n_obs > 0))
  # The coverage table's counts reconcile with the period totals.
  for (per in c("calib", "valid")) {
    expect_equal(sum(p$coverage$n_obs[p$coverage$period == per]),
                 p$periods$n_obs[p$periods$period == per])
  }
})

test_that("min_obs is enforced and a variable too sparse to split aborts", {
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 12)
  aeme <- make_cp_aeme(dates, vars = "HYD_temp", n_depth = 1)

  # One observation of a second variable: it cannot appear in both periods,
  # so no split satisfies the constraint and the message must name it.
  obs <- AEME::observations(aeme)
  obs$lake <- rbind(obs$lake,
                    data.frame(Date = dates[1], var_aeme = "CHM_oxy",
                               value = 1, depth_from = 1, depth_to = 1))
  AEME::observations(aeme) <- obs

  expect_error(get_calib_periods(aeme, vars_sim = c("HYD_temp", "CHM_oxy")),
               "CHM_oxy")
  # Dropping it, the split succeeds again.
  expect_no_error(get_calib_periods(aeme, vars_sim = "HYD_temp"))

  # A min_obs no split can satisfy is an error, not a silent bad split.
  expect_error(get_calib_periods(aeme, vars_sim = "HYD_temp", min_obs = 500),
               "min_obs")
  # min_obs is honoured when it is satisfiable.
  p <- get_calib_periods(aeme, vars_sim = "HYD_temp", min_obs = 4)
  expect_true(all(p$coverage$n_obs >= 4))
})

test_that("the whole observation record is used, whatever the forcing", {
  # The split is a function of the observations alone: the met record is
  # not consulted and does not clip either period.
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 24)
  short <- make_cp_aeme(dates, met_range = c("2020-06-01", "2020-12-31"))
  long <- make_cp_aeme(dates, met_range = c("2015-01-01", "2030-12-31"))

  p_short <- get_calib_periods(short, vars_sim = "HYD_temp")
  p_long <- get_calib_periods(long, vars_sim = "HYD_temp")

  expect_equal(p_short$periods, p_long$periods)
  expect_equal(p_short$periods$start[1], min(dates))
  expect_equal(p_short$periods$stop[2], max(dates))
  expect_equal(sum(p_short$periods$n_dates), length(dates))
})

test_that("min_density drops sparse years at the ends only", {
  # Five thin years, then five dense ones: the thin lead-in costs model
  # runtime out of all proportion to what it constrains. The contrast has to
  # be sharp to clear a median drawn from both halves - which is what a real
  # record looks like (Rototoa: ~10 observations a year against a median of
  # ~150).
  thin <- as.Date(paste0(2010:2014, "-06-01"))
  dense <- seq(as.Date("2015-01-01"), as.Date("2019-12-01"), by = "month")
  aeme <- make_cp_aeme(c(thin, dense))

  keep <- get_calib_periods(aeme, vars_sim = "HYD_temp", min_density = 0)
  trim <- get_calib_periods(aeme, vars_sim = "HYD_temp", min_density = 0.25)

  expect_equal(keep$periods$start[1], min(thin))
  expect_gte(trim$periods$start[1], min(dense))
  # The end of the record is dense, so it is untouched.
  expect_equal(trim$periods$stop[2], max(dense))
  # Fewer model-years, and the observations lost are a small share.
  expect_lt(sum(trim$periods$n_days), sum(keep$periods$n_days))
  expect_gt(sum(trim$periods$n_obs) / sum(keep$periods$n_obs), 0.8)
})

test_that("density_by = 'profiles' catches thin casts that counts miss", {
  # The case the default exists for: an early era sampled OFTEN but only
  # surface-and-bottom, against a later era sampled less often with full
  # depth casts. By sampling days the early era looks denser; by readings it
  # still clears the bar; only counting profiles scores it for what it is
  # worth to a depth-resolved calibration. This is Lake Rototoa's shape -
  # pre-1993 is six to eight visits a year of two depths each.
  mk <- function(years, n_dates, n_depths) {
    do.call(rbind, lapply(years, function(y) {
      dts <- seq(as.Date(paste0(y, "-01-15")), by = "month",
                 length.out = n_dates)
      expand.grid(Date = dts, depth_from = seq_len(n_depths),
                  stringsAsFactors = FALSE)
    }))
  }
  thin <- mk(2010:2014, n_dates = 12, n_depths = 2)   # 24 obs/yr, 0 profiles
  full <- mk(2015:2019, n_dates = 6, n_depths = 20)   # 120 obs/yr, 6 profiles
  lake <- rbind(thin, full)
  lake$depth_to <- lake$depth_from
  lake$var_aeme <- "HYD_temp"
  lake$value <- seq_len(nrow(lake))

  aeme <- make_cp_aeme()
  obs <- AEME::observations(aeme)
  obs$lake <- lake
  obs$level <- NULL
  AEME::observations(aeme) <- obs

  by <- function(m) get_calib_periods(aeme, vars_sim = "HYD_temp",
                                      density_by = m)$periods$start[1]

  # Profiles trims the thin era away...
  expect_gte(by("profiles"), as.Date("2015-01-01"))
  # ...while neither counting readings nor counting days does: 24 readings a
  # year clears a quarter of the 72-a-year median, and 12 sampling days a
  # year is more than the later era has.
  expect_lt(by("obs"), as.Date("2015-01-01"))
  expect_lt(by("dates"), as.Date("2015-01-01"))

  # min_depths is what "a profile" means; raising it past the full casts
  # leaves nothing dense enough to anchor on, so the record stands.
  expect_equal(
    get_calib_periods(aeme, vars_sim = "HYD_temp",
                      min_depths = 50L)$periods$start[1],
    min(lake$Date))
})

test_that("a scalar series counts as a profile on every date", {
  # Water level has no depths, so it must not be scored zero by a rule
  # about depth casts - it is a scalar series, not a thin profile.
  dates <- seq(as.Date("2015-01-01"), as.Date("2019-12-01"), by = "month")
  aeme <- make_cp_aeme()
  obs <- AEME::observations(aeme)
  obs$lake <- obs$lake[0, ]
  obs$level <- data.frame(Date = dates, var_aeme = "LKE_lvlwtr",
                          value = seq_along(dates))
  AEME::observations(aeme) <- obs

  p <- get_calib_periods(aeme, vars_sim = "LKE_lvlwtr")
  expect_equal(p$periods$start[1], min(dates))
  expect_equal(p$periods$stop[2], max(dates))
  expect_equal(sum(p$periods$n_obs), length(dates))
})

test_that("a lean year inside a dense record is kept", {
  # Trimming works in from the ends, so the period stays contiguous - a gap
  # in the middle would otherwise silently drop data the model still has to
  # simulate through.
  dates <- c(seq(as.Date("2015-01-01"), as.Date("2015-12-01"), by = "month"),
             as.Date("2016-06-01"),                       # the lean year
             seq(as.Date("2017-01-01"), as.Date("2018-12-01"), by = "month"))
  aeme <- make_cp_aeme(dates)

  p <- get_calib_periods(aeme, vars_sim = "HYD_temp", min_density = 0.25)
  expect_equal(p$periods$start[1], min(dates))
  expect_equal(p$periods$stop[2], max(dates))
  # The lean year's observation is still inside a period.
  expect_equal(sum(p$periods$n_dates), length(dates))
})

test_that("min_density leaves short records alone", {
  # A median across two or three years says nothing about which of them is
  # sparse, so the rule does not fire.
  dates <- c(as.Date("2019-06-01"),
             seq(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "month"))
  aeme <- make_cp_aeme(dates)

  p <- get_calib_periods(aeme, vars_sim = "HYD_temp", min_density = 0.5)
  expect_equal(p$periods$start[1], min(dates))

  # Raising min_years past the record's span suppresses it on a long record.
  long <- make_cp_aeme(c(as.Date(paste0(2010:2014, "-06-01")),
                         seq(as.Date("2015-01-01"), as.Date("2019-12-01"),
                             by = "month")))
  expect_gt(get_calib_periods(long, vars_sim = "HYD_temp")$periods$start[1],
            as.Date("2014-12-31"))
  expect_equal(
    get_calib_periods(long, vars_sim = "HYD_temp",
                      min_years = 99L)$periods$start[1],
    as.Date("2010-06-01"))
})

test_that("min_density applies to split = FALSE too, and is validated", {
  thin <- as.Date(paste0(2010:2014, "-06-01"))
  dense <- seq(as.Date("2015-01-01"), as.Date("2019-12-01"), by = "month")
  aeme <- make_cp_aeme(c(thin, dense))

  one <- get_calib_periods(aeme, vars_sim = "HYD_temp", split = FALSE)
  expect_equal(one$periods$period, "all")
  expect_gte(one$periods$start, min(dense))

  expect_error(get_calib_periods(aeme, min_density = -1), "min_density")
  expect_error(get_calib_periods(aeme, min_density = c(1, 2)), "min_density")
})

test_that("split = FALSE returns the whole record as one period", {
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 12)
  aeme <- make_cp_aeme(dates, vars = c("HYD_temp", "CHM_oxy"))
  p <- get_calib_periods(aeme, vars_sim = c("HYD_temp", "CHM_oxy"),
                         split = FALSE)

  expect_s3_class(p, "aeme_calib_periods")
  expect_equal(p$periods$period, "all")
  expect_null(p$split_date)

  # Nothing is held back: the period spans every observation.
  expect_equal(p$periods$start, min(dates))
  expect_equal(p$periods$stop, max(dates))
  expect_equal(p$periods$n_dates, length(dates))
  expect_equal(p$periods$n_obs, 12 * 2 * 3)

  # It carries the same totals a split would, just undivided.
  split <- get_calib_periods(aeme, vars_sim = c("HYD_temp", "CHM_oxy"))
  expect_equal(p$periods$n_obs, sum(split$periods$n_obs))
  expect_equal(p$periods$n_dates, sum(split$periods$n_dates))

  # Coverage keeps its per-variable shape, one row per variable.
  expect_setequal(p$coverage$var_aeme, c("HYD_temp", "CHM_oxy"))
  expect_equal(nrow(p$coverage), 2)
  expect_true(all(p$coverage$period == "all"))
  expect_equal(sum(p$coverage$n_obs), p$periods$n_obs)
})

test_that("split = FALSE works where a split is impossible", {
  # A single sampling date, and a variable with one observation: both make
  # a split impossible, neither prevents using the whole record.
  one <- make_cp_aeme(obs_dates = as.Date("2020-01-01"))
  expect_error(get_calib_periods(one, vars_sim = "HYD_temp"),
               "distinct observation dates")
  p <- get_calib_periods(one, vars_sim = "HYD_temp", split = FALSE)
  expect_equal(p$periods$start, p$periods$stop)
  expect_equal(p$periods$n_dates, 1)

  # The split error points at the way out.
  expect_error(get_calib_periods(one, vars_sim = "HYD_temp"),
               "split = FALSE")
})

test_that("split-search arguments are flagged as ignored, not silently dropped", {
  aeme <- make_cp_aeme()
  withr::with_options(
    list(AEME.inform = TRUE),
    expect_message(get_calib_periods(aeme, vars_sim = "HYD_temp",
                                     frac = 0.9, split = FALSE),
                   "ignored"))
  # No advisory when they are left at their defaults.
  withr::with_options(
    list(AEME.inform = TRUE),
    expect_no_message(get_calib_periods(aeme, vars_sim = "HYD_temp",
                                        split = FALSE),
                      message = "ignored"))
  # frac is not validated when it has nothing to act on.
  expect_no_error(get_calib_periods(aeme, vars_sim = "HYD_temp", frac = 1,
                                    split = FALSE))
  expect_error(get_calib_periods(aeme, vars_sim = "HYD_temp", split = "yes"),
               "split")
})

test_that("bad arguments and empty records abort with a usable message", {
  aeme <- make_cp_aeme()

  expect_error(get_calib_periods(aeme, frac = 0), "frac")
  expect_error(get_calib_periods(aeme, frac = 1), "frac")
  expect_error(get_calib_periods(aeme, frac = c(0.5, 0.6)), "frac")
  expect_error(get_calib_periods(aeme, min_obs = 0), "min_obs")
  expect_error(get_calib_periods(aeme, weight_by = "nonsense"), "weight_by")

  # An unknown variable lists what is actually there.
  expect_error(get_calib_periods(aeme, vars_sim = "NOT_A_VAR"),
               "NOT_A_VAR")
  expect_error(get_calib_periods(aeme, vars_sim = "NOT_A_VAR"),
               "HYD_temp")

  # A single sampling date cannot be split.
  one <- make_cp_aeme(obs_dates = as.Date("2020-01-01"))
  expect_error(get_calib_periods(one, vars_sim = "HYD_temp"),
               "2.*distinct observation dates")
})

test_that("the returned dates drive AEME::set_time", {
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 12)
  aeme <- make_cp_aeme(dates)
  p <- get_calib_periods(aeme, vars_sim = "HYD_temp")

  # The point of the function: the periods go straight into set_time().
  cal <- AEME::set_time(aeme, start = p$periods$start[1],
                        stop = p$periods$stop[1], spin_up = 2)
  tme <- AEME::time(cal)
  expect_equal(as.Date(tme$start), p$periods$start[1])
  expect_equal(as.Date(tme$stop), p$periods$stop[1])
})

test_that("the print method reports the split without erroring", {
  p <- get_calib_periods(make_cp_aeme(), vars_sim = "HYD_temp")
  out <- paste(capture.output(print(p)), collapse = "\n")
  expect_match(out, "calib")
  expect_match(out, "valid")
  expect_match(out, format(p$split_date))
  expect_invisible(print(p))
})
