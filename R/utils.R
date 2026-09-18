#' Round to a specified accuracy
#' @param x A numeric vector.
#' @param accuracy A positive number specifying the rounding accuracy.
#' @param f A rounding function, such as `round`, `floor`, or `ceiling`. 
#' Default is `round`.
#' @noRd
round_any <- function(x, accuracy, f = round) f(x / accuracy) * accuracy

#' Null coalescing operator
#' Returns the left-hand side if it is not NULL, otherwise returns the right-hand side.
#' @param x The value to check for NULL.
#' @param y The value to return if x is NULL.
#' @noRd
`%||%` <- function(x, y) if (!is.null(x)) x else y

#' Start a PSOCK cluster with a worker-startup timeout that tolerates a slow
#' first R session.
#'
#' `parallel::makeCluster()` defaults `setup_timeout` to 120 s. A worker here
#' is a fresh R session that sources the project `.Rprofile`; when that
#' activates `renv`, building the package sandbox on the first worker can take
#' several minutes - far past 120 s - so most workers "fail to connect" when
#' really they are still starting, and the run collapses to a serial fallback
#' or aborts. The workers are not doing anything a 10-minute cap would mask a
#' real hang behind, so raise it. Overridable with the
#' `AEMETOOLS_CLUSTER_SETUP_TIMEOUT` environment variable.
#' @noRd
aeme_make_cluster <- function(ncore, outfile = "parallel.log") {
  to <- suppressWarnings(as.numeric(
    Sys.getenv("AEMETOOLS_CLUSTER_SETUP_TIMEOUT", "600")))
  if (!is.finite(to) || to <= 0) to <- 600
  parallel::makeCluster(ncore, outfile = outfile, setup_timeout = to)
}

#' Reduce an AEME observation frame's timestamp to a UTC calendar `Date`
#'
#' AEME is moving lake / level observations from a daily `Date` column to a
#' `POSIXct` `datetime` (UTC). Built-in calibration, `get_calib_periods()` and
#' the ensemble summary all compare observations against model output on the
#' calendar day, so this guarantees a `Date`-class column named `Date`:
#' \itemize{
#'   \item a `Date` column is AEME's own calendar-day column - it was reduced
#'     with knowledge of the source timezone, so it is authoritative and is
#'     taken as-is (only its class is enforced).
#'   \item otherwise a `datetime` column (the post-rename schema) is reduced
#'     with `as.Date(datetime, tz = "UTC")`, matching AEME's own
#'     "everything runs in UTC" convention.
#' }
#'
#' Lossless while observations are daily. This is the single seam where a
#' sub-daily observation-to-output alignment step plugs in once AEME provides
#' one - until then a sub-daily observation is snapped to its UTC calendar day,
#' which at least matches it to daily model output instead of dropping it.
#'
#' @param df an observation data frame (`obs$lake` / `obs$level`), or `NULL`.
#' @return `df` with a `Date` column of class `Date`; `df` unchanged when it is
#'   `NULL`, not a data frame, or carries no recognised timestamp column.
#' @noRd
obs_calendar_date <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  src <- if ("Date" %in% names(df)) {
    df$Date
  } else if ("datetime" %in% names(df)) {
    df$datetime
  } else {
    return(df)
  }
  df$Date <- if (inherits(src, "Date")) as.Date(src) else as.Date(src, tz = "UTC")
  df
}

#' Ensure a lake observations data frame has a numeric `depth` column
#'
#' AEME (>= 0.4.0) stores lake observations with a single required `depth`
#' column (nominal sampling depth, metres positive-down from the surface).
#' Older Aeme objects and CSV files instead carry the `depth_from` / `depth_to`
#' column pair. This helper accepts either layout and always returns a data
#' frame with a numeric `depth` column, so downstream code does not need to
#' care which schema the observations came from:
#' \itemize{
#'   \item `depth` present - returned as-is (coerced to numeric).
#'   \item `depth_from` and `depth_to` present - `depth` is their midpoint,
#'     the same collapse AEME applies internally.
#'   \item only `depth_from` (or only `depth_mid`) present - used directly.
#' }
#'
#' @param lake data frame of lake observations, or `NULL`.
#' @return `lake` with a numeric `depth` column, or `lake` unchanged when it is
#'   `NULL` / not a data frame / has no depth-like column.
#' @noRd
normalise_lake_obs <- function(lake) {
  if (is.null(lake) || !is.data.frame(lake)) return(lake)
  nms <- names(lake)
  if ("depth" %in% nms) {
    lake$depth <- as.numeric(lake$depth)
  } else if (all(c("depth_from", "depth_to") %in% nms)) {
    lake$depth <- (as.numeric(lake$depth_from) + as.numeric(lake$depth_to)) / 2
  } else if ("depth_from" %in% nms) {
    lake$depth <- as.numeric(lake$depth_from)
  } else if ("depth_mid" %in% nms) {
    lake$depth <- as.numeric(lake$depth_mid)
  }
  lake
}
