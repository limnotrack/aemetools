#' Nash-Sutcliffe Efficiency (NSE) fit function
#'
#' Calculates the Nash-Sutcliffe Efficiency between observed and modelled
#' values, in its conventional orientation where **higher is better**
#' (`1` = perfect fit).
#'
#' NSE is \eqn{1 - \sum (obs - sim)^2 / \sum (obs - \bar{obs})^2}, ranging
#' from `-Inf` to `1` (`1` = perfect fit, `0` = no better than the mean of
#' the observations). It is dimensionless, which makes it a reasonable
#' default when combining fit values across variables with different units
#' or magnitudes - but it is squared-error based, so it over-weights peaks
#' and under-weights errors in the low/baseline range, and it conflates
#' bias, variability and timing error into one number. See
#' \code{\link{kge}}/\code{\link{kge_prime}} for a fit function that keeps
#' those separate.
#'
#' `calib_aeme()` and `run_and_fit()` **minimise** the values returned by
#' `FUN_list` entries, so `nse()` is not suitable as a calibration
#' objective directly - use \code{\link{nse_loss}} (which returns
#' `-1 * nse(df)`) for that.
#'
#' @param df dataframe; with columns `obs` (observed values) and `model`
#' (modelled values). Rows with `NA` in either column are dropped before
#' calculating.
#'
#' @return numeric; NSE, ranging `-Inf` to `1` (`1` = perfect fit).
#'
#' @seealso \code{\link{nse_loss}} for the minimise-oriented variant used in
#' `FUN_list`.
#'
#' @examples
#' df <- data.frame(obs = c(1, 2, 3, 4), model = c(1.1, 2.1, 2.9, 4.2))
#' nse(df)
#'
#' @export
nse <- function(df) {
  ok <- stats::complete.cases(df$obs, df$model)
  obs <- df$obs[ok]
  sim <- df$model[ok]

  1 - (sum((obs - sim)^2) / sum((obs - mean(obs))^2))
}

#' Kling-Gupta Efficiency (KGE) fit function
#'
#' Calculates the Kling-Gupta Efficiency (Gupta et al. 2009) between
#' observed and modelled values, in its conventional orientation where
#' **higher is better** (`1` = perfect fit).
#'
#' KGE decomposes fit into three components instead of conflating them the
#' way \code{\link{nse}}'s single squared-error term does: correlation
#' (`r`), variability ratio (`alpha = sd(sim) / sd(obs)`) and bias ratio
#' (`beta = mean(sim) / mean(obs)`):
#' \eqn{KGE = 1 - \sqrt{(r - 1)^2 + (\alpha - 1)^2 + (\beta - 1)^2}}, with a
#' maximum of `1` (perfect fit). Being dimensionless like NSE, it is also a
#' reasonable choice when combining fit values across variables with
#' different units or magnitudes, and is generally preferred over NSE in
#' current hydrological/environmental modelling practice.
#'
#' `calib_aeme()` and `run_and_fit()` **minimise** the values returned by
#' `FUN_list` entries - use \code{\link{kge_loss}} (which returns
#' `-1 * kge(df)`) as a calibration objective.
#'
#' @inheritParams nse
#'
#' @return numeric; KGE, with a maximum of `1` (perfect fit).
#'
#' @seealso \code{\link{kge_loss}} for the minimise-oriented variant used in
#' `FUN_list`.
#'
#' @examples
#' df <- data.frame(obs = c(1, 2, 3, 4), model = c(1.1, 2.1, 2.9, 4.2))
#' kge(df)
#'
#' @export
kge <- function(df) {
  ok <- stats::complete.cases(df$obs, df$model)
  obs <- df$obs[ok]
  sim <- df$model[ok]

  r <- stats::cor(obs, sim)
  alpha <- stats::sd(sim) / stats::sd(obs)
  beta <- mean(sim) / mean(obs)

  1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
}

#' Modified Kling-Gupta Efficiency (KGE') fit function
#'
#' As \code{\link{kge}}, but replaces the raw variability ratio with a
#' coefficient-of-variation ratio (Kling et al. 2012),
#' \eqn{\gamma = (sd(sim) / mean(sim)) / (sd(obs) / mean(obs))}, which
#' decouples the variability term from the bias term more cleanly. This is
#' generally the recommended default over the original KGE. Returned in its
#' conventional orientation where **higher is better** (`1` = perfect fit).
#'
#' `calib_aeme()` and `run_and_fit()` **minimise** the values returned by
#' `FUN_list` entries - use \code{\link{kge_prime_loss}} (which returns
#' `-1 * kge_prime(df)`) as a calibration objective.
#'
#' @inheritParams nse
#'
#' @return numeric; KGE', with a maximum of `1` (perfect fit).
#'
#' @seealso \code{\link{kge_prime_loss}} for the minimise-oriented variant
#' used in `FUN_list`.
#'
#' @examples
#' df <- data.frame(obs = c(1, 2, 3, 4), model = c(1.1, 2.1, 2.9, 4.2))
#' kge_prime(df)
#'
#' @export
kge_prime <- function(df) {
  ok <- stats::complete.cases(df$obs, df$model)
  obs <- df$obs[ok]
  sim <- df$model[ok]

  r <- stats::cor(obs, sim)
  beta <- mean(sim) / mean(obs)
  gamma <- (stats::sd(sim) / mean(sim)) / (stats::sd(obs) / mean(obs))

  1 - sqrt((r - 1)^2 + (gamma - 1)^2 + (beta - 1)^2)
}

#' Log-transformed Kling-Gupta Efficiency fit function
#'
#' \code{\link{kge}} calculated on `log1p()`-transformed observed and
#' modelled values, for skewed/concentration-type variables (e.g. oxygen,
#' chlorophyll, nutrients) where a few peak events would otherwise dominate
#' the fit. `log1p()` tolerates zeros but not negative values - not
#' suitable for variables that can be negative. Returned in its conventional
#' orientation where **higher is better** (`1` = perfect fit).
#'
#' `calib_aeme()` and `run_and_fit()` **minimise** the values returned by
#' `FUN_list` entries - use \code{\link{log_kge_loss}} (which returns
#' `-1 * log_kge(df)`) as a calibration objective.
#'
#' @inheritParams nse
#'
#' @return numeric; KGE calculated on `log1p(obs)`/`log1p(model)`, with a
#' maximum of `1` (perfect fit).
#'
#' @seealso \code{\link{log_kge_loss}} for the minimise-oriented variant
#' used in `FUN_list`.
#'
#' @examples
#' df <- data.frame(obs = c(1, 2, 3, 4), model = c(1.1, 2.1, 2.9, 4.2))
#' log_kge(df)
#'
#' @export
log_kge <- function(df) {
  df$obs <- log1p(df$obs)
  df$model <- log1p(df$model)
  kge(df)
}

#' Minimise-oriented (loss) variants of NSE / KGE fit functions
#'
#' \code{\link{nse}}, \code{\link{kge}}, \code{\link{kge_prime}} and
#' \code{\link{log_kge}} return their conventional statistic, where `1` is a
#' perfect fit and higher is better. `calib_aeme()` and `run_and_fit()`
#' instead **minimise** the values returned by `FUN_list` entries, so these
#' `_loss` companions return `-1 *` the corresponding statistic (lower is
#' better, `-1` = perfect fit) and are what you pass in `FUN_list` for
#' calibration:
#'
#' \preformatted{FUN_list <- list(HYD_temp = kge_loss, LKE_lvlwtr = rmse)}
#'
#' \code{\link{mae}}, \code{\link{rmse}} and \code{\link{pbias}} are already
#' `0`-is-best, minimise-oriented, so they have no `_loss` companion - use
#' them directly.
#'
#' @inheritParams nse
#'
#' @return numeric; `-1 *` the corresponding statistic (`-1` = perfect fit,
#' higher = worse fit).
#'
#' @seealso \code{\link{nse}}, \code{\link{kge}}, \code{\link{kge_prime}},
#' \code{\link{log_kge}} for the conventional (higher-is-better) statistics.
#'
#' @examples
#' df <- data.frame(obs = c(1, 2, 3, 4), model = c(1.1, 2.1, 2.9, 4.2))
#' nse_loss(df)
#' kge_loss(df)
#' kge_prime_loss(df)
#' log_kge_loss(df)
#'
#' @name fit_loss
NULL

#' @rdname fit_loss
#' @export
nse_loss <- function(df) -1 * nse(df)

#' @rdname fit_loss
#' @export
kge_loss <- function(df) -1 * kge(df)

#' @rdname fit_loss
#' @export
kge_prime_loss <- function(df) -1 * kge_prime(df)

#' @rdname fit_loss
#' @export
log_kge_loss <- function(df) -1 * log_kge(df)

#' Mean Absolute Error (MAE) fit function
#'
#' Calculates the mean absolute error between observed and modelled values.
#' Already `0` (perfect fit) at its best and increasing as fit worsens, so
#' it is minimise-oriented as-is and needs no `_loss` companion - pass it
#' straight into `FUN_list`, which \code{\link{calib_aeme}} and
#' \code{\link{run_and_fit}} minimise.
#'
#' MAE stays in the variable's native units, so it is **not** directly
#' comparable across variables with different units or magnitudes -
#' summing MAE from, say, a temperature fit (degC) and an oxygen fit (mg/L)
#' in a multi-variable `FUN_list` lets whichever variable happens to have
#' the larger natural magnitude dominate the combined fit. Prefer a
#' dimensionless metric such as \code{\link{nse_loss}} or
#' \code{\link{kge_loss}} when combining fits across variables; MAE/RMSE are
#' more suited to single-variable calibration or to reporting fit in
#' interpretable, original units.
#'
#' @inheritParams nse
#'
#' @return numeric; mean absolute error, in the same units as `obs`/`model`.
#'
#' @examples
#' df <- data.frame(obs = c(1, 2, 3, 4), model = c(1.1, 2.1, 2.9, 4.2))
#' mae(df)
#'
#' @export
mae <- function(df) {
  ok <- stats::complete.cases(df$obs, df$model)
  obs <- df$obs[ok]
  sim <- df$model[ok]

  mean(abs(obs - sim))
}

#' Root Mean Square Error (RMSE) fit function
#'
#' Calculates the root mean square error between observed and modelled
#' values. Already `0` (perfect fit) at its best and increasing as fit
#' worsens, so it is minimise-oriented as-is and needs no `_loss` companion
#' - pass it straight into `FUN_list`, which \code{\link{calib_aeme}} and
#' \code{\link{run_and_fit}} minimise.
#'
#' Like \code{\link{mae}}, RMSE stays in the variable's native units and so
#' is not directly comparable across variables with different units or
#' magnitudes when combined in a multi-variable `FUN_list` - see
#' \code{\link{mae}} for details. RMSE additionally squares errors before
#' averaging, so - like NSE - it weights large deviations more heavily than
#' MAE does.
#'
#' @inheritParams nse
#'
#' @return numeric; root mean square error, in the same units as
#' `obs`/`model`.
#'
#' @examples
#' df <- data.frame(obs = c(1, 2, 3, 4), model = c(1.1, 2.1, 2.9, 4.2))
#' rmse(df)
#'
#' @export
rmse <- function(df) {
  ok <- stats::complete.cases(df$obs, df$model)
  obs <- df$obs[ok]
  sim <- df$model[ok]

  sqrt(mean((obs - sim)^2))
}

#' Percent Bias (PBIAS) fit function
#'
#' Calculates the absolute percent bias between observed and modelled
#' values - the average tendency of the modelled values to be larger
#' (positive bias) or smaller (negative bias) than the observed values,
#' expressed as a percentage of the observed total. Already `0` (perfect
#' fit, no systematic bias) at its best and increasing as fit worsens in
#' *either* direction, so it is minimise-oriented as-is and needs no `_loss`
#' companion - pass it straight into `FUN_list`, which
#' \code{\link{calib_aeme}} and \code{\link{run_and_fit}} minimise. The
#' absolute value is used rather than the signed value, since an equally
#' large over- or under-estimate is an equally poor fit - minimising the
#' signed value would instead push the calibration towards the most negative
#' (under-estimating) bias possible.
#'
#' PBIAS is \eqn{100 \times |\sum (sim - obs) / \sum obs|}. Unlike
#' \code{\link{mae}}/\code{\link{rmse}}, it is expressed as a percentage of
#' the observed total rather than in the variable's native units, which
#' makes it directly comparable across variables with different units or
#' magnitudes - similar to \code{\link{nse}}/\code{\link{kge}} in that
#' respect. On its own it only captures systematic over/under-estimation,
#' not timing, variability or shape - it is typically combined with NSE or
#' KGE (which are largely insensitive to a consistent bias) rather than
#' used alone. See Moriasi et al. (2007) for commonly used PBIAS
#' performance thresholds.
#'
#' @inheritParams nse
#'
#' @return numeric; absolute percent bias.
#'
#' @examples
#' df <- data.frame(obs = c(1, 2, 3, 4), model = c(1.1, 2.1, 2.9, 4.2))
#' pbias(df)
#'
#' @export
pbias <- function(df) {
  ok <- stats::complete.cases(df$obs, df$model)
  obs <- df$obs[ok]
  sim <- df$model[ok]

  abs(100 * sum(sim - obs) / sum(obs))
}

#' Mean bias fit function
#'
#' Calculates the mean difference between modelled and observed values, in
#' the variable's native units - positive when the model overestimates on
#' average, negative when it underestimates. Unlike every other function in
#' this file, the raw (signed) value is returned rather than a
#' zero-is-best, non-negative one.
#'
#' **This makes `bias()` unsafe to drop directly into `FUN_list` for
#' calibration**: since `calib_aeme()`/`run_and_fit()` minimise the
#' returned value, minimising a signed bias would push the calibration
#' towards the most negative (maximally under-estimating) solution rather
#' than towards zero bias. Use \code{\link{pbias}} (or `abs(bias(df))`) as
#' a calibration objective; use `bias()` for diagnosing the *direction* of
#' a systematic error when inspecting fit after the fact. Like
#' \code{\link{mae}}/\code{\link{rmse}}, it stays in the variable's native
#' units and so is not directly comparable across variables with different
#' units or magnitudes.
#'
#' @inheritParams nse
#'
#' @return numeric; `mean(model - obs)`, signed, in the same units as
#' `obs`/`model`.
#'
#' @examples
#' df <- data.frame(obs = c(1, 2, 3, 4), model = c(1.1, 2.1, 2.9, 4.2))
#' bias(df)
#'
#' @export
bias <- function(df) {
  ok <- stats::complete.cases(df$obs, df$model)
  obs <- df$obs[ok]
  sim <- df$model[ok]

  mean(sim - obs)
}
