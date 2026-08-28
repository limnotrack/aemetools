#' Build a prior parameter covariance matrix
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Turns the calibration bounds into a diagonal prior parameter covariance,
#' the way `pyemu.Cov.from_parameter_data()` does: each parameter's standard
#' deviation is a fixed fraction of its feasible range, so that the range
#' spans `sigma_range` standard deviations. Log-transformed parameters are
#' treated in log10 space, matching how \code{\link{pest_param_table}} hands
#' them to PEST++.
#'
#' The result is written in the PEST ASCII matrix format and can be passed to
#' `pestpp-ies` as `++parcov(...)` (via `prior_cov` in
#' \code{\link{create_pest_control}}) so the prior ensemble is drawn from a
#' Gaussian rather than from independent uniforms on the bounds.
#'
#' @param param dataframe; as passed to \code{\link{calib_aeme}} - see
#'   \code{\link{pest_param_table}} for the required columns.
#' @param sigma_range Numeric. Number of standard deviations spanned by the
#'   `[min, max]` range. Default `4`, the `pyemu` default (so `sd` is a
#'   quarter of the range, or of the log10 range for a log parameter).
#' @param file Character or `NULL`. When given, the matrix is also written to
#'   this path in PEST ASCII matrix format.
#'
#' @return Invisibly, a square numeric matrix with `parnme` row/column names
#'   (`p001`, `p002`, ...), carrying the same `map` attribute as
#'   \code{\link{pest_param_table}} so the synthetic names can be related back
#'   to the aemetools parameters.
#' @seealso [pest_prior_ensemble()], [create_pest_control()]
#' @export
#'
#' @examples
#' param <- data.frame(model = "glm_aed", file = "glm3.nml",
#'                     group = "light", name = c("Kw", "ce"),
#'                     index = c(1, NA), value = c(0.5, 0.0013),
#'                     min = c(0.1, 0.0005), max = c(1.5, 0.005),
#'                     log = c(TRUE, FALSE))
#' pest_prior_cov(param)
pest_prior_cov <- function(param, sigma_range = 4, file = NULL) {

  if (!is.numeric(sigma_range) || length(sigma_range) != 1 || sigma_range <= 0) {
    cli::cli_abort("{.arg sigma_range} must be a single positive number.")
  }

  par_tbl <- pest_param_table(param)
  sd <- .pest_prior_sd(par_tbl, sigma_range)

  cov <- diag(sd^2, nrow = length(sd))
  dimnames(cov) <- list(par_tbl$parnme, par_tbl$parnme)
  attr(cov, "map") <- attr(par_tbl, "map")

  if (!is.null(file)) {
    .pest_write_cov(cov, file)
    AEME::cli_safe(paste0("Wrote prior covariance {.file ", file, "}"),
                   FUN = cli::cli_alert_success)
  }
  invisible(cov)
}

#' Draw a prior parameter ensemble for `pestpp-ies`
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Generates the initial (prior) parameter ensemble in R instead of letting
#' `pestpp-ies` draw it internally from independent uniforms on the bounds.
#' This buys three things the internal draw cannot: a fixed `seed` for
#' reproducibility, a non-uniform prior (Gaussian about the initial values,
#' or triangular), and an optional prior covariance so parameters can be
#' drawn correlated.
#'
#' Pass the written file to `pestpp-ies` as `++ies_parameter_ensemble(...)`.
#' \code{\link{calib_aeme}} does this automatically when
#' \code{\link{create_pest_control}} is given a `seed`, a `prior_cov`, or a
#' non-uniform `prior_dist`.
#'
#' @inheritParams pest_prior_cov
#' @param n Integer. Ensemble size (number of realisations). Default `100`.
#' @param dist Character. `"uniform"` (independent uniforms on the bounds),
#'   `"normal"` (Gaussian about `value`), or `"triangular"` (mode at
#'   `value`, limits at the bounds). Log parameters are drawn in log10 space
#'   and back-transformed.
#' @param cov Optional. A prior covariance matrix from
#'   \code{\link{pest_prior_cov}} (or a path to one). Only used when
#'   `dist = "normal"`: its diagonal sets the per-parameter spread, and a
#'   non-diagonal matrix draws correlated realisations via
#'   \code{\link[MASS]{mvrnorm}}.
#' @param seed Integer or `NULL`. Seed for the draw, for reproducibility.
#' @param include_base Logical. Replace the last realisation with the initial
#'   parameter values and name it `base`, mirroring `pestpp-ies`'
#'   `ies_include_base`. Keeps the ensemble size at `n`. Default `TRUE`.
#' @param real_names Character or `NULL`. Realisation names. Defaults to
#'   `real_0 ... real_{n-1}` (with the last renamed `base` when
#'   `include_base`). Supply this to align a parameter and an observation
#'   ensemble, which `pestpp-ies` matches by name.
#' @param file Character or `NULL`. Path to write the ensemble CSV to, in the
#'   `real_name, <par>, <par>, ...` layout `pestpp-ies` expects.
#'
#' @importFrom stats runif rnorm setNames
#' @importFrom MASS mvrnorm
#'
#' @return Invisibly, a dataframe with a `real_name` column and one column
#'   per `parnme`, values in native (untransformed) units.
#' @seealso [pest_obs_ensemble()], [pest_prior_cov()], [create_pest_control()]
#' @export
pest_prior_ensemble <- function(param, n = 100, dist = "uniform", cov = NULL,
                                sigma_range = 4, seed = NULL,
                                include_base = TRUE, real_names = NULL,
                                file = NULL) {

  dist <- rlang::arg_match(dist, c("uniform", "normal", "triangular"))
  n <- as.integer(n)
  if (is.na(n) || n < 2) cli::cli_abort("{.arg n} must be at least 2.")

  par_tbl <- pest_param_table(param)
  np <- nrow(par_tbl)
  islog <- par_tbl$partrans == "log"

  # Sampling space: log10 for log-transformed parameters, native otherwise.
  to_s <- function(x) ifelse(islog, log10(x), x)
  lo <- to_s(par_tbl$parlbnd)
  hi <- to_s(par_tbl$parubnd)
  mid <- to_s(par_tbl$parval1)

  if (!is.null(seed)) set.seed(as.integer(seed))

  M <- matrix(NA_real_, nrow = n, ncol = np,
              dimnames = list(NULL, par_tbl$parnme))

  if (dist == "uniform") {
    for (j in seq_len(np)) M[, j] <- stats::runif(n, lo[j], hi[j])

  } else if (dist == "triangular") {
    for (j in seq_len(np)) {
      M[, j] <- .pest_rtriangle(n, lo[j], hi[j], mid[j])
    }

  } else {                                                    # normal
    covm <- .pest_resolve_cov(cov, par_tbl)
    if (is.null(covm)) {
      sd <- .pest_prior_sd(par_tbl, sigma_range)
      for (j in seq_len(np)) M[, j] <- stats::rnorm(n, mid[j], sd[j])
    } else if (isTRUE(all.equal(covm, diag(diag(covm), nrow = np),
                                check.attributes = FALSE))) {
      sd <- sqrt(diag(covm))
      for (j in seq_len(np)) M[, j] <- stats::rnorm(n, mid[j], sd[j])
    } else {
      M[] <- MASS::mvrnorm(n, mu = mid, Sigma = covm)
    }
  }

  # Back-transform and clip to the feasible range (reset out-of-bounds draws).
  X <- M
  if (any(islog)) X[, islog] <- 10^M[, islog, drop = FALSE]
  for (j in seq_len(np)) {
    X[, j] <- pmin(pmax(X[, j], par_tbl$parlbnd[j]), par_tbl$parubnd[j])
  }

  rn <- .pest_real_names(real_names, n, include_base)
  if (include_base) X[n, ] <- par_tbl$parval1

  out <- data.frame(real_name = rn, X, check.names = FALSE,
                    stringsAsFactors = FALSE)

  if (!is.null(file)) {
    # Unquoted, to match PEST++'s own ensemble CSVs - its parser does not
    # strip quotes from the header, so "p001" would not match p001.
    utils::write.csv(out, file, row.names = FALSE, quote = FALSE)
    AEME::cli_safe(paste0("Wrote prior parameter ensemble {.file ", file,
                          "} ({.val ", n, "} realisations)."),
                   FUN = cli::cli_alert_success)
  }
  invisible(out)
}

#' Draw an observation (measurement-noise) ensemble for `pestpp-ies`
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Builds the observation ensemble `pestpp-ies` uses to represent
#' measurement noise, so that the noise is set from a physical measurement
#' error rather than inferred from the PEST weights - which, after
#' \code{\link{pest_obs_table}}'s `"balanced"` weighting, no longer carry a
#' meaningful `1/sigma` interpretation.
#'
#' Pass the written file as `++ies_observation_ensemble(...)`.
#' \code{\link{calib_aeme}} does this automatically when
#' \code{\link{create_pest_control}} is given `noise_sd`.
#'
#' @param obs_tbl dataframe; from \code{\link{pest_obs_table}}, carrying its
#'   `map` attribute.
#' @param n Integer. Ensemble size. Must match the parameter ensemble.
#'   Default `100`.
#' @param noise_sd Named numeric vector, keyed by `var_aeme`, giving the
#'   measurement standard deviation for each variable in data units (e.g.
#'   `c(HYD_temp = 0.5)`). When `NULL`, the standard deviation is taken as
#'   `1/weight` per observation and a warning is issued.
#' @param seed Integer or `NULL`. Seed for the draw.
#' @param include_base Logical. Replace the last realisation with the
#'   noise-free observed values and name it `base`. Default `TRUE`.
#' @param real_names Character or `NULL`. Realisation names; must equal the
#'   parameter ensemble's. Defaults as in \code{\link{pest_prior_ensemble}}.
#' @param file Character or `NULL`. Path to write the ensemble CSV to.
#'
#' @importFrom stats rnorm setNames
#'
#' @return Invisibly, a dataframe with a `real_name` column and one column
#'   per `obsnme`.
#' @seealso [pest_prior_ensemble()], [create_pest_control()]
#' @export
pest_obs_ensemble <- function(obs_tbl, n = 100, noise_sd = NULL, seed = NULL,
                              include_base = TRUE, real_names = NULL,
                              file = NULL) {

  n <- as.integer(n)
  if (is.na(n) || n < 2) cli::cli_abort("{.arg n} must be at least 2.")

  map <- attr(obs_tbl, "map")
  if (is.null(map)) {
    cli::cli_abort("{.arg obs_tbl} must carry the {.field map} attribute from
                   {.fn pest_obs_table}.")
  }
  var_by_obs <- map$var_aeme[match(obs_tbl$obsnme, map$obsnme)]

  if (is.null(noise_sd)) {
    AEME::cli_safe(
      paste0("No {.arg noise_sd} given; taking measurement noise as ",
             "{.code 1/weight}. After {.code weight_method = \"balanced\"} ",
             "that is not a physical error - pass {.arg noise_sd} per ",
             "variable instead."),
      FUN = cli::cli_alert_warning)
    sd_i <- ifelse(obs_tbl$weight > 0, 1 / obs_tbl$weight, 0)
  } else {
    miss <- setdiff(unique(var_by_obs), names(noise_sd))
    if (length(miss) > 0) {
      cli::cli_abort("{.arg noise_sd} has no entry for {.val {miss}}.")
    }
    sd_i <- as.numeric(noise_sd[var_by_obs])
    sd_i[obs_tbl$weight <= 0] <- 0            # zero-weight obs carry no noise
  }

  if (!is.null(seed)) set.seed(as.integer(seed))

  nobs <- nrow(obs_tbl)
  E <- matrix(NA_real_, nrow = n, ncol = nobs,
              dimnames = list(NULL, obs_tbl$obsnme))
  for (i in seq_len(nobs)) {
    E[, i] <- if (sd_i[i] > 0) {
      stats::rnorm(n, obs_tbl$obsval[i], sd_i[i])
    } else {
      rep(obs_tbl$obsval[i], n)
    }
  }

  rn <- .pest_real_names(real_names, n, include_base)
  if (include_base) E[n, ] <- obs_tbl$obsval

  out <- data.frame(real_name = rn, E, check.names = FALSE,
                    stringsAsFactors = FALSE)

  if (!is.null(file)) {
    utils::write.csv(out, file, row.names = FALSE, quote = FALSE)
    AEME::cli_safe(paste0("Wrote observation ensemble {.file ", file, "} ",
                          "({.val ", n, "} realisations)."),
                   FUN = cli::cli_alert_success)
  }
  invisible(out)
}

# Internal helpers -------------------------------------------------------

#' Per-parameter prior standard deviation from the bounds, in the space PEST
#' works in (log10 for log-transformed parameters).
#' @noRd
.pest_prior_sd <- function(par_tbl, sigma_range) {
  islog <- par_tbl$partrans == "log"
  rng <- ifelse(islog,
                log10(par_tbl$parubnd) - log10(par_tbl$parlbnd),
                par_tbl$parubnd - par_tbl$parlbnd)
  rng / sigma_range
}

#' Inverse-CDF draw from a triangular distribution on the interval a..b with
#' mode c.
#' @noRd
.pest_rtriangle <- function(n, a, b, c) {
  if (!(a < b)) return(rep(a, n))
  c <- min(max(c, a), b)
  u <- stats::runif(n)
  fc <- (c - a) / (b - a)
  ifelse(u < fc,
         a + sqrt(u * (b - a) * (c - a)),
         b - sqrt((1 - u) * (b - a) * (b - c)))
}

#' Realisation names: real_0..real_{n-1}, last renamed "base" when requested.
#' @noRd
.pest_real_names <- function(real_names, n, include_base) {
  if (is.null(real_names)) {
    rn <- paste0("real_", seq_len(n) - 1L)
    if (include_base) rn[n] <- "base"
    return(rn)
  }
  if (length(real_names) != n) {
    cli::cli_abort("{.arg real_names} must have length {.val {n}}.")
  }
  as.character(real_names)
}

#' Coerce `cov` (a matrix, a `pest_prior_cov()` result, or a file path) to a
#' plain numeric matrix ordered to match `par_tbl$parnme`, or NULL.
#' @noRd
.pest_resolve_cov <- function(cov, par_tbl) {
  if (is.null(cov)) return(NULL)
  if (is.character(cov)) cov <- .pest_read_cov(cov)
  cov <- as.matrix(cov)
  if (is.null(dimnames(cov)) || is.null(rownames(cov))) {
    if (nrow(cov) != nrow(par_tbl)) {
      cli::cli_abort("Unnamed {.arg cov} must be {.val {nrow(par_tbl)}} x
                     {.val {nrow(par_tbl)}}.")
    }
    dimnames(cov) <- list(par_tbl$parnme, par_tbl$parnme)
  }
  miss <- setdiff(par_tbl$parnme, rownames(cov))
  if (length(miss) > 0) {
    cli::cli_abort("{.arg cov} is missing parameter{?s}: {.val {miss}}.")
  }
  cov[par_tbl$parnme, par_tbl$parnme, drop = FALSE]
}

#' Write a covariance matrix in PEST ASCII matrix format (icode 2, full).
#' @noRd
.pest_write_cov <- function(mat, file) {
  nr <- nrow(mat)
  nc <- ncol(mat)
  body <- apply(mat, 1, function(r) {
    paste(formatC(r, format = "e", digits = 8, width = 16), collapse = " ")
  })
  writeLines(c(
    sprintf("%d %d 2", nr, nc),
    body,
    "* row names",
    rownames(mat),
    "* column names",
    colnames(mat)
  ), file)
  invisible(file)
}

#' Write per-observation measurement standard deviations as a PEST
#' uncertainty file (`START STANDARD_DEVIATION` block), for `++obscov(...)`.
#'
#' A diagonal `.unc` rather than a matrix `.cov`: `pestpp-ies` refuses a
#' "full" obscov together with `ies_drop_conflicts`, but accepts the
#' standard-deviation form.
#' @noRd
.pest_write_obscov <- function(obs_tbl, noise_sd, file) {
  map <- attr(obs_tbl, "map")
  var_by_obs <- if (is.null(map)) {
    rep(NA_character_, nrow(obs_tbl))
  } else {
    map$var_aeme[match(obs_tbl$obsnme, map$obsnme)]
  }
  miss <- setdiff(unique(var_by_obs), names(noise_sd))
  if (length(miss) > 0) {
    cli::cli_abort("{.arg noise_sd} has no entry for {.val {miss}}.")
  }
  sd_i <- as.numeric(noise_sd[var_by_obs])
  # A zero-weight observation would give a zero standard deviation, which
  # PEST cannot invert; give it a large one instead (it carries no weight).
  bad <- !is.finite(sd_i) | sd_i <= 0
  if (any(bad)) sd_i[bad] <- max(c(sd_i[!bad], 1))

  writeLines(c(
    "START STANDARD_DEVIATION",
    sprintf("  %-20s  %15.6E", obs_tbl$obsnme, sd_i),
    "END STANDARD_DEVIATION"
  ), file)
  invisible(file)
}

#' Read a `START STANDARD_DEVIATION` uncertainty file to a named vector.
#' @noRd
.pest_read_unc_sd <- function(file) {
  l <- trimws(readLines(file, warn = FALSE))
  l <- l[nzchar(l)]
  body <- l[!grepl("^(START|END)\\b", l, ignore.case = TRUE)]
  body <- body[!grepl("^std_multiplier", body, ignore.case = TRUE)]
  parts <- strsplit(body, "\\s+")
  stats::setNames(as.numeric(vapply(parts, `[`, character(1), 2)),
                  vapply(parts, `[`, character(1), 1))
}

#' Read a PEST ASCII matrix file written by `.pest_write_cov()`.
#' Minimal parser: supports icode 2 (full) and icode 1 (diagonal), which is
#' all this package writes or needs for round-trip tests.
#' @noRd
.pest_read_cov <- function(file) {
  l <- trimws(readLines(file, warn = FALSE))
  l <- l[nzchar(l)]
  hdr <- as.integer(strsplit(l[1], "\\s+")[[1]])
  nr <- hdr[1]; nc <- hdr[2]; icode <- hdr[3]

  rn_at <- grep("^\\*\\s*row", l, ignore.case = TRUE)
  cn_at <- grep("^\\*\\s*col", l, ignore.case = TRUE)
  val_lines <- l[2:(rn_at - 1)]
  vals <- as.numeric(unlist(strsplit(val_lines, "\\s+")))

  m <- if (icode == 1) {
    diag(vals, nrow = nr)
  } else {
    matrix(vals, nrow = nr, ncol = nc, byrow = TRUE)
  }
  rownames(m) <- l[(rn_at + 1):(rn_at + nr)]
  colnames(m) <- l[(cn_at + 1):(cn_at + nc)]
  m
}
