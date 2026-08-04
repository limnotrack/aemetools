#' Encode a parameter name with its group and index.
#' @param group The group name.
#' @param name The parameter name.
#' @param index The index of the parameter.
#' @return A string in the format "group/name\[index]".
#' @export
encode_param <- function(group, name, index) {
  paste0(group, "/", name, "[", index, "]")
}

#' Display parameter information in a human-readable format.
#' @param group The group name (can be NA).
#' @param name The parameter name.
#' @param index The index of the parameter (can be NA).
display_param_name <- function(group, name, index) {
  grp <- ifelse(is.na(group), "", paste0(group, "/"))
  idx <- ifelse(is.na(index), "", paste0("[", index, "]"))
  paste0(grp, name, idx)
}

#' Decode a full parameter name to extract the base name.
#' @param name_full The full parameter name in the format "group/name\[index]".
#' @return The base parameter name without group and index.
#' @export
decode_param <- function(name_full) {
  sub("^[^/]*/(.*)\\[.*\\]$", "\\1", name_full)
}

#' Decode a full parameter name into group, name, and index.
#' @param name_full The full parameter name in the format
#' "group/.../name\[index]".
#' @return A data.frame with columns: group, name, index.
#' @export
decode_param_full <- function(name_full) {
  
  # Extract group = everything before the last "/" occurrence
  group <- sub("/.*$", "", name_full)
  group <- ifelse(group == "NA", NA_character_, group)
  
  # Extract name = the part after the last "/"
  name  <- sub("^[^/]*/(.*)\\[.*\\]$", "\\1", name_full)
  
  # Extract index = inside [...]
  raw_index <- sub(".*\\[(.*)\\]$", "\\1", name_full)
  
  # Convert "NA" to real NA, keep numbers
  index <- suppressWarnings(as.integer(raw_index))
  index[raw_index == "NA"] <- NA_integer_
  
  data.frame(
    group = group,
    name = name,
    index = index,
    stringsAsFactors = FALSE
  )
}

#' Check if a solution is dominated by any other solution in the objective space.
#' This function is used to identify whether a given solution (row) in the objective matrix
#' is dominated by any other solution. A solution i is dominated if there exists another solution j such that
#' all objective values of j are less than or equal to those of i, and at
#' least one objective value of j is strictly less than that of i.
#' @param i The index of the solution to check for dominance.
#' @return TRUE if the solution is dominated, FALSE otherwise.
#' @noRd
is_dominated <- function(obj, i) {
  any(apply(obj, 1, function(j)
    all(j <= obj[i, ]) && any(j < obj[i, ])
  ))
}

#' Get the Pareto front from a data frame based on specified objective columns.
#' @param df A data frame containing the results of a multi-objective optimization.
#' @param obj_cols A character vector of column names in df that represent the
#' objective values to be minimized. The function will identify the rows that are
#' not dominated by any other row in terms of these objective values.
#' @return A data frame containing only the rows that are on the Pareto front.
#' @export
get_pareto_front <- function(df, obj_cols) {
  obj <- as.matrix(df[, obj_cols])
  pareto_idx <- which(!sapply(seq_len(nrow(obj)), is_dominated, obj = obj))
  
  df[pareto_idx, ]
}

#' Create a control object for calibration or sensitivity analysis.
#' @noRd
.create_control <- function(method, ...) {
  
  args <- list(...)
  
  if (method == "calib") {
    
    required <- c("VTR", "NP", "itermax", "reltol",
                  "cutoff", "mutate", "c_method")
    
  } else if (method == "sa") {
    
    required <- c("N", "vars_sim")
    
  } else {
    cli::cli_abort("Unknown method: {.val {method}} Can only be 'calib' or 
                   'sa'.")
  }
  
  # Optional: enforce required args present
  missing <- setdiff(required, names(args))
  if (length(missing) > 0) {
    cli::cli_abort("Missing required arguments: {.arg {missing}}")
  }
  if (is.null(args$file_name)) {
    if (args$file_type == "db") {
      args$file_name <- "results.db"
    } else if (args$file_type == "csv") {
      args$file_name <- "simulation_metadata.csv"
    }
  }
  
  args$method <- method
  class(args) <- "calib_sa_control"
  args
}

#' Print a calibration or sensitivity analysis control object
#'
#' @param x a `calib_sa_control` object, as created by
#' \code{\link{create_calib_control}} or \code{\link{create_sa_control}}.
#' @param ... further arguments passed to or from other methods (unused).
#'
#' @importFrom cli cli_h1 cli_dl
#'
#' @return `x`, invisibly.
#' @export
print.calib_sa_control <- function(x, ...) {
  method_label <- switch(x$method,
                         calib = "Calibration control",
                         sa = "Sensitivity analysis control",
                         "Control")
  cli::cli_h1(method_label)

  fields <- x[setdiff(names(x), "method")]
  vals <- vapply(fields, function(v) {
    if (is.null(v)) {
      "NULL"
    } else if (is.atomic(v)) {
      paste(format(v), collapse = ", ")
    } else {
      sprintf("<%s>", class(v)[1])
    }
  }, character(1))
  cli::cli_dl(stats::setNames(vals, names(fields)))

  invisible(x)
}

#' Resolve the NA value to use for calibration results.
#' This function checks if the user has provided an `na_value` argument. If not
#' it retrieves the default `na_value` from the calibration metadata. This 
#' ensures that the same `na_value` is consistently used across all functions 
#' that require it, even if the user does not explicitly provide it.
#' @param na_value The NA value provided by the user (can be NULL).
#' @param calib The calibration object containing metadata with the default NA value.
#' @return The resolved NA value to use for calibration results.
#' @noRd
#' @importFrom rlang `%||%`
resolve_na_value <- function(na_value, calib) {
  na_value %||% calib$calibration_metadata$na_value[1]
}

#' Check whether a fit value (or vector of per-variable fit values)
#' indicates a failed simulation.
#' @param fit numeric vector; combined or per-variable fit value(s).
#' @param ctrl list; control object holding `na_value`.
#' @return logical vector, the same length as `fit`.
#' @noRd
is_failed_fit <- function(fit, ctrl) {
  fit == ctrl$na_value
}

#' Reflect out-of-range values back into `lo, hi` instead of clamping
#' them to the boundary.
#'
#' Clamping out-of-range candidate parameter values to the nearest bound
#' causes them to pile up exactly on that bound over successive
#' generations, which can collapse the parameter's sample variance towards
#' zero (see regularize_cov()) and stall the search there. Reflecting
#' keeps the same `lo, hi` support without that pile-up.
#'
#' @param x numeric vector.
#' @param lo,hi numeric; lower/upper bounds.
#' @return `x` reflected off `lo`/`hi`, then clamped in case the overshoot
#' was larger than the `[lo, hi]` range.
#' @noRd
reflect_bounds <- function(x, lo, hi) {
  x <- ifelse(x < lo, lo + (lo - x), x)
  x <- ifelse(x > hi, hi - (x - hi), x)
  pmin(pmax(x, lo), hi)
}

#' Enforce a minimum-variance floor on a covariance matrix before it is
#' used to sample a new generation with `MASS::mvrnorm()`.
#'
#' Repeated boundary clipping (or a survivor set that happens to agree on a
#' parameter value) can drive a parameter's sample variance towards zero
#' across generations, collapsing the search around that value. Flooring
#' the diagonal keeps a minimum amount of exploration for every parameter.
#'
#' @param Sigma covariance matrix; column/row names must match
#' `param$name_full`.
#' @param param dataframe; with columns `name_full`, `min`, `max`.
#' @param min_frac numeric; minimum standard deviation as a fraction of
#' each parameter's `[min, max]` range. Default `0.01` (1% of the range).
#' @return `Sigma` with `diag(Sigma)` floored.
#' @noRd
regularize_cov <- function(Sigma, param, min_frac = 0.01) {
  idx <- match(colnames(Sigma), param$name_full)
  min_sd <- min_frac * (param$max[idx] - param$min[idx])
  diag(Sigma) <- pmax(diag(Sigma), min_sd^2)
  Sigma
}

#' Estimate a covariance matrix for sampling the next generation, using
#' shrinkage when there are enough points to support it and falling back
#' gracefully when there aren't.
#'
#' `corpcor::cov.shrink()` errors outright ("Sample size too small!") with
#' fewer than 3 rows, since it can't estimate the variance-shrinkage
#' intensity from so little data. With only 1-2 points there also isn't
#' enough information to estimate any correlation at all, so this falls
#' back to a diagonal (zero-correlation) matrix in that case, and to the
#' raw sample covariance for exactly 2 rows. `regularize_cov()` is always
#' applied afterwards to floor the variance.
#'
#' @param pf dataframe; parameter values to estimate a covariance matrix
#' from (rows = individuals, columns = parameters).
#' @inheritParams calib_aeme
#' @return covariance matrix, regularized via `regularize_cov()`.
#' @noRd
estimate_shrunk_cov <- function(pf, param) {
  n <- nrow(pf)
  Sigma <- if (n >= 3) {
    corpcor::cov.shrink(as.matrix(pf), verbose = FALSE)
  } else if (n == 2) {
    stats::cov(pf)
  } else {
    matrix(0, nrow = ncol(pf), ncol = ncol(pf),
          dimnames = list(names(pf), names(pf)))
  }
  regularize_cov(Sigma, param)
}

#' Zero out covariance entries between parameters that share no linked
#' variable in `param_var_matrix`.
#'
#' `param_var_matrix` says which parameters are relevant to which response
#' variables. Two parameters that share no variable in common (e.g. an
#' oxygen-only parameter and a temperature-only parameter) have no declared
#' reason to be correlated; rather than relying on shrinkage alone to
#' discover that from what may be a small survivor sample, this forces
#' their covariance to exactly zero. Parameters that share at least one
#' linked variable - including parameters linked to multiple variables -
#' keep whatever covariance `estimate_shrunk_cov()` estimated for them.
#'
#' @param Sigma covariance matrix; column/row names must match
#' `param_var_matrix$name_full`.
#' @inheritParams calib_aeme
#' @param vars_sim character vector; the variable columns of
#' `param_var_matrix` to check membership against.
#' @return `Sigma` with disallowed off-diagonal entries set to zero.
#' @noRd
mask_unlinked_cov <- function(Sigma, param_var_matrix, vars_sim) {
  membership <- param_var_matrix[match(colnames(Sigma), param_var_matrix$name_full),
                                 vars_sim, drop = FALSE]
  membership <- as.matrix(membership) * 1
  shared_var <- (membership %*% t(membership)) > 0
  Sigma[!shared_var] <- 0
  Sigma
}

#' Linearly anneal a control value from a starting value towards a final
#' value over the course of a calibration run.
#'
#' Used to shift `ctrl$cutoff`/`ctrl$mutate` from broad/exploratory values in
#' early generations towards narrow/exploitative values in later ones. When
#' `end` is `NULL` (the default for `cutoff_final`/`mutate_final`), the
#' control value stays fixed at `start` for the whole run, matching the
#' behaviour before annealing was added.
#'
#' @param start numeric; value to anneal from (generation 1).
#' @param end numeric; value to anneal towards by the last generation. `NULL`
#' disables annealing (returns `start` unchanged).
#' @param gen_n numeric; the generation just completed.
#' @param tot_gen numeric; total number of generations planned for this run.
#' @return numeric; the annealed value for the *next* generation.
#' @noRd
anneal_param <- function(start, end, gen_n, tot_gen) {
  if (is.null(end) || tot_gen <= 1) {
    return(start)
  }
  frac <- min(gen_n / tot_gen, 1)
  start + frac * (end - start)
}

#' Print a summary of a generation's candidate parameter values
#' This function takes a data frame of candidate parameter values for a given
#' generation and prints a summary of the mean, median, and standard deviation
#' for each parameter. It also informs the user that the generation has started.
#' @param df A data frame containing candidate parameter values for the current
#' generation. Each column
#' represents a parameter, and each row represents a candidate solution.
#' @param gen_n The current generation number.
#' @param tot_gen The total number of generations planned for this run.
#' @param ctrl list; control object holding `NP`.
#' @noRd
announce_generation <- function(df, gen_n, tot_gen, ctrl) {
  cli::cli_inform(c(">" = "Starting generation {.val {gen_n}}/{.val
      {tot_gen}}, {.val {ctrl$NP}} members. [{format(Sys.time())}]"))
  pr_df <- data.frame(rbind(signif(apply(df, 2, mean), 4),
                            signif(apply(df, 2, median), 4),
                            signif(apply(df, 2, sd), 4)),
                      row.names = c("mean", "median", "sd"))
  names(pr_df) <- gsub("\\[NA\\]", "", gsub("NA/", "", names(df)))
  cli::cli_inform("Parameter summary for generation {.val {gen_n}}:")
  print(pr_df)
}

#' Report a generation's fitness once it has been evaluated
#' This function takes a data frame of evaluated candidate parameter values for
#'  a given generation and prints a summary of the best fit value, its standard
#'  deviation, and optionally the best parameter values in a formatted string.
#'  It also informs the user that the generation has completed.
#' @param g_eval A data frame containing evaluated candidate parameter values
#' for the current generation.
#' Each row represents a candidate solution, and the `fit` column contains the
#' fitness values.
#' @param gen_n The current generation number.
#' @param tot_gen The total number of generations planned for this run.
#' @param m The model being calibrated, e.g. "glm_aed".
#' @param ctrl list; control object holding `na_value`.
#' @param best_pars_fmt An optional string containing the best parameter values
#' in a formatted
#' manner. If provided, it will be included in the report.
#' @noRd
report_generation <- function(g_eval, gen_n, tot_gen, m, ctrl,
                              best_pars_fmt = NULL) {
  rep_vars <- g_eval |>
    dplyr::filter(!is_failed_fit(fit, ctrl))
  cli::cli_alert_success("Completed generation {.val {gen_n}}/{.val {tot_gen}}
                             for {.val {m}}. [{format(Sys.time())}]")
  if (is.null(best_pars_fmt)) {
    cli::cli_inform("Best fit: {.val {signif(min(rep_vars$fit), 3)}} (sd:
                        {.val {signif(sd(rep_vars$fit), 5)}})")
  } else {
    cli::cli_inform("Best fit: {.val {signif(min(rep_vars$fit), 3)}} (sd:
                        {.val {signif(sd(rep_vars$fit), 5)}})
              Parameters: [ {.val {best_pars_fmt}} ]")
  }
}
