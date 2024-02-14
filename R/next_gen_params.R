#' Generate next generation of parameters
#'
#' @param param_df dataframe; with parameters and fitness.
#' @param param dataframe; with parameter names, min and max.
#' @param ctrl list; with control parameters.
#' @param best_pars dataframe; with best parameters.
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats cov rnorm sd quantile
#'
#' @return dataframe; with new parameters.
#' @noRd

next_gen_params <- function(param_df, param, ctrl, best_pars) {

  if (is.null(best_pars)) {
    best_pars <- param_df[which.min(param_df$fit), ]
  }

  survivors1 <- param_df[param_df$fit != ctrl$na_value, ]
  if (nrow(survivors1) == 0) {
    survivors <- param_df[order(param_df$fit), ]
  }
  survivors1 <- survivors1[order(survivors1$fit), ]
  names(survivors1) <- gsub("NA/", "", names(survivors1))
  keep_cols <- which(names(survivors1) %in% param$name)
  if ((nrow(survivors1) / nrow(param_df)) > 0.3) {
    message("Survival rate: ", round(nrow(survivors1) / nrow(param_df), 2))
    survivors2 <- survivors1[survivors1$fit <= stats::quantile(survivors1$fit,
                                                               ctrl$cutoff),
                             keep_cols]
  } else {
    message("Survival rate: ", round(nrow(survivors1) / nrow(param_df), 2),
            " is too low, using all individuals.")
    survivors2 <- survivors1[, keep_cols]
  }
  if (is.null(nrow(survivors2))) {
    survivors2 <- data.frame(matrix(survivors2))
    names(survivors2) <- names(survivors1)[keep_cols]
  }
  if (nrow(survivors2) == 1) {
    message("Number of survivors is too low (n=", nrow(survivors1),
            ")... using 2 * ctrl$cutoff.")
    survivors2 <- survivors1[survivors1$fit <= stats::quantile(survivors1$fit,
                                                               (ctrl$cutoff * 2)),
                             keep_cols]
  }
  if (nrow(survivors2) == 0) {
    message("All parameter sets are NA. Generating base parameters...")
    survivors2 <- survivors1[survivors1$fit <= stats::quantile(survivors1$fit,
                                                               (ctrl$cutoff * 3)),
                             keep_cols]
    g <- FME::Latinhyper(param[, c("min", "max")],
                         ctrl$NP)
    colnames(g) <- param$name
    g <- as.data.frame(g)
  } else {
    g <- as.data.frame(MASS::mvrnorm(n = ctrl$NP,
                                     mu = apply(survivors2, 2, mean),
                                     Sigma = stats::cov(survivors2), tol = 1))
  }

  # Correct parameters outside ranges ----
  for (p in names(g)) {
    g[[p]][g[[p]] < param$min[param$name == p]] <- param$min[param$name == p]
    g[[p]][g[[p]] > param$max[param$name == p]] <- param$max[param$name == p]
  }
  # Add mutation ----
  n_mut <- round(ctrl$NP * ctrl$mutate)
  for (p in names(g)) {
    g[[p]][sample(ctrl$NP, n_mut)] <- runif(n_mut,
                                            min = param$min[param$name == p],
                                            max = param$max[param$name == p])
  }
  # Replace last parameter rather than adding
  g[nrow(g), ] <- best_pars[, keep_cols]
  names(g) <- names(param_df)[keep_cols]

  return(g)
}
