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
  drop_cols <- which(names(survivors1) %in% c("fit", "gen"))
  if ((nrow(survivors1) / nrow(param_df)) > 0.3) {
    message("Survival rate: ", round(nrow(survivors1) / nrow(param_df), 2))
    survivors2 <- survivors1[survivors1$fit <= stats::quantile(survivors1$fit,
                                                               ctrl$cutoff),
                             -drop_cols]
  } else {
    message("Survival rate: ", round(nrow(survivors1) / nrow(param_df), 2),
            " is too low, using all individuals.")
    survivors2 <- survivors1[, -drop_cols]
  }
  if (nrow(survivors2) == 1) {
    message("Number of survivors is too low (n=", nrow(survivors1),
            ")... using 2 * ctrl$cutoff.")
    survivors2 <- survivors1[survivors1$fit <= stats::quantile(survivors1$fit,
                                                               (ctrl$cutoff * 2)),
                             -drop_cols]
  }


  g <- as.data.frame(MASS::mvrnorm(n = ctrl$NP, mu = apply(survivors2, 2, mean),
                                   Sigma = stats::cov(survivors2), tol = 1))

  # Improved targeting of outflow factor when no obs present ----
  if ("outflow" %in% names(g) & !ctrl$use_obs & "MET_pprain" %in% names(g)) {
    message("No observations, using normal distribution for outflow.")
    b_par <- param_df[which.min(param_df$fit), ]
    g[["outflow"]] <- stats::rnorm(n = ctrl$NP, mean = best_pars[["outflow"]],
                                   sd = stats::sd(g$outflow))
    g[["MET_pprain"]] <- stats::rnorm(n = ctrl$NP,
                                      mean = best_pars[["MET_pprain"]],
                                      sd = stats::sd(g$MET_pprain))

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
  g <- rbind(g, best_pars[, -drop_cols])

  return(g)
}
