#' Generate next generation of parameters
#'
#' @param param_df dataframe; with parameters and fitness.
#' @param param dataframe; with parameter names, min and max.
#' @param ctrl list; with control parameters.
#' @param best_pars dataframe; with best parameters.
#'
#' @return dataframe; with new parameters.
#' @noRd

next_gen_params <- function(param_df, param, ctrl, best_pars) {

  if (is.null(best_pars)) {
    best_pars <- param_df[which.min(param_df$fit), ]
  }

  survivors <- param_df[param_df$fit != ctrl$na_value, ]
  if (nrow(survivors) == 0) {
    survivors <- param_df[order(param_df$fit), ]
  }
  survivors <- survivors[order(survivors$fit), ]
  drop_cols <- which(names(survivors) %in% c("fit", "gen"))
  if ((nrow(survivors) / nrow(param_df)) > 0.5) {
    message("Survival rate: ", round(nrow(survivors) / nrow(param_df), 2))
    survivors <- survivors[survivors$fit <= quantile(survivors$fit, ctrl$p), -drop_cols]
  } else {
    survivors <- survivors[, -drop_cols]
  }


  g <- as.data.frame(MASS::mvrnorm(n = ctrl$NP, mu = apply(survivors, 2, mean),
                                   Sigma = cov(survivors), tol = 1))

  # Improved targeting of outflow factor when no obs present ----
  if("outflow" %in% names(g) & !ctrl$use_obs & "MET_pprain" %in% names(g)) {
    message("No observations, using normal distribution for outflow.")
    b_par <- param_df[which.min(param_df$fit), ]
    g[["outflow"]] <- rnorm(n = ctrl$NP, mean = best_pars[["outflow"]],
                            sd = sd(g$outflow))
    g[["MET_pprain"]] <- rnorm(n = ctrl$NP, mean = best_pars[["MET_pprain"]],
                               sd = sd(g$MET_pprain))

  }

  # Correct parameters outside ranges ----
  for(p in names(g)) {
    g[[p]][g[[p]] < param$min[param$name == p]] <- param$min[param$name == p]
    g[[p]][g[[p]] > param$max[param$name == p]] <- param$max[param$name == p]
  }
  # Add mutation ----
  n_mut <- round(ctrl$NP * ctrl$mutate)
  for(p in names(g)) {
    g[[p]][sample(ctrl$NP, n_mut)] <- runif(n_mut,
                                            min = param$min[param$name == p],
                                            max = param$max[param$name == p])
  }
  g <- rbind(g, best_pars[, -drop_cols])

  return(g)
}
