#' Generate next generation of parameters
#'
#' @param param_df dataframe; with parameters and fitness.
#' @param param dataframe; with parameter names, min and max.
#' @param ctrl list; with control parameters.
#' @param best_pars dataframe; with best parameters. Default is NULL.
#' @param add_mutation logical; add mutation to parameters. Default is TRUE.
#' @param keep_best_pars logical; keep best parameters. Default is TRUE.
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats cov rnorm sd quantile
#' @importFrom FME Latinhyper
#' @importFrom cli cli_alert_info
#'
#' @return dataframe; with new parameters.
#' @noRd

next_gen_params <- function(param_df, param, ctrl, best_pars = NULL,
                            add_mutation = TRUE, keep_best_pars = TRUE,
                            param_var_matrix = NULL) {

  if (is.null(best_pars)) {
    best_pars <- param_df[which.min(param_df$fit), ]
  }
  
  survivors1 <- param_df[param_df$fit != ctrl$na_value, ]
  if (nrow(survivors1) == 0) {
    survivors <- param_df[order(param_df$fit), ]
  }
  survivors1 <- survivors1[order(survivors1$fit), ]
  keep_cols <- which(names(survivors1) %in% param$name_full)
  if (!is.null(param_var_matrix)) {
    vars_sim <- names(param_var_matrix)
    survivors2 <- lapply(vars_sim, \(v) {
      sel_param <- rownames(param_var_matrix)[param_var_matrix[[v]]]
      sel_cols <- c(sel_param, v)
      sel_survivors <- survivors1 |> 
        dplyr::select(dplyr::all_of(sel_cols))
      sel_survivors[sel_survivors[[v]] <= stats::quantile(sel_survivors[[v]],
                                                          ctrl$cutoff), ] |> 
        dplyr::select(-dplyr::all_of(v))
    }) |> 
      dplyr::bind_rows() |> 
      dplyr::select(dplyr::all_of(param$name_full))
  } else  if ((nrow(survivors1) / nrow(param_df)) > 0.3) {
    cli::cli_alert_info(
      "Survival rate: {round(nrow(survivors1) / nrow(param_df), 2)}"
      )
    survivors2 <- survivors1[survivors1$fit <= stats::quantile(survivors1$fit,
                                                               ctrl$cutoff),
                             keep_cols]
  } else {
    cli::cli_alert_info(
      "Survival rate: {round(nrow(survivors1) / nrow(param_df), 2)} is too low.
      Using all individuals."
      )
    survivors2 <- survivors1[, keep_cols]
  }
  
  
  if (is.null(nrow(survivors2))) {
    survivors2 <- data.frame(matrix(survivors2))
    names(survivors2) <- names(survivors1)[keep_cols]
  }
  # if (nrow(survivors2) == 1) {
  #   message("Number of survivors is too low (n=", nrow(survivors1),
  #           ")... using 2 * ctrl$cutoff.")
  #   survivors2 <- survivors1[survivors1$fit <= stats::quantile(survivors1$fit,
  #                                                              (ctrl$cutoff * 2)),
  #                            keep_cols]
  # }
  if (nrow(survivors2) <= 1) {
    cli::cli_alert_info("All parameter sets are NA.
                        Generating base parameters...")
    qt <- ctrl$cutoff * 3
    qt <- ifelse(qt > 1, 1, qt)
    survivors2 <- survivors1[survivors1$fit <= stats::quantile(survivors1$fit,
                                                               qt),
                             keep_cols]
    g <- FME::Latinhyper(param[, c("min", "max")],
                         ctrl$NP)
    colnames(g) <- param$name_full
    g <- as.data.frame(g)
  } else if (!is.null(param_var_matrix)) {
    
    summ_survivors <- survivors2 |> 
      tidyr::pivot_longer(cols = dplyr::all_of(param$name_full), names_to = "param") |> 
      dplyr::group_by(param) |>
      dplyr::summarise(n = sum(!is.na(value)),
                       min = min(value, na.rm = TRUE),
                       max = max(value, na.rm = TRUE),
                       .groups = "drop") 
    
    g <- FME::Latinhyper(summ_survivors[, c("min", "max")],
                         ctrl$NP)
    colnames(g) <- summ_survivors$param
    g <- as.data.frame(g)
  } else {
    g <- as.data.frame(MASS::mvrnorm(n = ctrl$NP,
                                     mu = apply(survivors2, 2, mean),
                                     Sigma = stats::cov(survivors2), tol = 1))
  }

  # Correct parameters outside ranges ----
  for (p in names(g)) {
    g[[p]][g[[p]] < param$min[param$name_full == p]] <- param$min[param$name_full == p]
    g[[p]][g[[p]] > param$max[param$name_full == p]] <- param$max[param$name_full == p]
  }
  # Add mutation ----
  if (add_mutation) {
    n_mut <- round(ctrl$NP * ctrl$mutate)
    for (p in names(g)) {
      g[[p]][sample(ctrl$NP, n_mut)] <- runif(n_mut,
                                              min = param$min[param$name_full == p],
                                              max = param$max[param$name_full == p])
    }
  }
  # Replace last parameter rather than adding
  if (keep_best_pars) {
    g <- g |> 
      dplyr::bind_rows(best_pars[, keep_cols])
  }

  return(g)
}
