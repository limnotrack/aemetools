#' Generate next generation of parameters
#'
#' @param param_df dataframe; with parameters and fitness.
#' @param param dataframe; with parameter names, min and max.
#' @param ctrl list; with control parameters.
#' @param best_pars dataframe; with best parameters. Default is NULL.
#' @param add_mutation logical; add mutation to parameters. Default is TRUE.
#' @param keep_best_pars logical; keep best parameters. Default is TRUE.
#' @inheritParams calib_aeme
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats cov rnorm sd quantile
#' @importFrom FME Latinhyper
#' @importFrom cli cli_alert_info
#' @importFrom corpcor cov.shrink
#'
#' @return dataframe; with new parameters.
#' @noRd

next_gen_params <- function(param_df, param, ctrl, best_pars = NULL,
                            add_mutation = TRUE, keep_best_pars = TRUE,
                            param_var_matrix = NULL, weights) {

  if (is.null(best_pars)) {
    best_pars <- param_df[which.min(param_df$fit), ]
  }
  
  survivors1 <- param_df[!is_failed_fit(param_df$fit, ctrl), ]
  survivors1 <- survivors1[order(survivors1$fit), ]
  keep_cols <- which(names(survivors1) %in% param$name_full)
  if (!is.null(param_var_matrix)) {
    vars_sim <- names(param_var_matrix)[!names(param_var_matrix) %in% c("model", "file", "name_full", "group", "name", "index") ]

    all_params <- unique(param_var_matrix$name_full)

    # Dominance for the Pareto front is always computed over the full
    # vars_sim objective set, so every variable's "block" would select the
    # same elite rows - there is no statistical reason to estimate a
    # separate covariance matrix per variable. Instead, sample all linked
    # parameters jointly from one shrinkage-estimated covariance matrix
    # (corpcor::cov.shrink()): this captures correlation between parameters
    # that are genuinely linked to more than one variable directly from the
    # data, while shrinkage keeps the estimate well-conditioned even when
    # there are more parameters than Pareto-front survivors. Parameter
    # pairs that share *no* linked variable at all (e.g. an oxygen-only
    # parameter and a temperature-only parameter) are then masked back to
    # zero covariance via param_var_matrix - that's a declared modelling
    # constraint, not something to leave to shrinkage to (maybe) discover
    # from a small sample. `weights` no longer factors in here - true
    # multi-objective (Pareto-front) selection doesn't need per-variable
    # weights to combine objectives; `weights` still shapes the combined
    # `fit` column used earlier to decide which individuals survive.
    pf <- get_pareto_front(survivors1, vars_sim) |>
      dplyr::select(dplyr::all_of(all_params))

    Sigma <- estimate_shrunk_cov(pf, param)
    Sigma <- mask_unlinked_cov(Sigma, param_var_matrix, vars_sim)

    survivors2 <- as.data.frame(
      MASS::mvrnorm(n = ctrl$NP, mu = apply(pf, 2, mean), Sigma = Sigma,
                    tol = 1)
    )
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
    
    # best_pars_long <- best_pars |> 
    #   dplyr::select(dplyr::all_of(param$name_full)) |>
    #   tidyr::pivot_longer(cols = dplyr::all_of(param$name_full), names_to = "param")
    # 
    # summ_survivors <- survivors2 |> 
    #   dplyr::select(dplyr::all_of(param$name_full)) |>
    #   tidyr::pivot_longer(cols = dplyr::all_of(param$name_full), names_to = "param") |> 
    #   dplyr::group_by(param) |>
    #   dplyr::summarise(n = sum(!is.na(value)),
    #                    min = min(value, na.rm = TRUE),
    #                    # min = quantile(value, 0.1, na.rm = TRUE),
    #                    max = max(value, na.rm = TRUE),
    #                    # max = quantile(value, 0.9, na.rm = TRUE),
    #                    .groups = "drop") |>
    #   dplyr::left_join(best_pars_long, by = "param") |>
    #   dplyr::mutate(
    #     min = pmin(min, value),
    #     max = pmax(max, value)
    #   ) 
    # 
    # g <- FME::Latinhyper(summ_survivors[, c("min", "max")],
    #                      ctrl$NP)
    # colnames(g) <- summ_survivors$param
    g <- as.data.frame(survivors2)
  } else {
    Sigma <- estimate_shrunk_cov(survivors2, param)
    g <- as.data.frame(MASS::mvrnorm(n = ctrl$NP,
                                     mu = apply(survivors2, 2, mean),
                                     Sigma = Sigma, tol = 1))
  }

  # Correct parameters outside ranges ----
  # Reflect (rather than clamp) out-of-range draws so candidate values don't
  # pile up exactly on a boundary across generations.
  for (p in names(g)) {
    g[[p]] <- reflect_bounds(g[[p]], param$min[param$name_full == p],
                             param$max[param$name_full == p])
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
  
  # Correct indexed parameters ----
  g <- adj_index_params(g, param)
  
  # Replace last parameter rather than adding
  if (keep_best_pars) {
    best_pars <- best_pars |> 
      dplyr::select(dplyr::all_of(names(g)))
    
    g <- g |> 
      dplyr::slice_tail(n = -nrow(best_pars)) |>
      dplyr::bind_rows(best_pars)
  }

  return(g)
}
