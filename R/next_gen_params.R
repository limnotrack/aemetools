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
    vars_sim <- names(param_var_matrix)[!names(param_var_matrix) %in% c("model", "file", "name_full", "group", "name", "index") ]
    # pareto_front <- get_pareto_front(survivors1, vars_sim)
    # print(nrow(survivors2))
    block_pops <- lapply(vars_sim, \(v) {
      
      sel_param <- param_var_matrix[["name_full"]][param_var_matrix[[v]]]
      sel_cols  <- c(sel_param, vars_sim)
      
      sel_survivors <- survivors1 |>
        dplyr::select(dplyr::all_of(sel_cols))
      
      pf <- get_pareto_front(sel_survivors, vars_sim) |>
        dplyr::select(dplyr::all_of(sel_param))
      
      as.data.frame(
        MASS::mvrnorm(
          n     = ctrl$NP,
          mu    = apply(pf, 2, mean),
          Sigma = stats::cov(pf),
          tol   = 1
        )
      )
    })
    
    names(block_pops) <- vars_sim
    
    all_params <- unique(param_var_matrix$name_full)
    
    g <- as.data.frame(
      matrix(NA_real_, nrow = ctrl$NP, ncol = length(all_params))
    )
    
    colnames(g) <- all_params
    
    for (p in all_params) {
      
      # Which blocks contain this parameter?
      blocks_with_p <- vars_sim[
        sapply(vars_sim, function(v) {
          p %in% param_var_matrix$name_full[param_var_matrix[[v]]]
        })
      ]
      
      if (length(blocks_with_p) == 1) {
        
        # Only one block → take full column
        b <- blocks_with_p
        g[[p]] <- block_pops[[b]][[p]]
        
      } else if (length(blocks_with_p) >= 2) {
        
        # --- get weights for relevant blocks ---
        w <- weights[blocks_with_p]
        
        # if some blocks missing weights → assume 1
        w[is.na(w)] <- 1
        
        # normalise
        w <- w / sum(w)
        
        # --- number of rows per block ---
        n_per_block <- floor(w * ctrl$NP)
        
        # ensure total exactly equals NP
        remainder <- ctrl$NP - sum(n_per_block)
        if (remainder > 0) {
          # distribute remainder to largest weights
          ord <- order(w, decreasing = TRUE)
          n_per_block[ord[seq_len(remainder)]] <-
            n_per_block[ord[seq_len(remainder)]] + 1
        }
        
        # --- random row assignment ---
        idx <- sample(seq_len(ctrl$NP))
        
        start <- 1
        for (i in seq_along(blocks_with_p)) {
          
          end <- start + n_per_block[i] - 1
          rows_i <- idx[start:end]
          
          block_name <- blocks_with_p[i]
          
          g[[p]][rows_i] <-
            block_pops[[block_name]][[p]][rows_i]
          
          start <- end + 1
        }
      }
    }
    
    survivors2 <- g
    
    
    # sel_survivors[sel_survivors[[v]] <= stats::quantile(sel_survivors[[v]],
    #                                                     ctrl$cutoff), ]# |>
    # dplyr::select(-dplyr::all_of(v))
    
    # pf2 <- get_pareto_front(survivors2, vars_sim)
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
    best_pars <- best_pars |> 
      dplyr::select(dplyr::all_of(names(g)))
    
    g <- g |> 
      dplyr::slice_tail(n = -nrow(best_pars)) |>
      dplyr::bind_rows(best_pars)
  }

  return(g)
}
