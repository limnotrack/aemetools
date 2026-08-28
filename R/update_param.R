#' Update parameter values in param based on best_pars
#'
#' @inheritParams AEME::build_aeme
#' @inheritParams plot_calib
#' @inheritParams get_param
#' @param param A data frame with parameters to update. Defaults to NULL. When
#' NULL, the parameter values are extracted from `calib$parameter_metadata`.
#' @param best_pars A data frame with the best parameters from \code{\link{get_param}}.
#' Defaults to NULL. When NULL, \code{\link{get_param}} is called to get the 
#' best parameters.
#' @param aeme aeme; object. Defaults to NULL. When NULL, a dataframe of the
#' updated parameter values is returned. When provided, the updated parameter
#' values are added to the aeme object and the aeme object is returned.
#' @param replace Logical. If TRUE, the parameter values in the aeme object are
#' replaced with the updated values. Defaults to FALSE. Only used when aeme is
#' provided.
#' @param quantile `r lifecycle::badge("deprecated")` No longer used, replaced 
#' by `quantile_threshold`.
#' @param quantile_threshold The quantile to use for 
#' the top quantile of the fit_value. Defaults to 0.1. This is used to determine
#' min, max, for parameters when best_pars is not provided. 
#' @param na_value `r lifecycle::badge("deprecated")` Numeric. Penalty value 
#' substituted for \code{NA} fit values, this is no longer needed as NA values 
#' are now written to simulation_data in output of calib_aeme() and sa_aeme(). 
#' The argument will be removed in a future version.
#'
#' @importFrom dplyr filter group_by select summarise all_of anti_join arrange
#' @importFrom dplyr bind_rows left_join rows_upsert semi_join
#' @importFrom lifecycle deprecate_warn
#'
#' @return data frame with updated parameter values for running the model with
#'  \code{\link{run_aeme_param}}
#' @export

update_param <- function(calib, param, aeme, replace = FALSE,
                         fit_col = "fit", best_pars, quantile_threshold = 0.1, 
                         na_value = NULL, quantile) {
  
  param_column_names <- AEME::param_colnames(incl_opt = FALSE)
  
  if (!missing(quantile)) {
    # Warn it is deprecated but assign to quantile_threshold for backward compatibility
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "update_param(quantile)",
      details = "The 'quantile' argument is deprecated and will be removed in a future
      version. Please use 'quantile_threshold' instead."
    )
    quantile_threshold <- quantile
  }
  
  if (missing(param)) {
    param <- calib$parameter_metadata |>
      dplyr::select(all_of(param_column_names)) |> 
      dplyr::mutate(
        name_full = encode_param(group, name, index)
      )
  }
  # A caller-supplied `param` is otherwise used as-is, but every join below
  # keys on `name_full`, which only the branch above adds. Derive it when
  # absent so that the documented usage - passing your own parameter
  # dataframe, e.g. `AEME::aeme_parameters` - works rather than failing in
  # dplyr with "Join columns in `y` must be present in the data".
  if (!"name_full" %in% names(param)) {
    param <- param |>
      dplyr::mutate(name_full = encode_param(group, name, index))
  }

  na_value <- resolve_na_value(na_value = na_value, calib = calib)
  
  if (missing(best_pars)) {
    best_pars <- get_best_params(calib = calib, fit_col = fit_col) |> 
      dplyr::mutate(
        name_full = encode_param(group, name, index)
      )
  }
  pars <- get_sim_params(calib = calib, fit_col = fit_col,
                         quantile_threshold = quantile_threshold)
  
  min_max <- pars |> 
    dplyr::group_by(parameter_name) |> 
    dplyr::summarise(
      min = min(parameter_value, na.rm = TRUE),
      max = max(parameter_value, na.rm = TRUE),
      .groups = "drop"
    ) |> 
    dplyr::rename(
      name_full = parameter_name
    ) 
  
  key_cols <- c("model", "name_full")
  
  # best_pars rows with no matching row in param yet
  new_pars     <- dplyr::anti_join(best_pars, param, by = key_cols)
  matched_pars <- dplyr::semi_join(best_pars, param, by = key_cols)
  
  if (nrow(new_pars) > 0) {
    n_new <- nrow(new_pars)
    noun <- if (n_new == 1) "Parameter" else "Parameters"
    pronoun <- if (n_new == 1) "it" else "them"
    AEME::cli_safe(
      paste0(noun, " not found in {.arg param}, adding ", pronoun, ": ",
            "{.val ", paste(new_pars$name_full, collapse = ", "), "}"),
      FUN = cli::cli_alert_warning
    )
    # New params have no established bounds in `param`, so fall back to the
    # observed range across the sampled simulations
    new_pars <- new_pars |> 
      dplyr::select(-min, -max) |> 
      dplyr::left_join(min_max, by = "name_full")
  }
  
  best_pars_final <- dplyr::bind_rows(matched_pars, new_pars) |> 
    dplyr::select(dplyr::all_of(c(param_column_names, "name_full")))
  
  key_cols <- c("model", "name_full")
  
  if (dplyr::n_distinct(param$model, param$name_full) != nrow(param)) {
    dup_names <- param |> 
      dplyr::count(model, name_full, name = "n") |> 
      dplyr::filter(n > 1) |> 
      dplyr::pull(name_full)
    
    cli::cli_abort(
      "{cli::qty(length(dup_names))} Parameter{?s} found in multiple places in {.arg param}: {.val {dup_names}}"
    )
  }
  
  param <- dplyr::rows_upsert(param, best_pars_final, by = key_cols)

  
  # for (i in seq_len(nrow(best_pars))) {
  #   idx <- best_pars$name_full[i] == param$name_full &
  #     best_pars$model[i] == param$model
  # 
  #   if (sum(idx) == 0) {
  #     cli::cli_alert_warning(
  #       "Parameter {.val {best_pars$name_full[i]}} not found in param, adding it."
  #     )
  #     # Add to param
  #     mm_row <- min_max |> 
  #       dplyr::filter(name_full == best_pars$name_full[i])
  #     new_row <- best_pars[i, ] |> 
  #       dplyr::select(-min, -max) |> 
  #       dplyr::left_join(mm_row, by = "name_full") |>
  #       dplyr::select(dplyr::all_of(param_column_names))
  #     param <- dplyr::bind_rows(param, new_row)
  #     next
  #     
  #   }
  #   
  #   if (sum(idx) > 1) {
  #     cli::cli_abort(
  #       "Parameter {.val {best_pars$name_full[i]}} found in multiple places for
  #       {.val {best_pars$model[i]}}"
  #     )
  #   }
  #   param[idx, "value"] <- best_pars$value[i]
  #   param[idx, "min"] <- best_pars$min[i]
  #   param[idx, "max"] <- best_pars$max[i]
  # }
  # param$value %in% best_pars$value
  param <- param |>
    dplyr::select(dplyr::all_of(param_column_names))
  
  if (missing(aeme)) {
    return(param)
  } else {
    if (replace) {
      AEME::parameters(aeme) <- param
    } else {
      old_pars <- AEME::parameters(aeme)
      if (nrow(old_pars) > 0) {
        par_diff <- dplyr::anti_join(old_pars, param, by = c("model", "file",
                                                             "name", "group",
                                                             "index"))
        param <- dplyr::bind_rows(par_diff, param) |>
          dplyr::arrange(model, group, name)
      }
      AEME::parameters(aeme) <- param
    }
    return(aeme)
  }
}
