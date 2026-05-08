#' Get parameter values from calibration results
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' `get_param()` is deprecated. Please use either \link{get_all_params()} or
#' \link{get_best_params()} instead, depending on whether you want to retrieve 
#' all parameter values or just the best parameter values based on a specified 
#' fit column and quantile threshold.
#' 
#' @param calib A list with the calibration results loaded using
#' \code{\link{read_calib}}.
#' @param best A logical value indicating whether to return the best parameter
#' values or all parameter values.
#' @param na_value `r lifecycle::badge("deprecated")` Numeric. Penalty value 
#' substituted for \code{NA} fit values, this is no longer needed as NA values 
#' are now written to simulation_data in output of calib_aeme() and sa_aeme(). 
#' The argument will be removed in a future version.
#' @inheritParams plot_calib
#' @inheritParams update_param
#'
#' @importFrom dplyr case_when filter group_by mutate summarise
#' @importFrom stringr str_split_i
#' @importFrom tidyr pivot_wider
#' @importFrom lifecycle deprecate_soft
#'
#' @return A data frame with the parameter values.
#' @export

get_param <- function(calib, na_value, fit_col = "fit", best = FALSE, 
                      quantile = 0.1) {
  
  lifecycle::deprecate_soft("0.2.0", "get_param()", 
                            details = "Use either `get_all_params()` or
                            `get_best_params()` instead.)`")

  if (missing(na_value)) {
    na_value <- calib$calibration_metadata$na_value[1]
  }
  
  
  if (best) {
    param_df <- get_best_params(calib = calib, na_value = na_value,
                                fit_col = fit_col, 
                                quantile_threshold = quantile)
  } else {
    param_df <- get_all_params(calib = calib, na_value = na_value, 
                               fit_col = fit_col)
  }
  return(param_df)
}



#' Abbreviate parameters
#'
#' @param par character; with parameter names.
#' @param model character; with model name.
#' @return vector; with abbreviated parameter names.
#' @noRd
abbrev_pars <- function(par, model) {
  par1 <- gsub("NA.", "", par)
  if (all(model == "dy_cd")) {
    dy_abbrev <- function(string) {
      # Split the string into words
      words <- strsplit(string, "_")[[1]]
      if (length(words) > 1) {
        # Extract the first letter of each word
        initials <- abbreviate(words, 3)
        
        # Concatenate the initials to form the abbreviation
        abbreviation <- paste(initials, collapse = "_")
        
        return(abbreviation)
      } else {
        return(string)
      }
    }
    par1 <- sub("\\[NA\\]", "", sub(".*/([^/]+)$", "\\1", par))
    par2 <- sub("\\/.*", "", par1)
    par2 <- sapply(par2, \(x) {
      if (!grepl("MET_", x)) {
        dy_abbrev(x)
      } else {
        x
      }
    })
  } else if (all(model == "glm_aed")) {
    par1 <- sub("^NA/", "", par)
    par2 <- sub("\\[NA\\]", "", par1)
    # par2 <- sub("\\[NA\\]", "", sub(".*/([^/]+)$", "\\1", par))
  } else if (all(model == "gotm_wet")) {
    par2 <- sub("\\[NA\\]", "", sub(".*/([^/]+)$", "\\1", par))
    if ("constant_value" %in% par2) {
      par2[par2 == "constant_value"] <- sub(".*/([^/]+)/.*", "\\1",par1[par2 == "constant_value"])
    }
  } else {
    par2 <- sub("\\[NA\\]", "", sub(".*/([^/]+)$", "\\1", par))
  }
  if (any(grepl("MET_", par2))) {
    par2 <- sub("MET_", "", par2)
  }
  return(par2)
  
  # names(params) <- params1
  # par_ref <- data.frame(parameter = params1, label = params)
}

