#' Adjust parameter values for parameters with index to ensure correct ordering in calibration and sensitivity analysis.
#'
#' @param .data data frame containing parameter sets where columns represent 
#' parameter names and rows represent different parameter sets. The column names
#'  should match the 'name_full' format used in the 'param' data frame.
#' @inheritParams run_and_fit 
#'
#' @returns A data frame with the same structure as the input, but with 
#' parameter values adjusted for parameters with indices to ensure correct 
#' ordering in calibration and sensitivity analysis. The adjustment is based on 
#' the 'index' column in the 'param' data frame, which specifies the order of 
#' parameters with indices. Parameters with the same name but different indices 
#' will be sorted according to their index values, and their values will be 
#' adjusted accordingly.
#' @noRd
#' 
#' @importFrom dplyr filter
#' 
#'

adj_index_params <- function(.data, param) {
  index_param <- param |> 
    dplyr::filter(!is.na(index))
  if (nrow(index_param) > 0) {
    # Determine ordering direction from default values
    order_rules <- lapply(
      split(index_param, index_param$name),
      function(df) {
        
        df <- df[order(df$index), ]
        
        if (nrow(df) < 2) return(NULL)
        
        increasing <- df$value[1] <= df$value[2]
        
        list(
          cols = df$name_full,
          increasing = increasing
        )
      }
    )
    
    # Remove NULL entries
    order_rules <- order_rules[!vapply(order_rules, is.null, logical(1))]
    
    for (rule in order_rules) {
      
      cols <- rule$cols
      inc  <- rule$increasing
      
      # Skip if columns not present in .data
      if (!all(cols %in% names(.data))) next
      
      vals <- as.matrix(.data[, cols, drop = FALSE])
      
      if (inc) {
        vals <- t(apply(vals, 1, sort))
      } else {
        vals <- t(apply(vals, 1, function(x) sort(x, decreasing = TRUE)))
      }
      
      
      .data[, cols] <- vals
    }
  }
  return(.data)
}
