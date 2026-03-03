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

