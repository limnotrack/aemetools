#' Create a parameter-variable matrix for each model
#'
#' @inheritParams calib_aeme
#'
#' @returns A data frame with columns for model, file, name_full, and one 
#' column for each variable in vars_sim. The variable columns contain TRUE/FALSE
#' values indicating whether the parameter is associated with the variable.
#' @export
#' 
#' @importFrom dplyr filter pull
#' @importFrom AEME check_model
#'
#' @examples
#' param <- aeme_parameters
#' vars_sim <- c("HYD_temp", "CHM_oxy", "PHY_tchla")
#' param_var_matrix <- create_param_var_matrix(param, vars_sim)


create_param_var_matrix <- function(param, vars_sim) {
  
  model <- unique(param$model)
  model <- AEME::check_model(model)
  
  
  out <- lapply(model, \(m) {
    
    param_m <- param |>
      dplyr::filter(model == m)
    
    name_full <- encode_param(
      group = param_m$group,
      name  = param_m$name,
      index = param_m$index
    )
    
    param_vars_matrix <- matrix(
      FALSE,
      nrow = length(name_full),
      ncol = length(vars_sim),
      dimnames = list(NULL, vars_sim)
    ) |>
      as.data.frame() |> 
      dplyr::mutate(
        model = m,
        file = param_m$file,
        name_full = name_full,
        group = param_m$group,
        name = param_m$name,
        index = param_m$index,
      ) |> 
      dplyr::select(model, file, name_full, group, name, index,
                    dplyr::everything())
    
    # populate from var_sim column if present
    if ("var_sim" %in% colnames(param_m)) {
      
      var_list <- strsplit(
        param_m$var_sim,
        "|",
        fixed = TRUE
      )
      
      for (i in seq_along(name_full)) {
        vars_i <- var_list[[i]]
        
        if (!any(is.na(vars_i))) {
          vars_i <- intersect(vars_i, vars_sim)
          param_vars_matrix[i, vars_i] <- TRUE
        }
      }
      
    } else {
      # fallback: everything TRUE if no mapping provided
      param_vars_matrix[,] <- TRUE
    }
    
    param_vars_matrix
  }) |> 
    dplyr::bind_rows() |> 
    dplyr::select(model, file, name_full, group, name, index, 
                  dplyr::everything()) |> 
    dplyr::arrange(model, file, group, name, index) |> 
    dplyr::select(-group, -name, -index)
  
  return(out)
}
