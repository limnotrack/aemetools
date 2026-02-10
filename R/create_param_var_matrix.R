#' Create a parameter-variable matrix for each model
#'
#' @inheritParams calib_aeme
#'
#' @returns A list of data frames, one for each model, with parameters as rows 
#' and variables as columns. The values in the data frames indicate whether a 
#' parameter is associated with a variable (TRUE) or not (FALSE).
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
  out_list <- lapply(model, \(m) {
    param_m <- param |> 
      dplyr::filter(model == m) 
    param_full <- encode_param(group = param_m$group, name = param_m$name, 
                               index = param_m$index) 
    
    # param_vars_matrix <- matrix(1, nrow = length(param_full), 
    #                             ncol = length(vars_sim))
    param_vars_matrix <- matrix(TRUE, nrow = length(param_full),
                                ncol = length(vars_sim)) |> 
      as.data.frame()
    
    rownames(param_vars_matrix) <- param_full
    colnames(param_vars_matrix) <- vars_sim
    param_vars_matrix
  }) 
  names(out_list) <- model
  return(out_list)
}
