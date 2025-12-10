#' Run AEME with parameter dataframe
#'
#' @inheritParams AEME::run_aeme
#' @inheritParams run_and_fit
#' @param na_value numeric; value to return if model run is unsuccessful
#' @param return_nc boolean; return netCDF file connection
#' @param return_aeme boolean; return AEME object
#'
#' @importFrom AEME run_aeme lake input observations outflows
#' @importFrom AEME read_nml write_nml set_nml
#' @importFrom AEME write_yaml
#' @importFrom yaml read_yaml
#' @importFrom ncdf4 nc_open nc_close
#'
#' @return `na_value` if model run is unsuccessful
#' @export

run_aeme_param <- function(aeme, param, model, path = ".",
                           model_controls = NULL, na_value = 999, 
                           return_nc = FALSE, return_aeme = FALSE, 
                           parallel = FALSE, timeout = Inf) {
  
  # Function checks ----
  if (!is.data.frame(param))
    cli::cli_abort("{.arg param} must be a data.frame.")
  if (!is.character(model))
    cli::cli_abort("{.arg model} must be a character string.")
  if (return_nc & return_aeme)
    cli::cli_abort("Only one of 'return_nc' and 'return_aeme' can be TRUE.")
  if (return_nc & length(model) > 1)
    cli::cli_abort("Only one model can be run when 'return_nc' is TRUE.")
  
  if (is.null(model_controls)) {
    config <- AEME::configuration(aeme = aeme)
    model_controls <- config$model_controls
  }
  
  # Load AEME data
  lake_dir <- AEME::get_lake_dir(aeme = aeme, path = path)
  inp <- AEME::input(aeme)
  obs <- AEME::observations(aeme)
  obs$lake$depth_mid <- (obs$lake$depth_to - obs$lake$depth_from) / 2
  
  # Update parameter values ----
  AEME::input_model_parameters(aeme = aeme, model = model, param = param,
                               path = path)
  
  # Run model ----
  mod_out <- tryCatch({
    AEME::run_aeme(aeme = aeme, model = model, path = path,
                   check_output = FALSE, parallel = parallel,
                   model_controls = model_controls, 
                   return_type = "both", timeout = timeout)
  }, error = function(e) {
    cli::cli_alert_danger("Error running AEME: {e$message}. Probably due to a 
                          timeout.")
    return(NULL)
  })
  if (is.null(mod_out)) {
    return(na_value)
  }
  aeme <- mod_out$aeme
  
  # Check for timeout ----
  for (m in model) {
    if (mod_out$exec_result[[m]]$timeout) {
      cli::cli_alert_danger("Model {.strong {m}} run timed out.")
      return(na_value)
    }
  }
  
  
  # Check if model output is produced ----
  out_file <- AEME::get_model_outfile(lake_dir = lake_dir, model = model)
  
  out_file_chk <- sapply(out_file, \(x) !file.exists(x)) |> 
    unlist()
  if (any(out_file_chk) | length(out_file_chk) == 0) {
    out_file_unl <- unlist(out_file)
    cli::cli_alert_danger("No {.file {out_file_unl[out_file_chk]}} present.")
    return(na_value)
  }
  
  if (return_nc) {
    if (model == "gotm_wet") {
      file <- out_file[[model]][["output"]]
    } else {
      file <- out_file[[model]]
    }
    nc <- AEME::open_nc_safe(file = file, model = model)
    return(nc)
  }
  
  if (return_aeme) {
    return(aeme)
  }
}
