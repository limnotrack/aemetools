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
                           model_controls = NULL,
                           na_value = 999, return_nc = FALSE,
                           return_aeme = FALSE, parallel = FALSE) {

  # Function checks ----
  if (!is.data.frame(param))
    stop("Parameter 'param' must be a data.frame.")
  if (!is.character(model))
    stop("Parameter 'model' must be a character string.")
  if (return_nc & return_aeme)
    stop("Only one of 'return_nc' and 'return_aeme' can be TRUE.")
  # if (length(model) != 1)
  #   stop("Only one model can be run at a time.")
  if (is.null(model_controls)) {
    config <- AEME::configuration(aeme = aeme)
    model_controls <- config$model_controls
  }

  # Load AEME data
  lke <- AEME::lake(aeme)
  lakename <- tolower(lke[["name"]])
  lake_dir <- file.path(path, paste0(lke$id, "_", lakename))
  inp <- AEME::input(aeme)
  obs <- AEME::observations(aeme)
  obs$lake$depth_mid <- (obs$lake$depth_to - obs$lake$depth_from) / 2

  # Update parameter values ----
  AEME::input_model_parameters(aeme = aeme, model = model, param = param,
                               path = path)

  # Run model ----
  aeme <- AEME::run_aeme(aeme = aeme, model = model, path = path,
                         check_output = FALSE, parallel = parallel,
                         model_controls = model_controls, return = return_aeme)


  # Check if model output is produced ----
  out_file <- dplyr::case_when(model == "dy_cd" ~
                                 file.path(lake_dir,
                                           model, "DYsim.nc"),
                               model == "glm_aed" ~
                                 file.path(lake_dir, model,
                                           "output", "output.nc"),
                               model == "gotm_pclake" ~
                                 file.path(lake_dir, model,
                                           "output", "output.nc"),
                               model == "gotm_wet" ~
                                 file.path(lake_dir, model,
                                           "output", "output.nc")
  )

  out_file_chk <-  !file.exists(out_file)
  if (any(out_file_chk)) {
    message("No ", out_file[out_file_chk], " present.")
    return(na_value)
  }

  if (return_nc) {
    nc <- ncdf4::nc_open(out_file, return_on_error = TRUE)
    return(nc)
  }

  if (return_aeme) {
    return(aeme)
  }
}
