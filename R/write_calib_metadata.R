#' Write calibration output to file
#'
#' @inheritParams utils::write.csv
#' @inheritParams DBI::dbWriteTable
#'
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom duckdb duckdb
#'
#' @return \code{write_calib_output} writes the calibration output to a file
#' @noRd
#'

write_calib_metadata <- function(ctrl, nsim) {

  # Check output type
  type <- ctrl$file_type

  # Table names
  tbl_names <- c("calibration_metadata")

  file_to_check <- ifelse(type == "db", ctrl$file_name,
                          "calibration_metadata.csv")

  calibration_metadata <- data.frame(sim_id = ctrl$sim_id, n_sim = nsim,
                                     VTR = ctrl$VTR, NP = ctrl$NP,
                                     itermax = ctrl$itermax,
                                     reltol = ctrl$reltol, cutoff = ctrl$cutoff,
                                     mutate = ctrl$mutate,
                                     na_value = ctrl$na_value)

  output <- list(calibration_metadata = calibration_metadata)

  if (type == "csv") {
    write_to_csv(output = output, sim_id = ctrl$sim_id, gen_n = 1)
  } else if (type == "db") {
    write_to_db(file = ctrl$file_name, output = output)
  }
  return(invisible())
}
