#' Write calibration output to file
#'
#' @inheritParams utils::write.csv
#' @inheritParams DBI::dbWriteTable
#' @param t0 POSIXct; start time of the calibration
#'
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom duckdb duckdb
#'
#' @return \code{write_calib_output} writes the calibration output to a file
#' @noRd
#'

write_calib_metadata <- function(ctrl, nsim, t0) {

  # Check output type
  type <- ctrl$file_type

  # Table names
  tbl_names <- c("calibration_metadata")

  time_started <- format(t0, "%Y-%m-%d %H:%M:%S")
  time_finished <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  time_elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))

  calibration_metadata <- data.frame(sim_id = ctrl$sim_id, n_sim = nsim,
                                     ncore = ctrl$ncore, VTR = ctrl$VTR,
                                     NP = ctrl$NP, ngen = ctrl$ngen,
                                     itermax = ctrl$itermax,
                                     reltol = ctrl$reltol, cutoff = ctrl$cutoff,
                                     mutate = ctrl$mutate,
                                     na_value = ctrl$na_value,
                                     c_method = ctrl$c_method,
                                     time_started = time_started,
                                     time_finished = time_finished,
                                     time_elapsed = time_elapsed)

  output <- list(calibration_metadata = calibration_metadata)

  # Create directory if it does not exist for output
  path <- ctrl$file_dir
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }


  if (type == "csv") {
    write_to_csv(output = output, path = path, sim_id = ctrl$sim_id, gen_n = 1)
  } else if (type == "db") {
    write_to_db(file = ctrl$file_name, path = path, output = output)
  }
  return(invisible())
}
