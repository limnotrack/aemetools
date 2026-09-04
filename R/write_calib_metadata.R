#' Write calibration output to file
#'
#' @inheritParams utils::write.csv
#' @inheritParams DBI::dbWriteTable
#' @param t0 POSIXct; start time of the calibration
#'
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom duckdb duckdb
#' @importFrom rlang `%||%`
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

  # VTR/NP/ngen/... describe the built-in generational search and are absent
  # from a PEST++ control, where the solver owns the search. A missing field
  # is NULL, which data.frame() turns into a zero-length column and then
  # rejects as "differing number of rows", so every optional field is read
  # through num_field() and recorded as NA instead.
  num_field <- function(nm) {
    v <- ctrl[[nm]]
    if (length(v) != 1) NA_real_ else as.numeric(v)
  }
  chr_field <- function(nm) {
    v <- ctrl[[nm]]
    if (length(v) != 1) NA_character_ else as.character(v)
  }

  calibration_metadata <- data.frame(sim_id = ctrl$sim_id, n_sim = nsim,
                                     ncore = ctrl$ncore,
                                     VTR = num_field("VTR"),
                                     NP = num_field("NP"),
                                     ngen = num_field("ngen"),
                                     itermax = num_field("itermax"),
                                     reltol = num_field("reltol"),
                                     cutoff = num_field("cutoff"),
                                     mutate = num_field("mutate"),
                                     cutoff_final = num_field("cutoff_final"),
                                     mutate_final = num_field("mutate_final"),
                                     na_value = ctrl$na_value,
                                     c_method = ctrl$c_method,
                                     # PEST++ leaves artefacts the database
                                     # cannot hold - per-observation
                                     # residuals (.rei), the Jacobian
                                     # (.jcb), FOSM covariances - so record
                                     # where they are, or a sim_id cannot be
                                     # traced back to them later.
                                     engine = chr_field("engine"),
                                     pest_dir = chr_field("pest_dir"),
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
