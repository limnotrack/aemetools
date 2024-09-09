#' Write calibration/sensitivity analysis .csv files to database
#'
#' @param file_dir The directory where the .csv file is located.
#' @param file_name The name of the database file to write to. Default is
#' "results.db".
#'
#' @return file path to the database.
#' @export
#'

write_csv_to_db <- function(file_dir, file_name = "results.db") {

  ctrl <- list(file_type = "csv", method = "all", file_name = file_name,
               file_dir = file_dir)

  output <- read_simulation_output(ctrl = ctrl)

  write_to_db(file = ctrl$file_name, output = output, path = file_dir,
              add_lake_meta = TRUE)

  out_file <- file.path(file_dir, ctrl$file_name)
  return(out_file)
}
