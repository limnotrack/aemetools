#' Write CSV to database
#'
#' @inheritParams AEME::build_aeme
#'
#' @return file path to the database.
#' @export
#'

write_csv_to_db <- function(path, file_name = "results.db") {

  ctrl <- list(file_type = "csv", method = "all", file_name = file_name)

  output <- read_simulation_output(ctrl = ctrl, path = path)

  write_to_db(file = ctrl$file_name, output = output, path = path,
              add_lake_meta = TRUE)

  out_file <- file.path(path, ctrl$file_name)
  return(out_file)
}
