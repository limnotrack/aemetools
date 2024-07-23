#' Read the calibration metadata
#'
#' This function reads the calibration metadata from the output file. The output
#' file can be either a DuckDB database or a CSV file. The function reads the
#' metadata and returns it as a data frame.
#'
#' @param file The path to the output file. It can either be a DuckDB database
#' or a CSV file.
#'
#' @return A data frame with the calibration metadata
#' @export
#'
#' @importFrom dplyr tbl group_by summarise left_join
#' @importFrom DBI dbConnect dbDisconnect
#'

read_calib_meta <- function(file, path = ".") {

  file <- file.path(path, file)

  if (!file.exists(file)) stop("File not found: ", file)
  type <- tools::file_ext(file)
  # Read the file
  if (type == "db") {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    sim_meta <- dplyr::tbl(con, "calibration_metadata") |>
      as.data.frame()
  } else if (type == "csv") {
    sim_meta <- read.csv(file.path(path, "calibration_metadata.csv"))
  }
  return(sim_meta)
}
