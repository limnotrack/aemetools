#' Read the calibration metadata
#'
#' This function reads the calibration metadata from the output file. The output
#' file can be either a DuckDB database or a CSV file. The function reads the
#' metadata and returns it as a data frame.
#'
#' @param file The path to the output file. It can either be a DuckDB database
#' or a CSV file.
#' @inheritParams AEME::build_aeme
#'
#' @return A data frame with the calibration metadata
#' @export
#'
#' @importFrom dplyr tbl group_by summarise left_join
#' @importFrom DBI dbConnect dbDisconnect
#'

read_calib_meta <- function(file, file_dir = "calib") {

  file <- file.path(file_dir, file)

  if (!file.exists(file)) stop("File not found: ", file)
  type <- tools::file_ext(file)
  # Read the file
  if (type == "db") {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    sim_meta <- dplyr::tbl(con, "calibration_metadata") |>
      as.data.frame()
  } else if (type == "csv") {
    sim_meta <- read.csv(file.path(file_dir, "calibration_metadata.csv"))
  }
  return(sim_meta)
}
