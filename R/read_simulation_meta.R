#' Read the simulation metadata
#'
#' This function reads the simulation metadata from the output file. The output
#' file can be either a DuckDB database or a CSV file. The function reads the
#' metadata and returns it as a data frame.
#'
#' @param file The path to the output file. It can either be a DuckDB database
#' or a CSV file.
#'
#' @return A data frame with the simulation metadata
#' @export
#'
#' @importFrom dplyr tbl group_by summarise left_join
#' @importFrom DBI dbConnect dbDisconnect
#'

read_simulation_meta <- function(file, path = ".") {

  # type <- file_type
  # if (!is.null(file_name)) {
  #   file <- file_name
  #   type <- tools::file_ext(file_name)
  # } else if (type == "db") {
  #   file <- file_name
  # } else if (type == "csv") {
  #   file <- "simulation_metadata.csv"
  # }
  file <- file.path(path, file)

  if (!file.exists(file)) stop("File not found: ", file)
  type <- tools::file_ext(file)

  # Read the file
  if (type == "db") {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    n_sim <- dplyr::tbl(con, "simulation_data") |>
      dplyr::group_by(sim_id, gen) |>
      dplyr::summarise(nruns = max(run)) |>
      dplyr::group_by(sim_id) |>
      dplyr::summarise(n_run = sum(nruns)) |>
      as.data.frame()
    sim_meta <- dplyr::tbl(con, "simulation_metadata") |>
      as.data.frame() |>
      dplyr::left_join(n_sim, by = "sim_id")
    } else if (type == "csv") {
      n_sim <- read.csv(file.path(path, "simulation_data.csv")) |>
        dplyr::group_by(sim_id, gen) |>
        dplyr::summarise(nruns = max(run), .groups = "drop") |>
        dplyr::group_by(sim_id) |>
        dplyr::summarise(n_run = sum(nruns), .groups = "drop")
      sim_meta <- read.csv(file.path(path, "simulation_metadata.csv")) |>
        dplyr::left_join(n_sim, by = "sim_id")
    }
  return(sim_meta)
}
