#' Read the simulation metadata
#'
#' This function reads the simulation metadata from the output file. The output
#' file can be either a DuckDB database or a CSV file. The function reads the
#' metadata and returns it as a data frame.
#'
#' @param ctrl A list with the control parameters used in `calib_aeme()` or
#' `sa_aeme()`. If `ctrl` is provided, the `file_name` and `file_dir` arguments
#' are ignored.
#' @param file_name The name of the output file. If `ctrl` is provided, this
#' argument is ignored.
#' @param file_dir The directory of the output file. If `ctrl` is provided, this
#' argument is ignored.
#'
#' @return A data frame with the simulation metadata
#' @export
#'
#' @importFrom dplyr tbl group_by summarise left_join
#' @importFrom DBI dbConnect dbDisconnect
#'

read_simulation_meta <- function(ctrl = NULL, file_name, file_dir) {

  # type <- file_type
  # if (!is.null(file_name)) {
  #   file <- file_name
  #   type <- tools::file_ext(file_name)
  # } else if (type == "db") {
  #   file <- file_name
  # } else if (type == "csv") {
  #   file <- "simulation_metadata.csv"
  # }

  if (!is.null(ctrl)) {
    file_dir <- ctrl$file_dir
    file_name <- ctrl$file_name
  }
  file <- file.path(ctrl$file_dir, ctrl$file_name)


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
      n_sim <- read.csv(file.path(file_dir, "simulation_data.csv")) |>
        dplyr::group_by(sim_id, gen) |>
        dplyr::summarise(nruns = max(run), .groups = "drop") |>
        dplyr::group_by(sim_id) |>
        dplyr::summarise(n_run = sum(nruns), .groups = "drop")
      sim_meta <- read.csv(file.path(file_dir, "simulation_metadata.csv")) |>
        dplyr::left_join(n_sim, by = "sim_id")
    }
  return(sim_meta)
}
