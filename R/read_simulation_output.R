#' Read calibration output
#'
#' @inheritParams calib_aeme
#' @inheritParams AEME::build_ensemble
#' @param raw logical. If TRUE, return the raw calibration output as a dataframe
#' with the "fit" and "gen" columns. This is generally used when restarting a
#' calibration.
#'
#' @importFrom dplyr case_when left_join mutate select summarise group_by
#' @importFrom tidyr pivot_longer
#' @importFrom utils read.csv
#' @importFrom stats median
#' @importFrom DBI dbConnect dbReadTable dbDisconnect
#' @importFrom duckdb duckdb
#' @importFrom tools file_ext
#'
#' @return A data frame with the calibration results.
#' @export

read_simulation_output <- function(ctrl, file = NULL, sim_id = NULL) {

  meta_tables <- c("lake_metadata", "simulation_metadata",
                   "function_metadata", "parameter_metadata",
                   "simulation_data")
  if (ctrl$method == "sa") {
    meta_tables <- c(meta_tables, "sensitivity_metadata")
  } else if (ctrl$method == "calib") {
    meta_tables <- c(meta_tables, "calibration_metadata")
  }
  sim_vec <- sim_id

  if (is.null(file)) {
    type <- ctrl$file_type
    if (type == "db") {
      file <- ctrl$file_name
    } else if (type == "csv") {
      file <- paste0(meta_tables, ".csv")
    }
  } else {
    file <- file
  }

  if (!all(file.exists(file))) {
    stop("File not found: ", file)
  }

  # if (is.null(sim_id)) {
  #   simu_id <- ctrl$sim_id
  # } else {
  #   simu_id <- sim_id
  # }

  # type <- tools::file_ext(file)

  names(meta_tables) <- meta_tables

  # all <- lapply(sim_id, \(sid) {
  if (type == "csv") {
    out <- lapply(meta_tables, function(x) {
      if (x == "lake_metadata") {
        lake_id <- read.csv("simulation_metadata.csv") |>
          # dplyr::filter(grepl(sid, sim_id)) |>
          dplyr::filter(sim_id %in% sim_vec) |>
          dplyr::pull(id)
        read.csv(paste0(x, ".csv")) |>
          # dplyr::filter(grepl(lake_id, id)) |>
          dplyr::filter(id %in% lake_id) |>
          as.data.frame()
      } else {
        read.csv(paste0(x, ".csv")) |>
          dplyr::filter(sim_id %in% sim_vec) |>
          as.data.frame()
      }
    })
  } else if (type == "db") {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

    out <- lapply(meta_tables, function(x) {
      if (x == "lake_metadata") {
        lake_id <- dplyr::tbl(con, "simulation_metadata") |>
          dplyr::filter(sim_id %in% sim_vec) |>
          dplyr::pull(id)
        dplyr::tbl(con, x) |>
          dplyr::filter(id %in% lake_id) |>
          as.data.frame()
      } else {
        dplyr::tbl(con, x) |>
          dplyr::filter(sim_id %in% sim_vec) |>
          as.data.frame()
      }
    })
  }
  return(out)
}

#' @rdname read_simulation_output
#' @export
read_calib <- read_simulation_output

