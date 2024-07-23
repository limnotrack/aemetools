#' Read calibration output
#'
#' @inheritParams calib_aeme
#' @inheritParams AEME::build_aeme
#' @param file file name; to read. Can be either a "csv" or "db" file and it
#' must be located within the path.
#' @param sim_id A vector of simulation IDs to read.
#'
#' @importFrom dplyr case_when left_join mutate select summarise group_by
#' @importFrom tidyr pivot_longer
#' @importFrom utils read.csv
#' @importFrom stats median
#' @importFrom DBI dbConnect dbReadTable dbDisconnect
#' @importFrom duckdb duckdb
#' @importFrom tools file_ext
#'
#' @return A list with the metadata and simulation data frames.
#' @export

read_simulation_output <- function(ctrl, path = ".", file = NULL,
                                   sim_id = NULL) {

  meta_tables <- c("lake_metadata", "simulation_metadata",
                   "function_metadata", "parameter_metadata",
                   "simulation_data")
  if (ctrl$method == "sa") {
    meta_tables <- c(meta_tables, "sensitivity_metadata")
  } else if (ctrl$method == "calib") {
    meta_tables <- c(meta_tables, "calibration_metadata")
  } else if (ctrl$method == "all") {
    meta_tables <- c(meta_tables, "sensitivity_metadata",
                     "calibration_metadata")
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
  file <- file.path(path, file)

  if (!all(file.exists(file)) & ctrl$method != "all") {
    stop("File not found: ", file)
  } else if (ctrl$method == "all") {
    not_found <- !file.exists(file)
    message("File not present: ", file[not_found])
    meta_tables <- meta_tables[!not_found]
  }

  names(meta_tables) <- meta_tables

  # all <- lapply(sim_id, \(sid) {
  if (type == "csv") {
    out <- lapply(meta_tables, function(x) {
      df <- read.csv(file.path(path, paste0(x, ".csv")))
      if (!is.null(sim_id)) {
        if (x == "lake_metadata") {
          lake_id <- read.csv(file.path(path, "simulation_metadata.csv")) |>
            dplyr::filter(sim_id %in% sim_vec) |>
            dplyr::pull(id)
          df <- df |>
            dplyr::filter(id %in% lake_id) |>
            as.data.frame()
        } else {
          df <- df |>
            dplyr::filter(sim_id %in% sim_vec) |>
            as.data.frame()
        }
      }
      return(df)
    })
  } else if (type == "db") {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

    out <- lapply(meta_tables, function(x) {
      if (x == "lake_metadata" & !is.null(sim_id)) {
        lake_id <- dplyr::tbl(con, "simulation_metadata") |>
          dplyr::filter(sim_id %in% sim_vec) |>
          dplyr::pull(id)
        dplyr::tbl(con, x) |>
          dplyr::filter(id %in% lake_id) |>
          as.data.frame()
      } else {
        if (!is.null(sim_id)) {
          df <- dplyr::tbl(con, x) |>
            dplyr::filter(sim_id %in% sim_vec) |>
            as.data.frame()
        } else {
          df <- dplyr::tbl(con, x) |>
            as.data.frame()
        }
      }
    })
  }
  return(out)
}

#' @rdname read_simulation_output
#' @export
read_calib <- read_simulation_output

