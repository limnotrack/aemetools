#' Read calibration output
#'
#' @inheritParams calib_aeme
#' @inheritParams AEME::build_aeme
#' @inheritParams read_simulation_meta
#' @param sim_id A vector of simulation IDs to read. If NULL, all simulations
#' are read.
#' @param type A character string indicating the type of simulation. One of
#' "calib", "sa", or "all". If missing, the type is inferred from the
#' `ctrl` argument. If type is provided it overrides the `ctrl$method` value.
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

read_simulation_output <- function(ctrl = NULL, file_name, file_dir, 
                                   file_type = "db", sim_id = NULL, type) {

  meta_tables <- c("lake_metadata", "simulation_metadata",
                   "function_metadata", "parameter_metadata",
                   "simulation_data")
  if (!is.null(ctrl) & missing(type)) {
    type <- ctrl$method
  }
  if (!is.null(ctrl)) {
    file_dir <- ctrl$file_dir
    file_name <- ctrl$file_name
    file_type <- ctrl$file_type
  }
  
  if (type == "sa") {
    meta_tables <- c(meta_tables, "sensitivity_metadata")
  } else if (type == "calib") {
    meta_tables <- c(meta_tables, "calibration_metadata")
  } else if (type == "all") {
    meta_tables <- c(meta_tables, "sensitivity_metadata",
                     "calibration_metadata")
  }
  names(meta_tables) <- meta_tables
  
  if (file_type == "db") {
    file <- file.path(file_dir, file_name)
  } else if (file_type == "csv") {
    file <- file.path(file_dir, paste0(meta_tables, ".csv"))
  }
  
  sim_vec <- sim_id

  if (!all(file.exists(file)) & type != "all") {
    stop("File not found: ", file)
  } else if (type == "all") {
    not_found <- !file.exists(file)
    message("File not present: ", file[not_found])
    meta_tables <- meta_tables[!not_found]
  }


  # all <- lapply(sim_id, \(sid) {
  if (file_type == "csv") {
    out <- lapply(meta_tables, function(x) {
      df <- read.csv(file.path(file_dir, paste0(x, ".csv")))
      if (!is.null(sim_id)) {
        if (x == "lake_metadata") {
          lake_id <- read.csv(file.path(file_dir, "simulation_metadata.csv")) |>
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
  } else if (file_type == "db") {
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

