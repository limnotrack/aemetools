#' Write calibration output to file
#'
#' @inheritParams utils::write.csv
#' @inheritParams DBI::dbWriteTable
#' @inheritParams AEME::build_ensemble
#'
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom duckdb duckdb
#'
#' @return \code{write_calib_output} writes the calibration output to a file
#' @noRd
#'

write_simulation_output <- function(x, ctrl, aeme, model, param, FUN_list,
                                    sim_id = NULL, append_metadata = TRUE) {

  # Extract meta information
  lke <- AEME::lake(aeme)
  tme <- AEME::time(aeme)
  cfg <- AEME::configuration(aeme)
  use_bgc <- !is.null(cfg[[model]]$ecosystem)

  # Create function string
  fun_string <- lapply(FUN_list, deparse1)

  # Check if ctrl exists & assign sim id
  type <- ctrl$file_type
  iter <- 1
  if (is.null(sim_id)) {
    sim_stem <- paste0(lke$id, "_", gsub("_", "", model), "_",
                       ifelse(ctrl$method == "calib", "C", "S"))
  } else {
    sim_stem <- paste0(strsplit(sim_id, "_")[[1]][1:3], collapse = "_")
  }

  # Table names
  tbl_names <- c("lake_metadata", "simulation_metadata", "function_metadata",
                 "parameter_metadata", "simulation_data")
  path <- ctrl$file_dir
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  file_to_check <- ifelse(type == "db", ctrl$file_name,
                          "simulation_metadata.csv")
  file_to_check <- file.path(path, file_to_check)

  sim_stem_chk <- data.frame()
  add_lake_meta <- TRUE
  if (file.exists(file_to_check)) {
    if (type == "db") {
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file_to_check)
      db_tabs <- DBI::dbListTables(con)
      if ("lake_metadata" %in% db_tabs) {
        lke_ids <- dplyr::tbl(con, "lake_metadata") |>
          dplyr::pull(id)
        add_lake_meta <- !lke$id %in% lke_ids
      }
      if ("simulation_metadata" %in% db_tabs) {
        # message("No simulation_metadata table found in ", file_to_check)
        sim_stem_chk <- dplyr::tbl(con, "simulation_metadata") |>
          dplyr::filter(grepl(sim_stem, sim_id)) |>
          as.data.frame()
      }
      DBI::dbDisconnect(con, shutdown = TRUE)
    } else if (type == "csv") {
      sim_stem_chk <- read.csv(file.path(path, "simulation_metadata.csv")) |>
        dplyr::filter(grepl(sim_stem, sim_id))
      lke_ids <- read.csv(file.path(path, "lake_metadata.csv")) |>
        dplyr::pull(id)
      add_lake_meta <- !lke$id %in% lke_ids
    }

    if (nrow(sim_stem_chk) > 0) {
      prev_iter <- sim_stem_chk |>
        dplyr::slice_tail(n = 1) |>
        dplyr::pull(sim_id) |>
        strsplit("_")
      prev_iter <- max(as.numeric(prev_iter[[1]][4]))
      iter <- prev_iter + 1
    }
  }

  if (is.null(sim_id)) {
    # format to a 3 character number, pad with leading zeros
    sim_id <- paste0(sim_stem, "_", formatC(iter, width = 3, flag = "0"))
  }

  if (append_metadata) {
    # Convert lke list to dataframe
    lake_meta <- data.frame(name = lke$name, id = lke$id, latitude = lke$latitude,
                            longitude = lke$longitude, elevation = lke$elevation,
                            depth = lke$depth, area = lke$area)

    # Simulation metadata
    sim_meta <- data.frame(sim_id = sim_id, id = lke$id, model = model,
                           spin_up = tme$spin_up[[model]], start = tme$start,
                           stop = tme$stop, use_bgc = use_bgc,
                           n_params = nrow(param), method = ctrl$method,
                           time_started = format(Sys.time()))

    # Function metadata
    fun_meta <- lapply(names(FUN_list), \(f) {
      data.frame(sim_id = sim_id, var = f, fun = deparse1(FUN_list[[f]]))
    }) |>
      dplyr::bind_rows()

    # Extract parameter info
    param_column_names <- get_param_column_names()
    param_meta <- param |>
      dplyr::mutate(sim_id = sim_id, group = as.character(group)) |>
      dplyr::select(sim_id, dplyr::all_of(param_column_names))

    # Sensitivity analysis metadata
    if (ctrl$method == "sa") {
      sa_meta <- lapply(names(ctrl$vars_sim), \(n) {
        data.frame(sim_id = sim_id, variable = n, var = ctrl$vars_sim[[n]]$var,
                   depth_from = min(ctrl$vars_sim[[n]]$depth_range),
                   depth_to = max(ctrl$vars_sim[[n]]$depth_range),
                   month = ctrl$vars_sim[[n]]$month)
      }) |>
        dplyr::bind_rows()
    }
  }


  # Format simulation output
  sims <- x |>
    as.data.frame() |>
    dplyr::mutate(run = 1:dplyr::n()) |>
    dplyr::select(run, dplyr::everything())

  if (!"gen" %in% names(sims)) {
    sims$gen <- 1
  }

  # pivot_longer to get the output in long format
  sim_data <- sims |>
    tidyr::pivot_longer(cols = dplyr::contains("/"), names_to = "pname",
                        values_to = "pvalue") |>
    tidyr::pivot_longer(cols = dplyr::contains("_") | dplyr::contains("fit"),
                        names_to = "fit_type", values_to = "fit_value") |>
    as.data.frame() |>
    dplyr::rename(parameter_name = pname, parameter_value = pvalue) |>
    dplyr::mutate(sim_id = sim_id) |>
    dplyr::select(sim_id, gen, run, parameter_name, parameter_value, fit_type, fit_value)


  if (!append_metadata) {
    output <- list(simulation_data = sim_data)
  } else {
    output <- list(lake_metadata = lake_meta, simulation_metadata = sim_meta,
                   function_metadata = fun_meta, parameter_metadata = param_meta,
                   simulation_data = sim_data)
    if (ctrl$method == "sa") {
      output$sensitivity_metadata <- sa_meta
    }
  }

  if ("gen" %in% colnames(sim_data)) {
    gen_n <- sim_data[1, "gen"]

    file_to_print <- ifelse(type == "db", ctrl$file_name, "simulation_data.csv")

    message("Writing output for generation ", gen_n, " to ", file_to_print,
            " with sim ID: ", sim_id, " [", format(Sys.time()), "]")
  }

  if (type == "csv") {
    write_to_csv(output = output, sim_id = sim_id, gen_n = gen_n,
                 add_lake_meta = add_lake_meta, path = path)
  } else if (type == "db") {
    write_to_db(file = ctrl$file_name, output = output, path = path,
                add_lake_meta = add_lake_meta)
  }
  sim_id
}


#' Write output to csv
#'
#' @param output list; output to be written to the csv files
#' @param sim_id string; simulation ID
#' @param gen_n integer; generation number
#' @param add_lake_meta logical; whether to add lake metadata to the csv files.
#' Default is FALSE.
#'
#' @return NULL
#' @noRd
#'

write_to_csv <- function(output, path, sim_id, gen_n, add_lake_meta = FALSE) {
  for (i in seq_along(output)) {
    fname <- file.path(path, paste0(names(output)[i], ".csv"))
    file_chk <- file.exists(fname)
    if (names(output)[i] == "lake_metadata") {
      if (!add_lake_meta) next
    }
    if (gen_n == 1 & !file_chk) {
      write.csv(output[[i]], fname, quote = TRUE,
                row.names = FALSE)

    } else {
      write.table(output[[i]], fname, append = TRUE, sep = ",",
                  row.names = FALSE, col.names = FALSE)
    }
  }
}

#' Write output to database
#'
#' @param file filepath; to be used as the database
#' @param output list; output to be written to the database
#' @param add_lake_meta logical; whether to add lake metadata to the database.
#' Default is FALSE.
#'
#' @return NULL
#' @noRd
#'
#' @importFrom duckdb duckdb
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#'

write_to_db <- function(file, path, output, add_lake_meta = FALSE) {
  file_path <- file.path(path, file)
  file_chk <- file.exists(file_path)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  for (i in seq_along(output)) {
    if (names(output)[i] == "lake_metadata") {
      if (!add_lake_meta) next
    }
    if (names(output)[i] == "parameter_metadata") {
      if (any(is.na(output[[i]][["group"]]))) {
        output[[i]][["group"]][is.na(output[[i]][["group"]])] <- "NA"
      }
    }
    DBI::dbWriteTable(con, names(output)[i], output[[i]],
                      overwrite = !file_chk, append = file_chk)
  }
}
