#' Write calibration output to file
#'
#' @inheritParams utils::write.csv
#' @inheritParams DBI::dbWriteTable
#'
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom duckdb duckdb
#'
#' @return \code{write_calib_output} writes the calibration output to a file
#' @noRd
#'

write_calib_output <- function(x, file, name) {
  type <- tools::file_ext(file)
  if ("gen" %in% names(x)) {
    gen_n <- x[1, "gen"]

    message("Writing output for generation ", gen_n, " to ", basename(file),
            ". [", format(Sys.time()), "]")
  } else {
    gen_n <- 1
  }

  if (type == "csv") {
    if (gen_n == 1) {
      write.csv(x, file, quote = TRUE, row.names = FALSE)
    } else {
      write.table(x, file, append = TRUE, sep = ",", row.names = FALSE,
                  col.names = FALSE)
    }
  } else if (type == "db") {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file)
    append <- gen_n > 1
    DBI::dbWriteTable(con, name, as.data.frame(x),
                      overwrite = !append, append = append)
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
}
