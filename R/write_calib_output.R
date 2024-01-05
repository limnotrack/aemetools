#' Write calibration output to file
#'
#' @inheritParams utils::write.csv
#'
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom duckdb duckdb
#'
#' @return \code{write_calib_output} writes the calibration output to a file
#' @noRd
#'

write_calib_output <- function(x, file) {
  type <- tools::file_ext(file)
  gen_n <- x[1, "gen"]

  message("Writing output for generation ", gen_n, " to ", file, ". [",
          Sys.time(), "]")

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
    DBI::dbWriteTable(con, "calib_output", as.data.frame(x),
                      overwrite = !append, append = append)
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
}
