#' Read calibration output
#'
#' @inheritParams calib_aeme
#' @inheritParams AEME::build_ensemble
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

read_calib <- function(ctrl, model, path = ".") {

  file <- file.path(path, ctrl$out_file)
  type <- tools::file_ext(file)

  if (type == "csv") {
    out <- utils::read.csv(ctrl$out_file)
  } else if (type == "db") {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file)
    out <- DBI::dbReadTable(con, "calib_output")
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
  ngen <- ceiling(nrow(out) / ctrl$NP)
  if ("gen" %in% names(out)) {
    out$gen <- factor(out$gen)
  } else {
    out$gen <- factor(rep(1:ngen, each = ctrl$NP, length.out = nrow(out)))
  }
  out$index <- 1:nrow(out)
  mlt <- tidyr::pivot_longer(out, cols = -c(fit:index),
                             names_to = "parameter", values_to = "value") |>
    as.data.frame()

  gen_fit <- mlt |>
    dplyr::group_by(gen, parameter) |>
    dplyr::summarise(gen_fit = stats::median(fit), .groups = "drop")

  mlt |>
    dplyr::left_join(gen_fit, by = c("gen", "parameter")) |>
    dplyr::mutate(fit = dplyr::case_when(
      fit == ctrl$na_value ~ NA,
      .default = fit
    ),
    model = model) |>
    dplyr::select(model, gen, index, parameter, value, dplyr::everything())
}

