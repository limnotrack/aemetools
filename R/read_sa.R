#' Read calibration output
#'
#' @inheritParams calib_aeme
#' @inheritParams AEME::build_ensemble
#' @inheritParams sensobol::sobol_indices
#'
#' @importFrom dplyr case_when left_join mutate select summarise group_by
#' @importFrom tidyr pivot_longer
#' @importFrom utils read.csv
#' @importFrom stats median
#' @importFrom DBI dbConnect dbReadTable dbDisconnect
#' @importFrom duckdb duckdb
#' @importFrom tools file_ext
#' @importFrom sensobol sobol_indices
#'
#' @return A list with thedata frame with the sensitivity analysis results and
#' the sobol indices for each variable.
#' @export

read_sa <- function(ctrl, model, name = "sa_output", path = ".", R = 2^3,
                    boot = TRUE) {

  file <- file.path(path, ctrl$out_file)
  type <- tools::file_ext(file)

  if (type == "csv") {
    out <- utils::read.csv(file)
  } else if (type == "db") {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file)
    out <- DBI::dbReadTable(con, name)
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
  # ngen <- ceiling(nrow(out) / ctrl$NP)
  # if (length(ngen) == 0) {
  #   ngen <- 1
  # }
  # if ("gen" %in% names(out)) {
  #   out$gen <- factor(out$gen)
  # } else {
  #   out$gen <- factor(rep(1:ngen, each = ctrl$NP, length.out = nrow(out)))
  # }
  mat <- out |>
    dplyr::select(-c(fit:dplyr::last_col())) |>
    as.matrix()

  vars <- out |>
    dplyr::select(c(fit:dplyr::last_col())) |>
    dplyr::select(-fit) |>
    names()
  names(vars) <- vars

  params <- gsub("NA.", "", colnames(mat))
  N <- ctrl$N

  sobol_indices <- lapply(vars, function(v) {
    Y <- out[[v]]
    # Y[Y > 100] <- 999
    sensobol::sobol_indices(Y = Y, N = N, params = params, boot = boot, R = R)
  })

  sobol_dummy_indices <- lapply(vars, function(v) {
    Y <- out[[v]]
    # Y[Y > 100] <- 999
    sensobol::sobol_dummy(Y = Y, N = N, params = params, boot = boot, R = R)
  })


  out$index <- 1:nrow(out)
  mlt <- tidyr::pivot_longer(out, cols = -c(fit:dplyr::last_col()),
                             names_to = "parameter", values_to = "value") |>
    dplyr::select(-fit) |>
    tidyr::pivot_longer(cols = !c(index:value),
                        names_to = "variable", values_to = "output") |>
    dplyr::mutate(parameter = gsub("NA.", "", parameter)) |>
    as.data.frame()
#
#   gen_fit <- mlt |>
#     dplyr::group_by(gen, parameter) |>
#     dplyr::summarise(gen_fit = stats::median(fit), .groups = "drop")

  df <- mlt |>
    # dplyr::left_join(gen_fit, by = c("gen", "parameter")) |>
    dplyr::mutate(output = dplyr::case_when(
      output == ctrl$na_value ~ NA,
      .default = output
    ),
    model = model) |>
    dplyr::select(model, index, parameter, value, dplyr::everything())

  list(df = df, sobol_indices = sobol_indices,
       sobol_dummy_indices = sobol_dummy_indices)
}

