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

read_sa <- function(ctrl, sim_id, R = NULL, boot = TRUE) {

  out <- read_simulation_output(ctrl = ctrl, sim_id = sim_id, method = "sa")
  if (nrow(out$simulation_data) == 0) {
    stop("No data found for that sim_id. Check the sim_id.")
  }

  all <- lapply(sim_id, \(sid) {
    wid <- out$simulation_data |>
      dplyr::filter(sim_id == sid) |>
      tidyr::pivot_wider(names_from = c(parameter_name),
                         values_from = c(parameter_value)) |>
      tidyr::pivot_wider(names_from = fit_type, values_from = fit_value) |>
      dplyr::select(-c(sim_id, gen, run))

    model <- out$simulation_metadata |>
      dplyr::filter(sim_id == sid) |>
      dplyr::pull(model)

    vars <- out$sensitivity_metadata |>
      dplyr::filter(sim_id == sid) |>
      dplyr::pull(variable) |>
      unique()
    # vars <- c("fit", vars)

    mat <- wid |>
      dplyr::select(-dplyr::all_of(c("fit", vars))) |>
      as.matrix()

    # vars <- out |>
    #   dplyr::select(c(fit:dplyr::last_col())) |>
    #   dplyr::select(-fit) |>
    #   names()
    names(vars) <- vars

    params1 <- gsub("NA.", "", colnames(mat))
    params <- abbrev_pars(params1, model)

    # names(params) <- params1
    par_ref <- data.frame(parameter_name = params1, label = params)
    N <- ctrl$N

    sobol_indices <- lapply(vars, function(v) {
      Y <- wid[[v]]
      # Y[Y > 100] <- 999
      Y[is.na(Y)] <- ctrl$na_value
      if (sd(Y, na.rm = TRUE) < 1e-3) return()
      sensobol::sobol_indices(Y = Y, N = N, params = params, boot = boot, R = R)
    })

    sobol_dummy_indices <- lapply(vars, function(v) {
      Y <- wid[[v]]
      sensobol::sobol_dummy(Y = Y, N = N, params = params, boot = boot, R = R)
    })


    # out$index <- 1:nrow(out)
    # mlt <- tidyr::pivot_longer(out, cols = -c(fit:dplyr::last_col()),
    #                            names_to = "parameter", values_to = "value") |>
    #   dplyr::select(-fit) |>
    #   tidyr::pivot_longer(cols = !c(index:value),
    #                       names_to = "variable", values_to = "output") |>
    #   dplyr::mutate(parameter = gsub("NA.", "", parameter)) |>
    #   as.data.frame()
    #
    #   gen_fit <- mlt |>
    #     dplyr::group_by(gen, parameter) |>
    #     dplyr::summarise(gen_fit = stats::median(fit), .groups = "drop")

    df <- out$simulation_data |>
      dplyr::filter(sim_id == sid) |>
      dplyr::mutate(
        model = model,
        label = abbrev_pars(parameter_name, model),
        fit_value = dplyr::case_when(
          fit_value == ctrl$na_value ~ NA,
          .default = fit_value
        )) |>
      dplyr::select(sim_id, model, run, dplyr::everything())


    list(df = df, sobol_indices = sobol_indices,
         sobol_dummy_indices = sobol_dummy_indices)

  })
  names(all) <- sim_id
  return(all)
}

