#' Read sensitivity analysis output
#'
#' @inheritParams calib_aeme
#' @inheritParams AEME::build_aeme
#' @inheritParams sensobol::sobol_indices
#' @inheritParams read_simulation_output
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

read_sa <- function(ctrl = NULL, file_name, file_dir, sim_id, R = NULL,
                    boot = TRUE) {
  
  if (is.null(ctrl)) {
    ctrl <- list()
    ctrl$file_dir <- file_dir
    ctrl$file_name <- file_name
    ctrl$file_type <- tools::file_ext(file_name)
    ctrl$method <- "sa"
  }
  
  out <- read_simulation_output(ctrl = ctrl, sim_id = sim_id)
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
    
    sim_meta <- out$simulation_metadata |>
      dplyr::filter(sim_id == sid)
    sa_meta <- out$sensitivity_metadata |>
      dplyr::filter(sim_id == sid)
    
    model <- sim_meta |>
      dplyr::pull(model)
    
    vars <- out$sensitivity_metadata |>
      dplyr::filter(sim_id == sid) |>
      dplyr::pull(variable) |>
      unique()
    # vars <- c("fit", vars)
    
    par_df <- out$parameter_metadata |>
      dplyr::filter(sim_id == sid) 
    if (!is.null(ctrl$N)) {
      N <- ctrl$N
    } else {
      N <- nrow(wid) / (nrow(par_df) + 2)
    }
    if (is.null(ctrl$na_value)) {
      if ("na_value" %in% names(sa_meta)) {
        na_value <- sa_meta |> 
          dplyr::pull(na_value) |>
          unique()
        if (length(na_value) > 1) {
          stop("Multiple na_value found in sensitivity_metadata for sim_id ",
               sid)
        }
      } else {
        na_value <- 999
      }
    } else {
      na_value <- ctrl$na_value
    }
    
    mat <- wid |>
      dplyr::select(-dplyr::all_of(c(vars))) |>
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
    
    sobol_indices <- lapply(vars, function(v) {
      Y <- wid[[v]]
      Y[is.na(Y)] <- na_value
      Y[Y > 1e10] <- na_value
      if (sd(Y, na.rm = TRUE) < 1e-3) return()
      sensobol::sobol_indices(Y = Y, N = N, params = params, boot = boot, R = R)
    })
    
    sobol_dummy_indices <- lapply(vars, function(v) {
      Y <- wid[[v]]
      Y[is.na(Y)] <- na_value
      Y[Y > 1e10] <- na_value
      if (sd(Y, na.rm = TRUE) < 1e-3) return()
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
          fit_value == na_value ~ NA,
          .default = fit_value
        )) |>
      dplyr::select(sim_id, model, run, dplyr::everything())
    
    
    list(df = df, sobol_indices = sobol_indices,
         sobol_dummy_indices = sobol_dummy_indices)
    
  })
  names(all) <- sim_id
  return(all)
}

