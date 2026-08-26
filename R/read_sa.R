#' Read sensitivity analysis output
#'
#' @inheritParams calib_aeme
#' @inheritParams AEME::build_aeme
#' @inheritParams sensobol::sobol_indices
#' @inheritParams read_simulation_output
#' @param R positive integer; number of bootstrap replicas passed to
#' `sensobol::sobol_indices()`/`sobol_dummy()`. Only used when `boot = TRUE`
#' (the default here). Defaults to `1000`, a common choice for bootstrap
#' confidence intervals - unlike `sensobol`'s own functions, which default to
#' `R = NULL` because they default to `boot = FALSE`. If you pass
#' `boot = TRUE` explicitly with `R = NULL`, this errors immediately with a
#' clear message rather than letting the failure surface deep inside
#' `boot::boot()`.
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

read_sa <- function(ctrl = NULL, file_name, file_dir, sim_id, R = 1000,
                    boot = TRUE) {

  if (boot && (is.null(R) || !is.numeric(R) || length(R) != 1 || R < 1)) {
    cli::cli_abort(c(
      "{.arg R} must be a single positive integer when {.arg boot} is
      {.val TRUE}.",
      "i" = "Got {.val {R}}. Either supply {.arg R} or set
      {.code boot = FALSE} for point estimates without bootstrap CIs."
    ))
  }

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

  # Failed runs are written as NA (see write_simulation_output()'s
  # `na_if(fit_value, na_value)`). Rather than plugging in a single fixed
  # `na_value` sentinel - which can dominate the variance decomposition if
  # it's far outside the metric's natural range - impute the 97.5th
  # percentile of that variable's own successful runs, so a failure still
  # counts as a bad outcome without swamping the Sobol' estimator. This
  # applies uniformly to sim_ids written before or after this change, since
  # both store failures as NA in the same way.
  impute_failed <- function(Y, na_value) {
    if (any(is.na(Y))) {
      fill <- stats::quantile(Y, probs = 0.975, na.rm = TRUE)
      if (is.na(fill)) fill <- na_value # every run failed - nothing to derive a fill from
      Y[is.na(Y)] <- fill
    }
    Y
  }

  # Sobol' first/total-order indices divide by Var(Y), so a near-constant Y
  # (e.g. a "run_failed" indicator when nothing failed) gives NaN/garbage
  # indices rather than a real error. `sd(Y) < 1e-3` (the previous check) was
  # an absolute threshold, which silently mis-skips any variable whose
  # natural scale happens to be small. Scale the threshold to the variable's
  # own magnitude instead, so it's scale-invariant.
  is_degenerate <- function(Y, rel_tol = 1e-6) {
    Y <- Y[!is.na(Y)]
    if (length(Y) < 2) return(TRUE)
    scale <- max(abs(Y))
    if (scale == 0) return(TRUE) # every value is exactly zero
    sd(Y) < rel_tol * scale
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
      Y <- impute_failed(Y, na_value)
      Y[Y > 1e10] <- na_value
      if (is_degenerate(Y)) {
        AEME::cli_safe(paste0("Skipping {.val ", v, "}: near-zero variance, ",
                              "Sobol' indices are not computable (division by ",
                              "~0 variance)."),
                       FUN = cli::cli_warn)
        return()
      }
      sensobol::sobol_indices(Y = Y, N = N, params = params, boot = boot, R = R)
    })

    sobol_dummy_indices <- lapply(vars, function(v) {
      Y <- wid[[v]]
      Y <- impute_failed(Y, na_value)
      Y[Y > 1e10] <- na_value
      if (is_degenerate(Y)) return()
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

