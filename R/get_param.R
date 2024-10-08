#' Get parameter values from calibration results
#'
#' @param calib A list with the calibration results loaded using
#' \code{\link{read_calib}}.
#' @param na_value A numeric value which corresponds to the NA value used in
#' the calibration.
#' @param best A logical value indicating whether to return the best parameter
#' values or all parameter values.
#' @inheritParams plot_calib
#'
#' @importFrom dplyr case_when filter group_by mutate summarise
#' @importFrom stringr str_split_i
#'
#' @return A data frame with the parameter values.
#' @export

get_param <- function(calib, na_value, fit_col = "fit", best = FALSE) {

  # lapply(calib, \(x) {
  if (!all(fit_col %in% calib$simulation_data$fit_type)) {
    stop("fit_col not in calib")
  }
  # })

  sim_ids <- calib$simulation_metadata$sim_id

  all_pars <- lapply(sim_ids, \(x) {
    # calib$fit <- calib[[fit_col]]
    model <- calib$simulation_metadata |>
      dplyr::filter(sim_id == x) |>
      dplyr::pull(model)

    df_idx <- calib$simulation_data |>
      dplyr::filter(sim_id == x) |>
      dplyr::filter(fit_type == calib$simulation_data$fit_type[1]) |>
      tidyr::pivot_wider(id_cols = c("gen", "run"), names_from = parameter_name,
                         values_from = parameter_value) |>
      dplyr::mutate(index = 1:dplyr::n()) |>
      as.data.frame() |>
      dplyr::select(gen, run, index)


    calib$simulation_data |>
      dplyr::filter(sim_id == x) |>
      dplyr::left_join(df_idx, by = c("gen", "run")) |>
      dplyr::filter(
        fit_type %in% fit_col
      ) |>
      dplyr::mutate(
        model = model,
        fit2 = dplyr::case_when(
          fit_value == na_value ~ NA,
          .default = fit_value
        )) |>
      dplyr::mutate(
        label = abbrev_pars(parameter_name, model),
        gen = factor(gen),
        name = stringr::str_split_i(parameter_name, "^[^/]*/", 2),
        group = stringr::str_split_i(parameter_name, "/", 1),
        par = stringr::str_split_i(label, "%", 2)
      ) |>
      dplyr::mutate(group = dplyr::case_when(
        group == "NA" ~ NA,
        .default = group
      ))
  }) |>
    dplyr::bind_rows() |>
    dplyr::select(sim_id, model, gen, run, index, dplyr::everything())



  if (!best) return(all_pars)

  all_pars |>
    dplyr::filter(fit_value != na_value) |>
    dplyr::group_by(sim_id, model, label, fit_type) |>
    dplyr::summarise(parameter_value = parameter_value[which.min(fit_value)],
                     fit_value = min(fit_value),
                     gen = gen[which.min(fit_value)],
                     name = name[which.min(fit_value)],
                     group = group[which.min(fit_value)],
                     par = par[which.min(fit_value)],
                     .groups = "drop") |>
    as.data.frame()
}



#' Abbreviate parameters
#'
#' @param par character; with parameter names.
#' @param model character; with model name.
#' @return vector; with abbreviated parameter names.
#' @noRd
abbrev_pars <- function(par, model) {
  par1 <- gsub("NA.", "", par)
  if (all(model == "dy_cd")) {
    dy_abbrev <- function(string) {
      # Split the string into words
      words <- strsplit(string, "_")[[1]]
      if (length(words) > 1) {
        # Extract the first letter of each word
        initials <- abbreviate(words, 3)

        # Concatenate the initials to form the abbreviation
        abbreviation <- paste(initials, collapse = "_")

        return(abbreviation)
      } else {
        return(string)
      }
    }
    par2 <- sub("\\/.*", "", par1)
    par2 <- sapply(par2, \(x) {
      if (!grepl("MET_", x)) {
        dy_abbrev(x)
      } else {
        x
      }
    })
  } else if (all(model == "glm_aed")) {
    # par2 <- sub(".*\\.", "", par1)
    par2 <- sub(".*/", "", par1)
  } else if (all(model == "gotm_wet")) {
    par2 <- sub(".*/", "", par1)
    if ("constant_value" %in% par2) {
      par2[par2 == "constant_value"] <- sub(".*/([^/]+)/.*", "\\1",par1[par2 == "constant_value"])
    }
  }
  if (any(grepl("MET_", par2))) {
    par2 <- sub("MET_", "", par2)
  }
  return(par2)

  # names(params) <- params1
  # par_ref <- data.frame(parameter = params1, label = params)
}

