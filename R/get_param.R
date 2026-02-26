#' Get parameter values from calibration results
#'
#' @param calib A list with the calibration results loaded using
#' \code{\link{read_calib}}.
#' @param best A logical value indicating whether to return the best parameter
#' values or all parameter values.
#' @inheritParams plot_calib
#' @inheritParams update_param
#'
#' @importFrom dplyr case_when filter group_by mutate summarise
#' @importFrom stringr str_split_i
#'
#' @return A data frame with the parameter values.
#' @export

get_param <- function(calib, na_value, fit_col = "fit", best = FALSE, 
                      quantile = 0.1) {
  
  # lapply(calib, \(x) {
  if (!all(fit_col %in% calib$simulation_data$fit_type)) {
    stop("fit_col not in calib")
  }
  # })
  if (missing(na_value)) {
    na_value <- calib$calibration_metadata$na_value[1]
  }
  
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
      dplyr::arrange(gen, run) |>
      dplyr::mutate(index = dplyr::row_number()) |>
      # dplyr::mutate(index = gen * run) |>
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
        gen = factor(gen),
        name = decode_param(parameter_name),
        label = abbrev_pars(parameter_name, model),
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
  
  param <- calib$parameter_metadata |> 
    dplyr::mutate(parameter_name = encode_param(group = group, name = name, 
                                                index = index)) |> 
    dplyr::select(sim_id, model, file, name, group, index, parameter_name)
  
  
  # uniq_pars <- unique(all_pars$name)
  # # Remove "outflow", "inflow" and ones that contain "MET"
  # uniq_pars <- uniq_pars[!uniq_pars %in% c("outflow", "inflow")]
  # uniq_pars <- uniq_pars[!grepl("MET", uniq_pars)]
  # if (length(uniq_pars) > 0) {
  #   aeme_pars <- AEME::get_aeme_parameters(name = uniq_pars) |> 
  #     dplyr::select(sim_id, model, file, name)
  # }
  
  qtile <- all_pars |> 
    dplyr::filter(fit_value != na_value) |> 
    dplyr::group_by(sim_id) |> 
    dplyr::summarise(q10 = quantile(fit_value, probs = quantile, na.rm = TRUE),
                     .groups = "drop")
  
  pars_df <- all_pars |>
    dplyr::left_join(qtile, by = "sim_id") |>
    dplyr::filter(fit_value != na_value, fit_value <= q10) |>
    dplyr::group_by(sim_id, parameter_name) |>
    dplyr::summarise(label = label[which.min(fit_value)],
                     gen = gen[which.min(fit_value)],
                     min = min(parameter_value), 
                     max = max(parameter_value),
                     parameter_value = parameter_value[which.min(fit_value)],
                     par = par[which.min(fit_value)],
                     fit_value = min(fit_value),
                     .groups = "drop") |> 
    dplyr::select(sim_id, parameter_name, parameter_value, min, max, fit_value,
                  gen) |> 
    dplyr::rename(value = parameter_value)
  
  param_names <- AEME::param_colnames(incl_opt = FALSE)
  param_df <- param |> 
    dplyr::left_join(pars_df, by = c("sim_id", "parameter_name")) |> 
    dplyr::arrange(sim_id, model, file, name, group, index) |> 
    dplyr::select(dplyr::all_of(c("sim_id", param_names, "fit_value", "gen"))) 
  return(param_df)
  
  
  # if (length(uniq_pars) > 0) {
  #   pars_df <- pars_df |> 
  #     dplyr::left_join(aeme_pars, by = c("model", "name"))
  # }
  # if (!"file" %in% colnames(pars_df)) {
  #   pars_df <- pars_df |> 
  #     dplyr::mutate(file = NA_character_)
  # }
  # 
  pars_df <- pars_df |> 
    dplyr::mutate(
      value = parameter_value,
      min = value, max = value,
      file = dplyr::case_when(
        grepl("MET", name) ~ "met",
        grepl("outflow", name) ~ "wdr",
        grepl("inflow", name) ~ "inf",
        .default = .data$file
      )
    ) |> 
    dplyr::select(dplyr::all_of(c("sim_id", param_names, "fit_value", "gen",
                                  "fit_type"))) 
  return(pars_df)
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
    par1 <- sub("\\[NA\\]", "", sub(".*/([^/]+)$", "\\1", par))
    par2 <- sub("\\/.*", "", par1)
    par2 <- sapply(par2, \(x) {
      if (!grepl("MET_", x)) {
        dy_abbrev(x)
      } else {
        x
      }
    })
  } else if (all(model == "glm_aed")) {
    par1 <- sub("^NA/", "", par)
    par2 <- sub("\\[NA\\]", "", par1)
    # par2 <- sub("\\[NA\\]", "", sub(".*/([^/]+)$", "\\1", par))
  } else if (all(model == "gotm_wet")) {
    par2 <- sub("\\[NA\\]", "", sub(".*/([^/]+)$", "\\1", par))
    if ("constant_value" %in% par2) {
      par2[par2 == "constant_value"] <- sub(".*/([^/]+)/.*", "\\1",par1[par2 == "constant_value"])
    }
  } else {
    par2 <- sub("\\[NA\\]", "", sub(".*/([^/]+)$", "\\1", par))
  }
  if (any(grepl("MET_", par2))) {
    par2 <- sub("MET_", "", par2)
  }
  return(par2)
  
  # names(params) <- params1
  # par_ref <- data.frame(parameter = params1, label = params)
}

