#' Get parameter values from calibration results
#'
#' @param calib A data frame with the calibration results.
#' @param model A character vector with the model name.
#' @param na_value A numeric value which corresponds to the NA value used in
#' the calibration.
#' @param best A logical value indicating whether to return the best parameter
#' values or all parameter values.
#'
#' @importFrom dplyr case_when filter group_by mutate summarise
#' @importFrom stringr str_replace
#'
#' @return A data frame with the parameter values.
#' @export

get_param <- function(calib, model, na_value, best = FALSE) {

  all_pars <- calib |>
    dplyr::filter(model %in% model) |>
    dplyr::mutate(fit2 = dplyr::case_when(
      fit == na_value ~ NA,
      .default = fit
    )) |>
    # filter(fit < na_value) |>
    dplyr::mutate(model = model, fit = fit) |>
    dplyr::mutate(dplyr::across("parameter", ~ gsub("\\.", "/", .x))) |>
    # dplyr::mutate(param = gsub("\\.", "/", param)) #|>
    dplyr::mutate(param2 = dplyr::case_when(
      model == "glm_aed" ~ gsub("^.*/", "", parameter),
      model == "gotm_wet" ~ stringr::str_replace(parameter, "^.*/([^/]+)/([^/]+)$",
                                                 "\\1/\\2"),
      model == "dy_cd" ~ gsub("/.*", "", parameter)
    ))

  if (!best) return(all_pars)

  all_pars |>
    dplyr::filter(fit != na_value) |>
    dplyr::group_by(model, param2) |>
    dplyr::summarise(value = value[which.min(fit)], fit = min(fit),
                     .groups = "drop") |>
    as.data.frame()



  # all_pars <- lapply(model, \(m) {
  #   df <- calib |>
  #     dplyr::filter(model == m) |>
  #     dplyr::mutate(fit2 = dplyr::case_when(
  #       fit == na_value ~ NA,
  #       .default = fit
  #     )) |>
  #     # filter(fit < na_value) |>
  #     dplyr::mutate(model = m, fit = fit)
  # }) |>
  #   do.call(rbind, .) |>
  #   dplyr::mutate(param = gsub("\\.", "/", param)) |>
  #   dplyr::mutate(param2 = dplyr::case_when(
  #     model == "glm_aed" ~ gsub("^.*/", "", param),
  #     model == "gotm_wet" ~ stringr::str_replace(param, "^.*/([^/]+)/([^/]+)$",
  #                                                "\\1/\\2"),
  #     model == "dy_cd" ~ gsub("/.*", "", param)
  #   ))
  #
  # if (best) {
  #   summ <- all_pars |>
  #     dplyr::filter(fit < na_value) |>
  #     dplyr::group_by(model, param2) |>
  #     dplyr::summarise(value = value[which.min(fit)], fit = min(fit))
  #   return(summ)
  # } else {
  #   return(all_pars)
  # }
}

