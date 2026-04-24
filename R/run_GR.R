#' Run GR model
#'
#' Run a GR hydrological model from the `airGR` package using a [HydroModel]
#' object that has been populated by [make_GR_inputs()].
#'
#' @param hydro_model [HydroModel]; object returned by [make_GR_inputs()].
#' @param param numeric vector; model parameters. Usually obtained from
#'   [calib_GR()].
#' @param warmup integer vector; row indices of `hydro_model@data` to use as
#'   the warm-up period. Defaults to `1:(hydro_model@start - 1)`.
#' @param run_index integer vector; row indices of `hydro_model@data` to use
#'   for the model run. Defaults to
#'   `hydro_model@start:nrow(hydro_model@data)`.
#' @inheritParams airGR::CreateRunOptions
#'
#' @import airGR
#'
#' @return An `airGR` `OutputsModel` list with an additional
#'   `catchment_area` element (m²).
#' @export

run_GR <- function(hydro_model, param, warmup = NULL, run_index = NULL,
                   IniStates = NULL, IniResLevels = NULL) {

  if (is.null(hydro_model@inputs_model)) {
    stop("'hydro_model' has no airGR inputs. Run make_GR_inputs() first.")
  }

  if (is.null(warmup)) {
    warmup <- seq_len(hydro_model@start - 1L)
  }

  if (is.null(run_index)) {
    run_index <- hydro_model@start:nrow(hydro_model@data)
  }

  RunOptions <- make_RunOptions(hydro_model = hydro_model, warmup = warmup,
                                run_index = run_index,
                                IniStates = IniStates,
                                IniResLevels = IniResLevels)

  out <- hydro_model@fun_mod(InputsModel = hydro_model@inputs_model,
                              RunOptions = RunOptions, Param = param)
  out$catchment_area <- hydro_model@catchment_area
  out
}
