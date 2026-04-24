#' Calibrate GR model
#'
#' Calibrate a GR hydrological model from the `airGR` package using a
#' [HydroModel] object that has been populated by [make_GR_inputs()].
#'
#' @param hydro_model [HydroModel]; object returned by [make_GR_inputs()].
#' @param warmup integer vector; row indices of `hydro_model@data` to use as
#'   the warm-up period. Defaults to `1:(hydro_model@start - 1)`.
#' @param run_index integer vector; row indices of `hydro_model@data` to use
#'   during calibration. Defaults to
#'   `hydro_model@start:nrow(hydro_model@data)`.
#' @inheritParams airGR::CreateRunOptions
#' @inheritParams airGR::CreateInputsCrit
#' @inheritParams airGR::CreateCalibOptions
#'
#' @import airGR
#'
#' @return An `airGR` `OutputsCalib` list.
#'
#' @export

calib_GR <- function(hydro_model, warmup = NULL, run_index = NULL,
                     FUN_CRIT  = airGR::ErrorCrit_NSE,
                     FUN_CALIB = airGR::Calibration_Michel,
                     IniStates = NULL,
                     IniResLevels = NULL) {

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

  InputsCrit <- airGR::CreateInputsCrit(
    FUN_CRIT     = FUN_CRIT,
    InputsModel  = hydro_model@inputs_model,
    RunOptions   = RunOptions,
    VarObs       = "Q",
    Obs          = hydro_model@data$Qmm[run_index]
  )

  CalibOptions <- airGR::CreateCalibOptions(
    FUN_MOD   = hydro_model@fun_mod,
    FUN_CALIB = FUN_CALIB
  )

  airGR::Calibration_Michel(
    InputsModel  = hydro_model@inputs_model,
    RunOptions   = RunOptions,
    InputsCrit   = InputsCrit,
    CalibOptions = CalibOptions,
    FUN_MOD      = hydro_model@fun_mod
  )
}
