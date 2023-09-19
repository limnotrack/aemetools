#' Calibrate GR model
#'
#' @inheritParams run_GR
#' @inheritParams airGR::CreateRunOptions
#' @inheritParams airGR::CreateInputsCrit
#' @inheritParams airGR::CreateCalibOptions
#'
#' @import airGR
#'
#' @return list of airGR calibration outputs.
#'
#' @export

calib_GR <- function(inputs, warmup = NULL, run_index,
                     FUN_CRIT = airGR::ErrorCrit_NSE,
                     FUN_CALIB = airGR::Calibration_Michel,
                     IniStates = NULL,
                     IniResLevels = NULL) {

  RunOptions <- make_RunOptions(inputs = inputs, warmup = warmup,
                                run_index = run_index,
                                IniStates = IniStates,
                                IniResLevels = IniResLevels)

  InputsCrit <- airGR::CreateInputsCrit(FUN_CRIT = FUN_CRIT,
                                        InputsModel = inputs$InputsModel,
                                        RunOptions = RunOptions, VarObs = "Q",
                                        Obs = inputs$data$Qmm[run_index])

  CalibOptions <- airGR::CreateCalibOptions(FUN_MOD = inputs$FUN_MOD,
                                            FUN_CALIB = FUN_CALIB)

  airGR::Calibration_Michel(InputsModel = inputs$InputsModel,
                            RunOptions = RunOptions,
                            InputsCrit = InputsCrit,
                            CalibOptions = CalibOptions,
                            FUN_MOD = inputs$FUN_MOD)
}
