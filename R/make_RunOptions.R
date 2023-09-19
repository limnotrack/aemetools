#' @inheritParams run_GR
#' @inheritParams airGR::CreateRunOptions
#'
#' @importFrom airGR CreateRunOptions
#'
#' @noRd

make_RunOptions <- function(inputs, warmup, run_index, IniStates,
                            IniResLevels) {
  if (!is.null(warmup)) {
    IndPeriod_WarmUp <- warmup
  } else {
    IndPeriod_WarmUp <- NULL
  }

  Ind_Run <- run_index

  airGR::CreateRunOptions(FUN_MOD = inputs$FUN_MOD,
                          InputsModel = inputs$InputsModel,
                          IndPeriod_Run = Ind_Run,
                          IniStates = IniStates, IniResLevels = IniResLevels,
                          IndPeriod_WarmUp = IndPeriod_WarmUp)
}
