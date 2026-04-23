#' @param hydro_model [HydroModel]; object created by [make_GR_inputs()].
#' @param warmup integer vector; warm-up period row indices.
#' @param run_index integer vector; run period row indices.
#' @inheritParams airGR::CreateRunOptions
#'
#' @importFrom airGR CreateRunOptions
#'
#' @noRd

make_RunOptions <- function(hydro_model, warmup, run_index, IniStates,
                            IniResLevels) {
  IndPeriod_WarmUp <- if (!is.null(warmup)) warmup else NULL

  airGR::CreateRunOptions(
    FUN_MOD          = hydro_model@fun_mod,
    InputsModel      = hydro_model@inputs_model,
    IndPeriod_Run    = run_index,
    IniStates        = IniStates,
    IniResLevels     = IniResLevels,
    IndPeriod_WarmUp = IndPeriod_WarmUp
  )
}
