#' Get all upstream reaches
#'
#' @param HydroID numeric; identifier for the target reach
#' @param reaches sf dataframe; with all reaches with HydroID's
#'
#' @importFrom dplyr bind_rows
#'
#' @return sf dataframe of all upstream reaches
#' @export

get_upstream_rec <- function(HydroID, reaches) {

  # intial row
  upstr <- reaches[reaches$HydroID == HydroID, ]

  startLength <- 0
  # search upstream rec for connected reaches until output stops growing
  while (nrow(upstr) > startLength) {
    startLength <- nrow(upstr)
    new_reach <- reaches[reaches$NextDownID %in% upstr$HydroID, ]
    upstr <- dplyr::bind_rows(upstr, new_reach)
    upstr <- upstr[!duplicated(upstr$HydroID), ]
  }
  upstr
}
