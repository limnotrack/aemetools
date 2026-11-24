#' Read a `pestpp-sen` sensitivity analysis
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Companion to \code{\link{read_sa}} for runs made with
#' \code{\link{create_sen_control}}. Returns, per `sim_id`, the stored
#' Morris indices plus the raw model runs, in a shape
#' \code{\link{plot_sen}} consumes directly.
#'
#' @inheritParams read_sa
#' @param sim_id character; one or more simulation ids to read.
#'
#' @importFrom tools file_ext
#'
#' @return A named list, one element per `sim_id`, each
#'   `list(indices = <dataframe>, runs = <dataframe>)`.
#' @seealso \code{\link{plot_sen}}, \code{\link{read_pest_sen_indices}},
#'   \code{\link{read_sa}}
#' @export
read_sen <- function(ctrl = NULL, file_name, file_dir, sim_id) {

  if (missing(sim_id) || length(sim_id) == 0) {
    cli::cli_abort("{.arg sim_id} is required.")
  }

  if (is.null(ctrl)) {
    ctrl <- list(file_dir = file_dir, file_name = file_name,
                 file_type = tools::file_ext(file_name), method = "sa")
  }

  out <- read_simulation_output(ctrl = ctrl, sim_id = sim_id, type = "sa")

  if (is.null(out$sensitivity_indices) ||
      nrow(out$sensitivity_indices) == 0) {
    cli::cli_abort(c(
      "No {.field sensitivity_indices} recorded for {.val {sim_id}}.",
      "i" = "Only runs made with {.fn create_sen_control} write this table."
    ))
  }

  stats::setNames(lapply(sim_id, function(sid) {
    list(
      indices = out$sensitivity_indices[
        out$sensitivity_indices$sim_id == sid, , drop = FALSE],
      runs = out$simulation_data[
        out$simulation_data$sim_id == sid, , drop = FALSE]
    )
  }), sim_id)
}
