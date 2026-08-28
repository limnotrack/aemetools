#' Read the `pestpp-ies` prior-data-conflict report
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `pestpp-ies` flags an observation as being in *prior-data conflict* when
#' the prior (iteration 0) simulated ensemble cannot bracket the observed
#' value: no combination of parameters within their prior ranges reproduces
#' it. A conflicted observation is usually a modelling finding ("the prior
#' cannot reach the observed hypolimnetic temperatures"), not a nuisance,
#' which is why aemetools leaves `ies_drop_conflicts` off by default and
#' reports the conflicts here instead. Turn it on through `pestpp_options`
#' to have `pestpp-ies` remove them from the update - but only once
#' `noise_sd` is set, or the conflict test is comparing against observed
#' values with no spread and will flag nearly everything. This reader is
#' then the only way to see which observations went.
#'
#' Reads `<case>.pdc.csv`, which `pestpp-ies` writes only when at least one
#' conflict is detected.
#'
#' @inheritParams read_pest_phi
#'
#' @return A dataframe: the columns of `<case>.pdc.csv` (observation name
#'   plus the solver's per-observation statistics), left-joined to
#'   `var_aeme`, `Date` and `depth` from the observation map. A zero-row
#'   dataframe when no conflict file was written.
#' @seealso [read_pest_phi()], [pest_residuals()]
#' @export
pest_prior_data_conflict <- function(ctrl) {

  ctrl <- .pest_locate(ctrl)

  f <- file.path(ctrl$pest_dir, paste0(ctrl$case, ".pdc.csv"))
  if (!file.exists(f)) {
    alt <- list.files(ctrl$pest_dir, pattern = "\\.pdc\\.csv$",
                      full.names = TRUE)
    f <- if (length(alt) > 0) alt[[1]] else f
  }
  if (!file.exists(f)) {
    AEME::cli_safe(
      paste0("No prior-data-conflict file in {.file ", ctrl$pest_dir, "}. ",
             "{.val pestpp-ies} writes {.file ", ctrl$case, ".pdc.csv} only ",
             "when a conflict is detected."),
      FUN = cli::cli_alert_info)
    return(data.frame())
  }

  pdc <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(pdc) == 0) return(pdc)

  # First column is the observation name under every pestpp-ies release seen;
  # normalise its name and case so it joins the map.
  names(pdc)[1] <- "obsnme"
  pdc$obsnme <- tolower(trimws(as.character(pdc$obsnme)))

  map <- .pest_obs_map(ctrl)
  if (!is.null(map)) {
    idx <- match(pdc$obsnme, map$obsnme)
    pdc$var_aeme <- map$var_aeme[idx]
    pdc$Date <- as.Date(map$Date[idx])
    pdc$depth <- map$depth[idx]
  }
  pdc
}

#' Read the `pestpp-ies` per-group objective-function trajectory
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' \code{\link{read_pest_phi}} gives the total phi per iteration;
#' `<case>.phi.group.csv` breaks that down by observation group. Since
#' \code{\link{pest_obs_table}} puts one group per `vars_sim` variable, this
#' shows whether - say - temperature is still improving while dissolved
#' oxygen has stalled, which the aggregate hides.
#'
#' @inheritParams read_pest_phi
#'
#' @return A long dataframe: `iteration`, `total_runs`, `obgnme`, `var_aeme`
#'   and `phi` (the group's mean contribution across the ensemble at that
#'   iteration).
#' @seealso [read_pest_phi()], [plot_pest_phi_group()]
#' @export
read_pest_phi_group <- function(ctrl) {

  ctrl <- .pest_locate(ctrl)
  f <- file.path(ctrl$pest_dir, paste0(ctrl$case, ".phi.group.csv"))
  if (!file.exists(f)) {
    cli::cli_abort(c("No group phi file at {.file {f}}.",
                     "i" = "{.val pestpp-ies} writes it once per iteration."))
  }

  wide <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  meta <- intersect(c("iteration", "total_runs"), names(wide))
  grp_cols <- setdiff(names(wide), c(meta, "mean", "standard_deviation",
                                     "min", "max"))
  if (length(grp_cols) == 0) {
    cli::cli_abort("No group columns in {.file {f}}.")
  }

  long <- tidyr::pivot_longer(wide, cols = dplyr::all_of(grp_cols),
                              names_to = "obgnme", values_to = "phi")

  # Group names are .pest_safe_name(var_aeme); invert that against the map.
  map <- .pest_obs_map(ctrl)
  if (!is.null(map)) {
    vars <- unique(map$var_aeme)
    long$var_aeme <- vars[match(tolower(long$obgnme), .pest_safe_name(vars))]
  } else {
    long$var_aeme <- long$obgnme
  }

  keep <- intersect(c(meta, "obgnme", "var_aeme", "phi"), names(long))
  as.data.frame(long[, keep])
}

#' Rebalance observation weights from a completed run
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' After a first `pestpp-ies` run, rescale each observation group's weights
#' so that its contribution to phi is on the order of the number of
#' (non-zero-weight) observations in the group - the "discrepancy" rule from
#' `pyemu.Pst.adjust_weights_discrepancy()`. A group the model fits far
#' better than its weights imply gets down-weighted, and vice versa, so a
#' second run balances effort across variables instead of chasing whichever
#' group happened to start with the largest misfit.
#'
#' @param obs_tbl dataframe; from \code{\link{pest_obs_table}}, carrying its
#'   `map` attribute. Its `weight` column is what gets rescaled.
#' @param calib The completed run - as accepted by \code{\link{read_pest_phi}}.
#' @param mode Character. Only `"discrepancy"` is implemented.
#' @param iteration Integer or `NULL`. Which iteration's simulated ensemble
#'   to measure the misfit at. `NULL` (default) uses the last (posterior).
#'
#' @return `obs_tbl` with a rescaled `weight` column and its `map`
#'   attribute preserved, ready to pass to a second \code{\link{calib_aeme}}.
#' @seealso [pest_obs_table()], [read_pest_phi_group()]
#' @export
pest_adjust_weights <- function(obs_tbl, calib, mode = "discrepancy",
                                iteration = NULL) {

  mode <- rlang::arg_match(mode, "discrepancy")
  if (is.null(attr(obs_tbl, "map"))) {
    cli::cli_abort("{.arg obs_tbl} must carry the {.field map} attribute from
                   {.fn pest_obs_table}.")
  }

  ctrl <- .pest_locate(calib)
  sim <- read_pest_ensemble(ctrl, iteration = iteration, type = "obs")

  ov  <- stats::setNames(obs_tbl$obsval, obs_tbl$obsnme)
  wt  <- stats::setNames(obs_tbl$weight, obs_tbl$obsnme)
  grp <- stats::setNames(obs_tbl$obgnme, obs_tbl$obsnme)

  d <- sim[!sim$is_base, , drop = FALSE]
  d$obs    <- ov[d$obsnme]
  d$weight <- wt[d$obsnme]
  d$obgnme <- grp[d$obsnme]
  d$wr2    <- (d$weight * (d$model - d$obs))^2

  # Mean group phi across realisations.
  gsum <- d |>
    dplyr::group_by(obgnme, realisation) |>
    dplyr::summarise(phi = sum(wr2, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(obgnme) |>
    dplyr::summarise(mean_phi = mean(phi, na.rm = TRUE), .groups = "drop")

  nnz <- obs_tbl[obs_tbl$weight > 0, , drop = FALSE] |>
    dplyr::count(obgnme, name = "n_nnz")

  adj <- dplyr::left_join(gsum, nnz, by = "obgnme")
  adj$mult <- ifelse(
    is.na(adj$n_nnz) | adj$n_nnz == 0 | !is.finite(adj$mean_phi) |
      adj$mean_phi <= 0,
    1, sqrt(adj$n_nnz / adj$mean_phi))

  m <- stats::setNames(adj$mult, adj$obgnme)
  out <- obs_tbl
  fac <- m[out$obgnme]
  fac[is.na(fac)] <- 1
  out$weight <- out$weight * fac
  attr(out, "map") <- attr(obs_tbl, "map")

  chg <- adj[adj$mult != 1, ]
  if (nrow(chg) > 0) {
    AEME::cli_safe(
      paste0("Rescaled weights: ",
             paste(sprintf("%s x%.2f", chg$obgnme, chg$mult), collapse = ", "),
             "."),
      FUN = cli::cli_alert_info)
  }
  out
}
