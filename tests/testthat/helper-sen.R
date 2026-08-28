# Fixtures for the pestpp-sen (Method of Morris) sensitivity path.
# Kept in a helper- file so test-sa_aeme-pest.R can be run on its own with a
# testthat filter.

#' A small parameter dataframe for the sensitivity tests: three glm_aed
#' parameters, one of them log = TRUE with strictly-positive bounds so the
#' `transform` argument of `pest_param_table()` is exercised.
make_sen_param <- function() {
  data.frame(
    model = "glm_aed",
    file  = c("glm3.nml", "glm3.nml", "met"),
    group = c("light", "light", NA),
    name  = c("Kw", "ce", "MET_tmpair"),
    index = c(1, NA, NA),
    value = c(0.5, 0.9, 1.0),
    min   = c(0.1, 0.2, 0.8),
    max   = c(1.5, 1.6, 1.2),
    log   = c(TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
}

#' A `vars_sim` list with two sub-regions on one AEME variable, as
#' `create_sa_control()` / `create_sen_control()` expect. Names carry an
#' underscore, which the results writer keys on.
make_sen_vars_sim <- function() {
  list(
    surf_temp = list(var = "HYD_temp", month = c(12, 1, 2),
                     depth_range = c(0, 2)),
    bot_temp  = list(var = "HYD_temp", month = c(12, 1, 2),
                     depth_range = c(8, 10))
  )
}

#' Stand in for the per-observation-group Morris summary `pestpp-sen` writes
#' to `<case>.group.msn` (columns observed from pestpp-sen 5.x:
#' `parameter_name,obs_group_name,n_samples,sen_mean,sen_mean_abs,sen_std_dev`).
#' `read_pest_sen_indices()` matches on name patterns, not fixed positions -
#' if a future binary changes the header, update this fixture *and*
#' `.sen_read_morris()`.
write_fake_msn <- function(dir, case, groups, parnmes, seed = 1L) {
  set.seed(seed)
  rows <- do.call(rbind, lapply(groups, function(g) {
    data.frame(
      parameter_name = parnmes,
      obs_group_name = g,
      n_samples      = 2L,
      sen_mean       = round(stats::runif(length(parnmes), -1, 1), 4),
      sen_mean_abs   = round(stats::runif(length(parnmes), 0, 1), 4),
      sen_std_dev    = round(stats::runif(length(parnmes), 0, 0.5), 4),
      stringsAsFactors = FALSE
    )
  }))
  f <- file.path(dir, paste0(case, ".group.msn"))
  utils::write.csv(rows, f, row.names = FALSE)
  f
}

#' Stand in for `<case>.mos` (per parameter/observation, carrying the
#' range-scaled effect `scaled_sen`). pestpp-sen writes this header with
#' spaces after the commas and upper-case names.
write_fake_mos <- function(dir, case, obs, parnmes, seed = 2L) {
  set.seed(seed)
  rows <- do.call(rbind, lapply(obs, function(o) {
    data.frame(
      par_name   = toupper(parnmes),
      n_samples  = 2L,
      obs_name   = toupper(o),
      mean       = round(stats::runif(length(parnmes), -1, 1), 4),
      abs_mean   = round(stats::runif(length(parnmes), 0, 1), 4),
      sigma      = round(stats::runif(length(parnmes), 0, 0.5), 4),
      scaled_sen = round(stats::runif(length(parnmes), 0, 1), 4),
      stringsAsFactors = FALSE
    )
  }))
  f <- file.path(dir, paste0(case, ".mos"))
  utils::write.csv(rows, f, row.names = FALSE)
  f
}

#' Write the `_par_map.csv` / `_obs_map.csv` sidecars that `write_pst()`
#' would have left in the run directory, so the index reader can be tested
#' without a full `write_pst()` call.
write_fake_maps <- function(dir, case, par_tbl, obs_tbl) {
  utils::write.csv(attr(par_tbl, "map"),
                   file.path(dir, paste0(case, "_par_map.csv")),
                   row.names = FALSE)
  utils::write.csv(attr(obs_tbl, "map"),
                   file.path(dir, paste0(case, "_obs_map.csv")),
                   row.names = FALSE)
  invisible(dir)
}
