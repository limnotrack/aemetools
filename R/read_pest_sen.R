#' Read `pestpp-sen` Method of Morris sensitivity indices
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Parses the Morris sensitivity summary written by `pestpp-sen` into a tidy
#' dataframe, translating the synthetic PEST parameter names (`p001`, ...)
#' back to aemetools `name_full` identities and the observation groups back
#' to the `names(ctrl$vars_sim)` sub-regions.
#'
#' `pyemu` ships no parser for these files. `pestpp-sen` with
#' `gsa_morris_obs_sen` writes several: `<case>.group.msn` (per observation
#' group), `<case>.mio` (per observation), `<case>.mos` (per observation,
#' including a range-scaled effect), and `<case>.msn` (pooled over all
#' observations). The most disaggregated one available is used, in that
#' order, falling back to the pooled file with `variable = "all"`. Columns
#' are matched on name patterns rather than fixed positions.
#'
#' Raw elementary effects have the units of the response divided by the
#' parameter's units, so a parameter with a wide `[min, max]` produces a
#' `mu_star` orders of magnitude larger than a tightly-bounded one. When
#' `<case>.mos` is present its `scaled_sen` column (effect scaled by the
#' parameter range) is also returned, as `index_type = "scaled_sen"`, and is
#' the better basis for ranking across parameters.
#'
#' @param ctrl A \code{\link{create_sen_control}} object with its `pest_dir`
#'   resolved (as inside \code{\link{sa_aeme}}), or a path to the run
#'   directory.
#' @param param dataframe; the parameters that were analysed.
#' @param vars_sim character; unused, kept for signature symmetry with the
#'   other readers.
#' @param model character; model name, used only to abbreviate parameter
#'   labels.
#'
#' @return A long dataframe with columns `model`, `variable`, `parameter`,
#'   `label`, `index_type` (`"mu_star"`, `"mu"`, `"sigma"`, and
#'   `"scaled_sen"` when `<case>.mos` is present), `value`, `low_ci`,
#'   `high_ci`. Zero rows (with a warning) when no recognised file is found.
#' @seealso \code{\link{read_sen}}, \code{\link{plot_sen}},
#'   \code{\link{create_sen_control}}
#' @export
read_pest_sen_indices <- function(ctrl, param, vars_sim = NULL, model = NULL) {

  loc <- .pest_locate(ctrl)
  dir <- loc$pest_dir
  case <- loc$case

  if (!"name_full" %in% names(param)) {
    param$name_full <- encode_param(param$group, param$name, param$index)
  }
  par_map <- .pest_par_map(list(pest_dir = dir, case = case))
  obs_map <- .pest_obs_map(list(pest_dir = dir, case = case))

  idx <- .sen_read_morris(dir = dir, case = case, par_map = par_map,
                          obs_map = obs_map)
  idx <- rbind(idx, .sen_read_mos(dir = dir, case = case, par_map = par_map,
                                  obs_map = obs_map))

  if (is.null(idx) || nrow(idx) == 0) {
    AEME::cli_safe(
      paste0("No Morris sensitivity output ({.file ", case,
             ".group.msn} / {.file ", case, ".mio} / {.file ", case,
             ".msn}) found in {.file ", dir, "}."),
      FUN = cli::cli_alert_warning
    )
    return(.sen_empty())
  }

  idx$model <- model %||% NA_character_
  idx$label <- if (!is.null(model)) {
    abbrev_pars(idx$parameter, model)
  } else {
    idx$parameter
  }
  idx[, c("model", "variable", "parameter", "label", "index_type",
          "value", "low_ci", "high_ci")]
}

# Internal helpers -----------------------------------------------------------

#' @noRd
.sen_empty <- function() {
  data.frame(model = character(), variable = character(),
             parameter = character(), label = character(),
             index_type = character(), value = numeric(),
             low_ci = numeric(), high_ci = numeric(),
             stringsAsFactors = FALSE)
}

#' Map a vector of PEST `parnme` values back to `name_full`.
#' @noRd
.sen_map_par <- function(x, par_map) {
  x <- as.character(x)
  if (is.null(par_map)) return(x)
  hit <- match(tolower(x), tolower(par_map$parnme))
  ifelse(is.na(hit), x, par_map$name_full[hit])
}

#' Map a vector of observation-group names back to the `names(ctrl$vars_sim)`
#' sub-region. Groups PEST++ writes for a pooled run ("all"/"") fall through
#' unchanged.
#' @noRd
.sen_map_grp <- function(x, obs_map) {
  x <- as.character(x)
  if (is.null(obs_map) || !"name" %in% names(obs_map)) return(x)
  hit <- match(tolower(x), tolower(obs_map$obsnme))
  ifelse(is.na(hit), x, obs_map$name[hit])
}

#' Pick the first column whose (lower-cased, punctuation-stripped) name
#' matches any of `patterns`. Returns `NA` when none match.
#' @noRd
.sen_pick_col <- function(nms, patterns) {
  key <- gsub("[^a-z0-9]", "", tolower(nms))
  for (p in patterns) {
    j <- which(grepl(p, key))
    if (length(j) > 0) return(nms[j[1]])
  }
  NA_character_
}

#' Column-name patterns for the parameter column. Deliberately strict: a
#' bare `name$` would also match `observation_name`.
#' @noRd
.sen_par_patterns <- c("^parnme$", "^parametername$", "^parname$",
                       "^parameter$", "^par$")

#' Column-name patterns for the observation / observation-group column.
#' @noRd
.sen_grp_patterns <- c("^obsgroupname$", "^observationgroupname$",
                       "^obsgroup$", "^observationgroup$", "^obgnme$",
                       "^groupname$", "^group$", "^observationname$",
                       "^observation$", "^obsname$", "^obsnme$", "^obs$")

#' Read the most disaggregated Morris summary available.
#'
#' Preference: `<case>.group.msn` (per observation group) then `<case>.mio`
#' (per observation) then `<case>.msn` (pooled, `variable = "all"`).
#'
#' @return A dataframe with `variable`, `parameter`, `index_type`
#'   (`mu_star` / `mu` / `sigma`), `value`, `low_ci`, `high_ci`, or `NULL`.
#' @noRd
.sen_read_morris <- function(dir, case, par_map, obs_map) {

  f <- Find(file.exists, file.path(dir, paste0(case, c(".group.msn", ".mio",
                                                      ".msn"))))
  if (is.null(f)) return(NULL)

  raw <- tryCatch(
    utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE,
                    strip.white = TRUE),
    error = function(e) NULL
  )
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  names(raw) <- trimws(names(raw))

  par_col <- .sen_pick_col(names(raw), .sen_par_patterns)
  if (is.na(par_col)) return(NULL)
  grp_col <- .sen_pick_col(names(raw), .sen_grp_patterns)

  measures <- c(
    mu_star = .sen_pick_col(names(raw), c("^senmeanabs$", "meanabs", "mustar",
                                          "absmean")),
    mu      = .sen_pick_col(names(raw), c("^senmean$", "^mean$", "^mu$")),
    sigma   = .sen_pick_col(names(raw), c("^senstddev$", "stddev", "sigma",
                                          "standarddeviation"))
  )
  measures <- measures[!is.na(measures)]
  if (length(measures) == 0) return(NULL)

  grp <- if (is.na(grp_col)) rep("all", nrow(raw)) else raw[[grp_col]]

  do.call(rbind, lapply(names(measures), function(mt) {
    d <- data.frame(
      variable   = .sen_map_grp(grp, obs_map),
      parameter  = .sen_map_par(raw[[par_col]], par_map),
      index_type = mt,
      value      = suppressWarnings(as.numeric(raw[[measures[[mt]]]])),
      low_ci     = NA_real_,
      high_ci    = NA_real_,
      stringsAsFactors = FALSE
    )
    d[!is.na(d$value), , drop = FALSE]
  }))
}

#' Read the range-scaled sensitivity from `<case>.mos`, if present.
#'
#' `<case>.mos` is per (parameter, observation) and carries `scaled_sen` -
#' the elementary effect scaled by the parameter range - which is
#' comparable across parameters with very different bounds. Its header uses
#' spaces after the commas and upper-case names, hence `strip.white`.
#'
#' @return A dataframe in the same shape as \code{\link{.sen_read_morris}}
#'   with `index_type = "scaled_sen"`, or `NULL`.
#' @noRd
.sen_read_mos <- function(dir, case, par_map, obs_map) {

  f <- file.path(dir, paste0(case, ".mos"))
  if (!file.exists(f)) return(NULL)

  raw <- tryCatch(
    utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE,
                    strip.white = TRUE),
    error = function(e) NULL
  )
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  names(raw) <- trimws(names(raw))

  par_col <- .sen_pick_col(names(raw), .sen_par_patterns)
  grp_col <- .sen_pick_col(names(raw), .sen_grp_patterns)
  val_col <- .sen_pick_col(names(raw), c("scaledsen", "^scaled$"))
  if (is.na(par_col) || is.na(val_col)) return(NULL)

  grp <- if (is.na(grp_col)) rep("all", nrow(raw)) else raw[[grp_col]]
  d <- data.frame(
    variable   = .sen_map_grp(grp, obs_map),
    parameter  = .sen_map_par(raw[[par_col]], par_map),
    index_type = "scaled_sen",
    value      = suppressWarnings(as.numeric(raw[[val_col]])),
    low_ci     = NA_real_,
    high_ci    = NA_real_,
    stringsAsFactors = FALSE
  )
  d[!is.na(d$value), , drop = FALSE]
}
