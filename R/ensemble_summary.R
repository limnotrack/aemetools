#' Summarise an AEME ensemble across members
#'
#' @description
#' [run_aeme_ensemble()] stores every member's full model output on the
#' `aeme` object. Turning that into a plot or a skill score means, each time,
#' looping over the members and pulling the variable out with
#' [AEME::get_var()] - the expensive step. `ensemble_summary()` does that
#' extraction **once** and returns a tidy object holding
#'
#' * `stats` - one row per date (and depth, and model, and variable) with the
#'   ensemble `mean`, `sd`, `n` and the quantiles named in `probs`;
#' * `members` - the extracted long frame the statistics were built from
#'   (`Date`, `depth`, `value`, `Model`, `var_sim`, `ens`), kept unless
#'   `keep_members = FALSE`;
#' * `obs` - the matching lake observations, depth-aligned to `depths`.
#'
#' [plot_ensemble()] accepts the result directly, so a summary built once can
#' be replotted for several variables, depths and intervals for free.
#'
#' @inheritParams plot_ensemble
#' @param vars_sim character; one or more AEME variables to summarise.
#' @param depths numeric; depths (m, referenced to `depth_ref`) at which to
#'   summarise depth-resolved variables. `NULL` (default) pools all layers for
#'   a depth-resolved variable - matching the historical [plot_ensemble()]
#'   behaviour - and is the only sensible value for a 1-D variable such as
#'   `LKE_lvlwtr` or `HYD_thmcln`.
#' @param probs numeric; quantile probabilities for the summary bands. `0.5`
#'   is always added so a median is available.
#' @param depth_ref character; `"surface"` (default) or `"bottom"`, passed to
#'   [AEME::get_var()] and used to align observations.
#' @param obs_tol numeric; half-width (m) of the window used to attach
#'   observations to each requested depth. Default 0.5.
#' @param keep_members logical; keep the extracted per-member frame on the
#'   result. Default `TRUE`. `FALSE` drops it - smaller object, but
#'   `plot_ensemble(type = "line")` and future member-level scores then can't
#'   be computed from it.
#'
#' @return An `aeme_ensemble_summary` object: a list of `stats`, `members`
#'   (or `NULL`) and `obs` (or `NULL`), with `probs`, `depths`, `depth_ref`,
#'   `vars_sim`, `vars_2d`, `n_members` and `period` attributes.
#'
#' @seealso [plot_ensemble()], [score_ensemble()], [run_aeme_ensemble()]
#'
#' @importFrom AEME check_aeme check_model get_var observations output check_aeme_vars
#' @importFrom dplyr bind_rows group_by reframe across all_of mutate
#' @importFrom tidyr pivot_wider
#' @importFrom stats sd quantile
#' @export
ensemble_summary <- function(aeme, model, vars_sim = "HYD_temp", depths = NULL,
                             probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                             remove_spin_up = TRUE, add_obs = TRUE,
                             depth_ref = c("surface", "bottom"), obs_tol = 0.5,
                             keep_members = TRUE) {

  aeme <- AEME::check_aeme(aeme)
  model <- AEME::check_model(model)
  depth_ref <- rlang::arg_match(depth_ref)

  vars_sim <- vapply(vars_sim, function(v) AEME::check_aeme_vars(v),
                     character(1), USE.NAMES = FALSE)
  probs <- sort(unique(c(as.numeric(probs), 0.5)))
  if (any(probs <= 0 | probs >= 1)) {
    cli::cli_abort("{.arg probs} must lie strictly between 0 and 1.")
  }
  if (!is.null(depths)) depths <- sort(unique(as.numeric(depths)))

  outp <- AEME::output(aeme)
  n_members <- outp$n_members
  if (is.null(n_members) || n_members < 1L) {
    cli::cli_abort(c("No ensemble output found on {.arg aeme}.",
                     "i" = "Run {.fn run_aeme_ensemble} first."))
  }
  ens1 <- paste0("ens_", sprintf("%03d", 1L))

  q_names <- .ens_q_name(probs)

  # ---- per-variable extraction + summary ---------------------------------
  vars_2d <- character(0)
  per_var <- lapply(vars_sim, function(v) {

    raw <- outp[[ens1]][[model[[1]]]][[v]]
    is_2d <- !is.null(raw) && !is.null(dim(raw)) &&
      !inherits(raw, "aeme_grouped_var")
    if (is_2d) vars_2d[[length(vars_2d) + 1L]] <<- v

    by_depth <- is_2d && !is.null(depths)
    if (is_2d && is.null(depths)) {
      cli::cli_warn(c(
        "!" = "{.val {v}} is depth-resolved but {.arg depths} is {.code NULL};
               pooling all layers.",
        "i" = "Pass {.arg depths} for per-depth bands."))
    }

    members <- dplyr::bind_rows(lapply(seq_len(n_members), function(i) {
      one <- if (by_depth) {
        dplyr::bind_rows(lapply(depths, function(d) {
          AEME::get_var(aeme = aeme, model = model, var_sim = v, depth = d,
                        depth_ref = depth_ref, ens_n = i, return_df = TRUE,
                        remove_spin_up = remove_spin_up, cumulative = FALSE)
        }))
      } else {
        AEME::get_var(aeme = aeme, model = model, var_sim = v, depth = NULL,
                      depth_ref = depth_ref, ens_n = i, return_df = TRUE,
                      remove_spin_up = remove_spin_up, cumulative = FALSE)
      }
      one$ens <- i
      one
    }))
    members$var_sim <- v

    keys <- c("Date", "Model", "var_sim", if (by_depth) "depth")
    stats <- dplyr::reframe(
      dplyr::group_by(members, dplyr::across(dplyr::all_of(keys))),
      n = sum(!is.na(.data$value)),
      mean = mean(.data$value, na.rm = TRUE),
      sd = stats::sd(.data$value, na.rm = TRUE),
      .stat = q_names,
      .q = stats::quantile(.data$value, probs = probs, na.rm = TRUE,
                           names = FALSE))
    stats <- tidyr::pivot_wider(stats, names_from = ".stat",
                                values_from = ".q")
    if (!by_depth) stats$depth <- NA_real_

    list(members = members, stats = stats)
  })

  members <- dplyr::bind_rows(lapply(per_var, `[[`, "members"))
  stats <- dplyr::bind_rows(lapply(per_var, `[[`, "stats"))
  period <- as.Date(range(members$Date, na.rm = TRUE))

  # ---- observations -----------------------------------------------------
  obs_df <- .ens_obs(aeme, vars_sim, vars_2d, depths, obs_tol, period,
                     enabled = add_obs)

  structure(
    list(stats = stats,
         members = if (keep_members) members else NULL,
         obs = obs_df),
    class = "aeme_ensemble_summary",
    probs = probs, q_names = q_names, depths = depths, depth_ref = depth_ref,
    vars_sim = vars_sim, vars_2d = vars_2d, n_members = n_members,
    keep_members = keep_members, period = period)
}

#' Quantile-column name for a probability, e.g. 0.025 -> "q2.5".
#' @noRd
.ens_q_name <- function(p) {
  paste0("q", sub("\\.?0+$", "", sprintf("%.4f", p * 100)))
}

#' Depth-aligned lake observations for [ensemble_summary()].
#' @noRd
.ens_obs <- function(aeme, vars_sim, vars_2d, depths, obs_tol, period,
                     enabled = TRUE) {
  if (!enabled) return(NULL)
  obs <- AEME::observations(aeme)
  if (is.null(obs$lake)) return(NULL)
  lk <- obs$lake[obs$lake$var_aeme %in% vars_sim &
                   obs$lake$Date >= period[1] & obs$lake$Date <= period[2], ,
                 drop = FALSE]
  if (!nrow(lk)) return(NULL)
  lk$depth_mid <- (lk$depth_from + lk$depth_to) / 2

  out <- lapply(vars_sim, function(v) {
    s <- lk[lk$var_aeme == v, , drop = FALSE]
    if (!nrow(s)) return(NULL)
    if (!is.null(depths) && v %in% vars_2d) {
      dplyr::bind_rows(lapply(depths, function(d) {
        hit <- s[abs(s$depth_mid - d) <= obs_tol, , drop = FALSE]
        if (!nrow(hit)) return(NULL)
        data.frame(Date = hit$Date, depth = d, value = hit$value,
                   var_sim = v, stringsAsFactors = FALSE)
      }))
    } else {
      data.frame(Date = s$Date, depth = NA_real_, value = s$value,
                 var_sim = v, stringsAsFactors = FALSE)
    }
  })
  out <- dplyr::bind_rows(out)
  if (!nrow(out)) NULL else out
}

#' @export
print.aeme_ensemble_summary <- function(x, ...) {
  vs <- attr(x, "vars_sim")
  v2 <- attr(x, "vars_2d")
  dep <- attr(x, "depths")
  per <- attr(x, "period")
  cat("<aeme_ensemble_summary>\n")
  cat(sprintf("  members  : %d %s\n", attr(x, "n_members"),
              if (isTRUE(attr(x, "keep_members"))) "(kept)" else "(dropped)"))
  cat(sprintf("  variables: %s\n", paste(vs, collapse = ", ")))
  if (length(v2)) {
    cat(sprintf("  depths   : %s (%s-ref)\n",
                if (is.null(dep)) "pooled layers" else
                  paste(format(dep), collapse = ", "),
                attr(x, "depth_ref")))
  }
  cat(sprintf("  period   : %s .. %s  (%d rows in $stats)\n",
              per[1], per[2], nrow(x$stats)))
  cat(sprintf("  quantiles: %s %%\n",
              paste(format(attr(x, "probs") * 100), collapse = ", ")))
  cat(sprintf("  obs      : %s\n",
              if (is.null(x$obs)) "none" else paste(nrow(x$obs), "points")))
  invisible(x)
}

#' @description
#' `as.data.frame()` returns the `stats` table - the long per-date summary,
#' ready to write out or join to something else.
#' @rdname ensemble_summary
#' @param row.names,optional passed to the data.frame method; ignored.
#' @param x an `aeme_ensemble_summary`.
#' @param ... unused.
#' @export
as.data.frame.aeme_ensemble_summary <- function(x, row.names = NULL,
                                                optional = FALSE, ...) {
  as.data.frame(x$stats, row.names = row.names, optional = optional)
}

#' Skill scores for an AEME ensemble
#'
#' @description
#' **Not implemented yet.** Planned: given an [ensemble_summary()] (or an
#' `aeme` carrying ensemble output plus observations), return the standard
#' ensemble verification scores against the lake observations -
#'
#' * coverage - fraction of observations inside the central `conf_int` band;
#' * bias and RMSE of the ensemble mean;
#' * spread-skill ratio - mean ensemble spread over ensemble-mean RMSE;
#' * mean CRPS.
#'
#' resolved per variable and per depth, with an overall row.
#'
#' @param x an [ensemble_summary()] result, or an `aeme` object.
#' @param conf_int numeric; central interval for the coverage score. Default
#'   0.95.
#' @param ... reserved for future arguments.
#'
#' @return (planned) a data.frame of scores, one row per variable/depth.
#' @seealso [ensemble_summary()]
#' @export
score_ensemble <- function(x, conf_int = 0.95, ...) {
  cli::cli_abort(c(
    "{.fn score_ensemble} is not implemented yet.",
    "i" = "Planned: coverage, ensemble-mean bias/RMSE, spread-skill ratio,
           mean CRPS - per variable and depth.",
    "i" = "Build the inputs now with {.fn ensemble_summary}."),
    class = "aemetools_not_implemented")
}
