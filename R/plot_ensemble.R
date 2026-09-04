#' Plot AEME ensemble output
#'
#' @description
#' Plots an ensemble produced by [run_aeme_ensemble()] as a quantile ribbon
#' (`type = "ribbon"`) or as one line per member (`type = "line"`), with the
#' matching lake observations overlaid.
#'
#' `aeme` may be either an `aeme` object carrying `AEME::output()$ens_*` or an
#' [ensemble_summary()] result. Passing a summary skips the per-member
#' extraction, so building one summary and plotting several variables, depths
#' or intervals from it is cheap.
#'
#' @inheritParams AEME::plot_output
#' @inheritParams AEME::get_var
#' @param aeme an `aeme` object with ensemble output, or an
#'   [ensemble_summary()].
#' @param model character; model(s) to plot. Optional when `aeme` is an
#'   [ensemble_summary()].
#' @param conf_int numeric; central interval for the ribbon. Default 0.95.
#'   When `aeme` is a summary the interval's quantiles must have been included
#'   in its `probs`.
#' @param type character; `"ribbon"` (default) or `"line"`.
#'
#' @return ggplot object
#' @export
#'
#' @importFrom ggplot2 ggplot geom_line geom_ribbon geom_point labs ylab theme_bw aes
#' @importFrom AEME observations input
#' @seealso [ensemble_summary()], [run_aeme_ensemble()]
plot_ensemble <- function(aeme, model, var_sim = "HYD_temp", depth = NULL,
                          conf_int = 0.95, type = "ribbon",
                          remove_spin_up = TRUE, add_obs = TRUE,
                          var_lims = NULL) {

  if (!is.character(var_sim)) {
    cli::cli_abort("{.arg var_sim} must be a character vector")
  }
  type <- rlang::arg_match(type, c("ribbon", "line"))

  if (inherits(aeme, "aeme_ensemble_summary")) {
    return(.plot_ensemble_summary(
      aeme, model = if (missing(model)) NULL else model, var_sim = var_sim,
      depth = depth, conf_int = conf_int, type = type, add_obs = add_obs))
  }

  # aeme object: extract once into a summary, then render the same way.
  aeme <- AEME::check_aeme(aeme)
  model <- AEME::check_model(model)
  var_sim <- AEME::check_aeme_vars(var_sim)

  probs <- sort(unique(c((1 - conf_int) / 2, 0.5, 1 - (1 - conf_int) / 2)))
  s <- ensemble_summary(aeme = aeme, model = model, vars_sim = var_sim,
                        depths = depth, probs = probs,
                        remove_spin_up = remove_spin_up,
                        add_obs = add_obs, keep_members = TRUE)
  .plot_ensemble_summary(s, model = model, var_sim = var_sim, depth = depth,
                         conf_int = conf_int, type = type, add_obs = add_obs)
}

#' Render an [ensemble_summary()] to a ggplot. Shared by both `plot_ensemble()`
#' entry points.
#' @noRd
.plot_ensemble_summary <- function(x, model = NULL, var_sim = "HYD_temp",
                                   depth = NULL, conf_int = 0.95,
                                   type = "ribbon", add_obs = TRUE) {

  var_sim <- var_sim[[1]]
  st <- x$stats[x$stats$var_sim == var_sim, , drop = FALSE]
  if (!nrow(st)) {
    cli::cli_abort(c(
      "{.val {var_sim}} is not in this summary.",
      "i" = "It has: {.val {attr(x, 'vars_sim')}}."))
  }

  if (!is.null(model)) {
    disp <- tryCatch(AEME::toggle_models(model, to = "display"),
                     error = function(e) model)
    keep <- st$Model %in% c(model, disp)
    if (any(keep)) st <- st[keep, , drop = FALSE]
  }

  # Depth selection: snap the request to the nearest summarised depth.
  pick <- NULL
  has_dep <- any(!is.na(st$depth))
  if (has_dep && !is.null(depth)) {
    ud <- sort(unique(st$depth[!is.na(st$depth)]))
    pick <- ud[which.min(abs(ud - depth))]
    st <- st[!is.na(st$depth) & st$depth == pick, , drop = FALSE]
  }

  y_lab <- .ens_ylab(var_sim)

  if (type == "ribbon") {
    lo <- .ens_q_name((1 - conf_int) / 2)
    hi <- .ens_q_name(1 - (1 - conf_int) / 2)
    mid <- .ens_q_name(0.5)
    miss <- setdiff(c(lo, hi, mid), names(st))
    if (length(miss)) {
      cli::cli_abort(c(
        "The {.val {conf_int}} interval needs columns {.val {miss}}, which
         this summary does not carry.",
        "i" = "It has quantiles at {.val {attr(x, 'probs') * 100}} %.",
        "i" = "Rebuild {.fn ensemble_summary} with a matching {.arg probs}."))
    }
    st$.lower <- st[[lo]]
    st$.upper <- st[[hi]]
    st$.mid <- st[[mid]]
    p <- ggplot2::ggplot(st) +
      ggplot2::geom_ribbon(ggplot2::aes(x = .data$Date, ymin = .data$.lower,
                                        ymax = .data$.upper, fill = .data$Model),
                           alpha = 0.2) +
      ggplot2::geom_line(ggplot2::aes(x = .data$Date, y = .data$.mid,
                                      color = .data$Model)) +
      ggplot2::ylab(y_lab) + ggplot2::theme_bw()
  } else {
    if (is.null(x$members)) {
      cli::cli_abort(c(
        "{.code type = \"line\"} needs the per-member frame.",
        "i" = "Rebuild {.fn ensemble_summary} with {.code keep_members = TRUE}."))
    }
    mem <- x$members[x$members$var_sim == var_sim, , drop = FALSE]
    if (!is.null(model)) {
      disp <- tryCatch(AEME::toggle_models(model, to = "display"),
                       error = function(e) model)
      keep <- mem$Model %in% c(model, disp)
      if (any(keep)) mem <- mem[keep, , drop = FALSE]
    }
    if (!is.null(pick)) {
      mem <- mem[!is.na(mem$depth) & mem$depth == pick, , drop = FALSE]
    }
    p <- ggplot2::ggplot(mem) +
      ggplot2::geom_line(ggplot2::aes(x = .data$Date, y = .data$value,
                                      color = .data$Model, group = .data$ens),
                         alpha = 0.8) +
      ggplot2::ylab(y_lab) + ggplot2::theme_bw()
  }

  if (add_obs && !is.null(x$obs)) {
    ob <- x$obs[x$obs$var_sim == var_sim, , drop = FALSE]
    if (has_dep && !is.null(pick) && any(!is.na(ob$depth))) {
      ob <- ob[!is.na(ob$depth) & ob$depth == pick, , drop = FALSE]
    }
    if (nrow(ob)) {
      p <- p +
        ggplot2::geom_point(data = ob,
                            ggplot2::aes(x = .data$Date, y = .data$value,
                                         fill = "Obs")) +
        ggplot2::labs(fill = "")
    }
  }

  p
}

#' Parsed y-axis label for a variable, from AEME::key_naming.
#' @noRd
.ens_ylab <- function(var_sim) {
  kn <- AEME::key_naming
  row <- kn[kn$var_aeme == var_sim, , drop = FALSE]
  if (!nrow(row) || is.na(row$name_parse[1]) || !nzchar(row$name_parse[1])) {
    return(var_sim)
  }
  out <- tryCatch(eval(parse(text = row$name_parse[1])),
                  error = function(e) var_sim)
  out
}
