#' Normalise `calib_aeme()`'s `vars_sim` into its flat AEME-variable form
#' and, when given as a named list of sub-regions, the region spec itself.
#'
#' Mirrors the named-list convention `sa_aeme()` / `create_sen_control()`
#' already use for `vars_sim`: `list(surf_temp = list(var = "HYD_temp",
#' month = c(12, 1, 2), depth_range = c(0, 2)), bot_temp = list(...))`. This
#' lets `calib_aeme()` split a variable's calibration objective into
#' independently-weighted depth/month windows (e.g. surface vs bottom
#' temperature), the same split `sa_aeme()` already uses for Morris
#' screening.
#'
#' @param vars_sim as passed to \code{\link{calib_aeme}}: either a plain
#'   character vector (the existing, flat form) or a fully-named list of
#'   region specs.
#' @return list(flat = unique character vector of AEME variables to
#'   simulate, regions = NULL for the flat form, or the named list - each
#'   element's `weight` defaulted to `1` - for the region form).
#' @noRd
.as_calib_regions <- function(vars_sim) {
  is_named_list <- is.list(vars_sim) && !is.data.frame(vars_sim) &&
    !is.null(names(vars_sim)) && all(nzchar(names(vars_sim)))
  if (!is_named_list) {
    return(list(flat = unique(as.character(vars_sim)), regions = NULL))
  }

  bad <- vapply(vars_sim, function(v) !is.list(v) || is.null(v$var),
                logical(1))
  if (any(bad)) {
    cli::cli_abort("Each element of {.arg vars_sim} must be a list with a
                   {.field var} element, as in {.fn create_sen_control}.")
  }

  # write_simulation_output() long-formats per-region result columns with a
  # `contains("_")` pivot; create_sen_control() enforces the same convention
  # for the identical vars_sim shape.
  no_us <- names(vars_sim)[!grepl("_", names(vars_sim))]
  if (length(no_us) > 0) {
    cli::cli_abort(c(
      "Every {.arg vars_sim} region name must contain an underscore:
       {.val {no_us}}.",
      "i" = "e.g. {.val surf_temp}, {.val bot_temp} - the results writer
             keys on it."
    ))
  }

  if (any(vapply(vars_sim, function(v) identical(v$var, "LKE_lvlwtr"),
                 logical(1)))) {
    cli::cli_abort("{.val LKE_lvlwtr} cannot be used inside a {.arg vars_sim}
                   region - it has no depth axis.")
  }

  vars_sim <- lapply(vars_sim, function(v) {
    if (is.null(v$weight)) v$weight <- 1
    v
  })

  vars <- vapply(vars_sim, function(v) v$var, character(1))
  list(flat = unique(vars), regions = vars_sim)
}

#' Abort if two regions targeting the same underlying variable overlap in
#' both month and depth.
#'
#' An observation matching more than one region would otherwise be silently
#' double-counted (or assigned to whichever region happens to match first),
#' either of which biases the objective without any indication that it
#' happened.
#' @noRd
.check_region_overlap <- function(regions) {
  nmes <- names(regions)
  vars <- vapply(regions, function(v) v$var, character(1))
  for (target in unique(vars)) {
    idx <- which(vars == target)
    if (length(idx) < 2) next
    for (i in seq_along(idx)[-length(idx)]) {
      for (j in seq(i + 1, length(idx))) {
        a <- regions[[idx[i]]]
        b <- regions[[idx[j]]]
        month_overlap <- is.null(a$month) || is.null(b$month) ||
          length(intersect(a$month, b$month)) > 0
        depth_overlap <- is.null(a$depth_range) || is.null(b$depth_range) ||
          (max(a$depth_range) >= min(b$depth_range) &&
             max(b$depth_range) >= min(a$depth_range))
        if (month_overlap && depth_overlap) {
          cli::cli_abort(c(
            "Regions {.val {nmes[idx[i]]}} and {.val {nmes[idx[j]]}} both
             target {.val {target}} and overlap in month and depth.",
            "i" = "Narrow their {.field month}/{.field depth_range} so no
                   observation matches more than one region."
          ))
        }
      }
    }
  }
  invisible(NULL)
}
