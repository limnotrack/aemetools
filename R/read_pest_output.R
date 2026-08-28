#' Read the PEST++ objective-function trajectory
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Reads `<case>.phi.actual.csv`, which `L2PhiHandler::write()` appends one
#' row to per iteration. This is how a run's convergence is judged: phi is
#' the weighted sum of squared residuals, so a falling `min`/`mean` means
#' the ensemble is fitting the observations better, and a flattening
#' trajectory means further iterations are not buying anything.
#'
#' @param ctrl The PEST++ run to read. Usually the object returned by
#'   \code{\link{read_calib}}, which carries the resolved run directory
#'   in its metadata. A directory path also works, as does a
#'   \code{\link{create_pest_control}} object when `pest_dir` was given
#'   as an absolute path - a relative `pest_dir` is resolved against the
#'   lake directory when the run starts, so the control on its own does
#'   not know where the files ended up.
#' @param type Character. Which phi to read. `"actual"` (default) is
#'   measured against the observed values; `"meas"` is against the noise
#'   realisations each ensemble member was assigned; `"composite"` adds the
#'   regularisation term when `ies_reg_factor` is non-zero.
#'
#' @return A dataframe with `iteration`, `total_runs`, `mean`,
#'   `standard_deviation`, `min` and `max`, plus one column per
#'   realisation.
#' @seealso [plot_pest_phi()], [read_pest_ensemble()]
#' @export
read_pest_phi <- function(ctrl, type = "actual") {

  type <- rlang::arg_match(type, c("actual", "meas", "composite"))
  ctrl <- .pest_locate(ctrl)
  f <- file.path(ctrl$pest_dir,
                 paste0(ctrl$case, ".phi.", type, ".csv"))
  if (!file.exists(f)) {
    cli::cli_abort(c("No phi file at {.file {f}}.",
                     "i" = "Has the solver run, and is {.arg pest_dir} the
                            directory it ran in?"))
  }
  utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Read a PEST++ parameter or observation ensemble
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Reads one of the ensembles the solver writes per iteration, translating
#' the synthetic PEST names back to aemetools identities: parameters become
#' `name_full` (`group/name[index]`), observations become their
#' `var_aeme`, `Date` and `depth`.
#'
#' The **final** parameter ensemble is the posterior - the product of an
#' iterative ensemble smoother run, and what you push back through the
#' model to get predictive uncertainty. Iteration `0` is the prior, so
#' reading both gives the prior-to-posterior comparison that shows which
#' parameters the observations actually informed.
#'
#' @inheritParams read_pest_phi
#' @param iteration Integer. Which iteration to read. `NULL` (the default)
#'   reads the last one written, i.e. the posterior.
#' @param type Character. `"par"` for the parameter ensemble, `"obs"` for
#'   the simulated observation ensemble.
#'
#' @return A long dataframe with an `is_base` flag marking the `base`
#'   realisation (the initial parameter values, carried through the run by
#'   `ies_include_base`). For `"par"`: `realisation`, `iteration`, `is_base`,
#'   `name_full`, `value`. For `"obs"`: `realisation`, `iteration`,
#'   `is_base`, `obsnme`, `var_aeme`, `Date`, `depth`, `model`.
#' @seealso [pest_param_summary()], [pest_residuals()]
#' @export
read_pest_ensemble <- function(ctrl, iteration = NULL, type = "par") {

  type <- rlang::arg_match(type, c("par", "obs"))
  ctrl <- .pest_locate(ctrl)
  ef <- .pest_ensemble_files(ctrl$pest_dir, ctrl$case, type)
  if (nrow(ef) == 0) {
    cli::cli_abort(c(
      "No {.val {type}} ensembles in {.file {ctrl$pest_dir}}.",
      "i" = "{.val pestpp-ies} writes these; {.val pestpp-glm} does not."
    ))
  }

  iters <- ef$iteration
  it <- iteration %||% max(iters)
  if (!it %in% iters) {
    cli::cli_abort("No iteration {.val {it}}; available: {.val {sort(iters)}}.")
  }
  f <- ef$path[match(it, iters)]

  ens <- .pest_read_ens_file(f)
  real_col <- names(ens)[1]

  long <- tidyr::pivot_longer(ens, cols = -dplyr::all_of(real_col),
                              names_to = "pest_name", values_to = "value") |>
    dplyr::rename(realisation = dplyr::all_of(real_col)) |>
    dplyr::mutate(iteration = it,
                  is_base = tolower(as.character(realisation)) == "base")

  if (type == "par") {
    map <- .pest_par_map(ctrl)
    if (is.null(map)) {
      cli::cli_abort("Missing {.file {paste0(ctrl$case, '_par_map.csv')}}.")
    }
    return(long |>
             dplyr::mutate(name_full = map$name_full[match(pest_name,
                                                           map$parnme)]) |>
             dplyr::select(realisation, iteration, is_base, name_full, value) |>
             as.data.frame())
  }

  map <- .pest_obs_map(ctrl)
  if (is.null(map)) {
    cli::cli_abort("Missing {.file {paste0(ctrl$case, '_obs_map.csv')}}.")
  }
  idx <- match(long$pest_name, map$obsnme)
  long |>
    dplyr::mutate(obsnme = pest_name, var_aeme = map$var_aeme[idx],
                  Date = as.Date(map$Date[idx]), depth = map$depth[idx],
                  model = value) |>
    dplyr::select(realisation, iteration, is_base, obsnme, var_aeme, Date,
                  depth, model) |>
    as.data.frame()
}

#' Compare prior and posterior parameter distributions
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The question a calibration run is really answering is not "what is the
#' best value" but "which parameters did the data actually constrain".
#' This summarises each parameter's prior (iteration 0) and posterior
#' (final iteration) ensembles side by side and reports the **variance
#' reduction**, `1 - sd_post / sd_prior`.
#'
#' A value near 1 means the observations pinned that parameter down; near 0
#' means the posterior is as wide as the prior and the data said nothing
#' about it - a parameter you could fix, or need different observations to
#' inform. Negative values (posterior wider than prior) usually indicate a
#' poorly posed problem.
#'
#' `bound_frac` reports the posterior spread as a fraction of the
#' parameter's `[min, max]` range, so a parameter whose posterior is
#' pressed against a bound is visible. `post_base` is the posterior value of
#' the `base` realisation - where the initial (often hand-calibrated)
#' parameter set ended up - or `NA` when the run carried no base.
#'
#' @inheritParams read_pest_phi
#' @param param dataframe; the calibrated parameters, for bounds.
#'
#' @return A dataframe, one row per parameter, ordered by decreasing
#'   variance reduction.
#' @seealso [plot_pest_ensemble()]
#' @export
pest_param_summary <- function(ctrl, param) {

  ctrl <- .pest_locate(ctrl)
  if (!"name_full" %in% names(param)) {
    param$name_full <- encode_param(param$group, param$name, param$index)
  }

  prior <- read_pest_ensemble(ctrl, iteration = 0, type = "par")
  post <- read_pest_ensemble(ctrl, type = "par")

  stat <- function(df, tag) {
    df |>
      dplyr::group_by(name_full) |>
      dplyr::summarise(
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        sd = stats::sd(value, na.rm = TRUE),
        min = min(value, na.rm = TRUE),
        max = max(value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      stats::setNames(c("name_full", paste0(tag, "_",
                                            c("n", "mean", "sd", "min",
                                              "max"))))
  }

  base <- post[post$is_base, c("name_full", "value")]
  names(base)[2] <- "post_base"

  out <- dplyr::left_join(stat(prior, "prior"), stat(post, "post"),
                          by = "name_full") |>
    dplyr::left_join(param[, c("name_full", "min", "max")], by = "name_full") |>
    dplyr::left_join(base, by = "name_full") |>
    dplyr::mutate(
      # Guard a zero prior spread, which would make the ratio undefined
      # rather than merely uninformative.
      variance_reduction = ifelse(prior_sd > 0, 1 - (post_sd / prior_sd),
                                  NA_real_),
      bound_frac = ifelse((max - min) > 0, (post_max - post_min) / (max - min),
                          NA_real_)
    ) |>
    dplyr::arrange(dplyr::desc(variance_reduction))

  if (!"post_base" %in% names(out)) out$post_base <- NA_real_
  as.data.frame(out)
}

#' Posterior residuals per observation
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Joins the simulated observation ensemble to the observed values, giving
#' one row per realisation per observation. This is the diagnostic layer
#' that says *which* observations, variables, dates or depths are driving
#' the misfit - the question the aggregate fit value cannot answer.
#'
#' @inheritParams read_pest_ensemble
#' @param obs_tbl dataframe; from \code{\link{pest_obs_table}}. When
#'   missing, the observed values are read from the control file written
#'   beside the case.
#'
#' @return A dataframe with `realisation`, `var_aeme`, `Date`, `depth`,
#'   `obs`, `model` and `residual`.
#' @seealso [plot_pest_residuals()]
#' @export
pest_residuals <- function(calib, iteration = NULL, obs_tbl = NULL) {

  ctrl <- .pest_locate(calib)
  na_value <- resolve_na_value(na_value = calib$na_value, calib = calib)
  sim <- read_pest_ensemble(ctrl, iteration = iteration, type = "obs")

  obsval <- if (!is.null(obs_tbl)) {
    stats::setNames(obs_tbl$obsval, obs_tbl$obsnme)
  } else {
    .pest_read_pst_obs(ctrl)
  }
  if (is.null(obsval)) {
    cli::cli_abort(c("Could not recover observed values.",
                     "i" = "Pass {.arg obs_tbl} from {.fn pest_obs_table}."))
  }

  sim |>
    dplyr::mutate(obs = unname(obsval[obsnme]),
                  residual = model - obs) |>
    dplyr::select(realisation, iteration, var_aeme, Date, depth, obs, model,
                  residual) |>
    as.data.frame()
}

# Internal helpers -------------------------------------------------------

#' @noRd
.pest_obs_map <- function(ctrl) {
  f <- file.path(ctrl$pest_dir, paste0(ctrl$case, "_obs_map.csv"))
  if (!file.exists(f)) return(NULL)
  utils::read.csv(f, stringsAsFactors = FALSE)
}

#' List per-iteration ensemble files, CSV or binary.
#'
#' `pestpp-ies` writes `<case>.<n>.<type>.csv` normally and
#' `<case>.<n>.<type>.jcb` (a PEST binary matrix) when `ies_save_binary` is
#' on. When both are present for an iteration the CSV is kept.
#'
#' @return A dataframe of `path`, `iteration` and `ext`, ordered by
#'   iteration; zero rows when nothing matches.
#' @noRd
.pest_ensemble_files <- function(dir, case, type = c("par", "obs")) {
  type <- match.arg(type)
  pat <- paste0("^", case, "\\.(\\d+)\\.", type, "\\.(csv|jcb|jco|bin)$")
  files <- list.files(dir, pattern = pat, full.names = TRUE)
  if (length(files) == 0) {
    return(data.frame(path = character(), iteration = integer(),
                      ext = character(), stringsAsFactors = FALSE))
  }
  bn <- basename(files)
  df <- data.frame(path = files,
                   iteration = as.integer(sub(pat, "\\1", bn)),
                   ext = sub(pat, "\\2", bn),
                   stringsAsFactors = FALSE)
  df <- df[order(df$iteration, df$ext != "csv"), , drop = FALSE]
  df[!duplicated(df$iteration), , drop = FALSE]
}

#' Read one ensemble file - CSV or PEST binary - to a plain dataframe whose
#' first column is the realisation name.
#' @noRd
.pest_read_ens_file <- function(f) {
  if (grepl("\\.csv$", f, ignore.case = TRUE)) {
    return(utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE))
  }
  .pest_read_ensemble_bin(f)
}

#' Read a PEST/PEST++ binary matrix (ensemble) file.
#'
#' Mirrors `pyemu.Matrix.read_binary`. The three little-endian int32 header
#' words select the layout:
#'
#' * `(0, -ncol, -ncol)` - the "dense" format `pestpp-ies` writes for
#'   ensembles: per-column name lengths, the column names, then for each row
#'   a name-length, the name, and `ncol` doubles.
#' * `(-ncol, -nrow, nnz)` - classic sparse: `nnz` records of
#'   `(int32 linear-index, double)` with a 1-based column-major index, then
#'   12-char column names and 20-char row names.
#' * `(ncol, nrow, nnz)` with non-negative words - newer COO sparse: `nnz`
#'   records of `(int32 i, int32 j, double)` (0-based), then 200-char names.
#'
#' Rows are realisations, columns are parameters or observations.
#'
#' @return A dataframe: `real_name` then one column per matrix column,
#'   matching the shape of the CSV ensemble readers.
#' @noRd
.pest_read_ensemble_bin <- function(file) {

  con <- file(file, "rb")
  on.exit(close(con))

  hdr <- readBin(con, "integer", n = 3L, size = 4L, endian = "little")
  if (length(hdr) < 3L) {
    cli::cli_abort("{.file {file}} is too short to be a PEST binary matrix.")
  }
  itemp1 <- hdr[1]; itemp2 <- hdr[2]; icount <- hdr[3]

  rd_name <- function(nbytes) {
    raw <- readBin(con, "raw", n = nbytes)
    trimws(rawToChar(raw[raw != as.raw(0L)]))
  }

  if (itemp1 == 0L && itemp2 == icount) {                       # dense
    ncol <- abs(itemp2)
    col_slens <- readBin(con, "integer", n = ncol, size = 4L, endian = "little")
    col_names <- vapply(col_slens, rd_name, character(1))
    row_names <- character(0)
    rows <- list()
    repeat {
      slen <- readBin(con, "integer", n = 1L, size = 4L, endian = "little")
      if (length(slen) == 0L) break
      rn <- rd_name(slen)
      vals <- readBin(con, "double", n = ncol, size = 8L, endian = "little")
      if (length(vals) != ncol) break
      row_names <- c(row_names, rn)
      rows[[length(rows) + 1L]] <- vals
    }
    m <- matrix(unlist(rows), nrow = length(rows), ncol = ncol, byrow = TRUE)

  } else if (itemp1 < 0L) {                                     # classic sparse
    ncol <- abs(itemp1); nr <- abs(itemp2)
    m <- matrix(0, nr, ncol)
    for (k in seq_len(icount)) {
      j <- readBin(con, "integer", n = 1L, size = 4L, endian = "little")
      v <- readBin(con, "double", n = 1L, size = 8L, endian = "little")
      icol <- ((j - 1L) %/% nr) + 1L
      irow <- j - ((icol - 1L) * nr)
      m[irow, icol] <- v
    }
    col_names <- vapply(seq_len(ncol), function(i) rd_name(12L), character(1))
    row_names <- vapply(seq_len(nr), function(i) rd_name(20L), character(1))

  } else {                                                      # newer COO sparse
    ncol <- abs(itemp1); nr <- abs(itemp2)
    m <- matrix(0, nr, ncol)
    for (k in seq_len(icount)) {
      ij <- readBin(con, "integer", n = 2L, size = 4L, endian = "little")
      v <- readBin(con, "double", n = 1L, size = 8L, endian = "little")
      m[ij[1] + 1L, ij[2] + 1L] <- v
    }
    col_names <- vapply(seq_len(ncol), function(i) rd_name(200L), character(1))
    row_names <- vapply(seq_len(nr), function(i) rd_name(200L), character(1))
  }

  colnames(m) <- tolower(col_names)
  data.frame(real_name = tolower(row_names), m, check.names = FALSE,
             stringsAsFactors = FALSE)
}

#' Recover observed values from the `* observation data` section of the
#' control file, so residuals can be computed from the run directory alone.
#' @noRd
.pest_read_pst_obs <- function(ctrl) {
  f <- file.path(ctrl$pest_dir, paste0(ctrl$case, ".pst"))
  if (!file.exists(f)) return(NULL)
  l <- readLines(f, warn = FALSE)
  start <- which(l == "* observation data")
  if (length(start) == 0) return(NULL)
  ends <- grep("^\\* ", l)
  stop_at <- ends[ends > start][1]
  rows <- l[(start + 1):(if (is.na(stop_at)) length(l) else stop_at - 1)]
  rows <- rows[nzchar(trimws(rows))]
  parts <- strsplit(trimws(rows), "\\s+")
  stats::setNames(as.numeric(vapply(parts, `[`, character(1), 2)),
                  vapply(parts, `[`, character(1), 1))
}
