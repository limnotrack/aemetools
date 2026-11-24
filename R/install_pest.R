#' Install the PEST++ executables
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Downloads a PEST++ release from
#' \url{https://github.com/usgs/pestpp/releases} and unpacks the executables
#' into a per-user data directory so that \code{\link{calib_aeme}} can find
#' them. PEST++ is public-domain software distributed by the USGS; it is *not*
#' bundled with aemetools and is not required unless
#' `ctrl$c_method` is one of the `"PESTPP-*"` methods.
#'
#' @param version Character. Release tag to install, e.g. `"5.2.16"`, or
#'   `"latest"` (the default) to query the GitHub API for the most recent
#'   release.
#' @param dir Character. Directory to install into. Defaults to
#'   `tools::R_user_dir("aemetools", "data")/pestpp`, which persists between
#'   sessions.
#' @param os Character. One of `"win"`, `"iwin"` (Intel-compiled Windows),
#'   `"linux"` or `"mac"`. Defaults to the current platform.
#' @param force Logical. Reinstall even if the executables are already
#'   present. Default `FALSE`.
#' @param quiet Logical. Suppress progress messages. Default `FALSE`.
#'
#' @importFrom httr2 request req_perform req_url_query resp_body_json
#' @importFrom utils unzip untar
#'
#' @return Invisibly, the directory containing the installed executables.
#' @seealso [pest_exe_path()], [have_pest()]
#' @export
#'
#' @examples
#' \dontrun{
#' install_pest()
#' pest_exe_path("pestpp-ies")
#' }
install_pest <- function(version = "latest", dir = pest_install_dir(),
                         os = NULL, force = FALSE, quiet = FALSE) {

  os <- os %||% .pest_detect_os()
  os <- rlang::arg_match(os, c("win", "iwin", "linux", "mac"))

  if (!force && have_pest(dir = dir)) {
    msg <- sprintf("PEST++ already installed at %s. Use force = TRUE to reinstall.",
                   .pest_bin_dir(dir))
    AEME::cli_safe(msg, FUN = cli::cli_alert_info)
    return(invisible(.pest_bin_dir(dir)))
  }

  rel <- .pest_release_info(version = version, os = os)

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- file.path(tempdir(), rel$asset)
  on.exit(unlink(tmp), add = TRUE)

  if (!quiet) {
    msg <- sprintf("Downloading %s (%0.1f MB)", rel$asset, rel$size / 1e6)
    AEME::cli_inform_safe(c("i" = msg))
  }
  httr2::request(rel$url) |>
    httr2::req_progress() |>
    httr2::req_perform(path = tmp)

  # Release archives nest the binaries a few levels down (e.g.
  # pestpp-<ver>-<os>/bin/<os>/), and the layout has changed between
  # releases, so unpack to a staging dir and locate the executables by name
  # rather than assuming a fixed path.
  stage <- file.path(tempdir(), paste0("pestpp-stage-", as.integer(Sys.time())))
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)

  if (grepl("\\.zip$", tmp)) {
    utils::unzip(tmp, exdir = stage)
  } else {
    utils::untar(tmp, exdir = stage)
  }

  ext <- if (os %in% c("win", "iwin")) ".exe" else ""
  wanted <- paste0(pest_executables(), ext)
  found <- list.files(stage, recursive = TRUE, full.names = TRUE)
  found <- found[basename(found) %in% wanted]

  if (length(found) == 0) {
    cli::cli_abort(c(
      "No PEST++ executables found in {.val {rel$asset}}.",
      "i" = "The release layout may have changed; unpack it manually into
             {.path {.pest_bin_dir(dir)}}."
    ))
  }

  bin <- .pest_bin_dir(dir)
  dir.create(bin, recursive = TRUE, showWarnings = FALSE)
  file.copy(found, bin, overwrite = TRUE)

  if (os %in% c("linux", "mac")) {
    Sys.chmod(file.path(bin, basename(found)), mode = "0755")
  }

  writeLines(rel$tag, file.path(dir, "VERSION"))

  if (!quiet) {
    msg <- sprintf("Installed %d PEST++ executable%s (%s) to %s",
                   length(found), if (length(found) > 1) "s" else "",
                   rel$tag, bin)
    AEME::cli_safe(msg, FUN = cli::cli_alert_success)
  }
  invisible(bin)
}

#' Names of the PEST++ executables aemetools knows about
#' @return Character vector of executable base names (no extension).
#' @export
pest_executables <- function() {
  c("pestpp-glm", "pestpp-ies", "pestpp-sen", "pestpp-opt", "pestpp-mou",
    "pestpp-da", "pestpp-swp")
}

#' Default PEST++ installation directory
#' @return Character path.
#' @export
pest_install_dir <- function() {
  getOption("aemetools.pest_dir",
            file.path(tools::R_user_dir("aemetools", "data"), "pestpp"))
}

#' Locate a PEST++ executable
#'
#' Looks first in the aemetools installation directory (see
#' [install_pest()]), then falls back to the system `PATH` so that a
#' site-wide or conda-installed PEST++ is picked up automatically.
#'
#' @param exe Character. Executable name, e.g. `"pestpp-ies"`.
#' @param dir Character. Installation directory to search. Defaults to
#'   [pest_install_dir()].
#' @param error Logical. Abort with an install hint if not found?
#'   Default `TRUE`.
#'
#' @return Character path to the executable, or `NA_character_` when
#'   `error = FALSE` and it cannot be found.
#' @export
pest_exe_path <- function(exe = "pestpp-ies", dir = pest_install_dir(),
                          error = TRUE) {

  exe <- rlang::arg_match(exe, pest_executables())
  ext <- if (.Platform$OS.type == "windows") ".exe" else ""
  local <- file.path(.pest_bin_dir(dir), paste0(exe, ext))

  if (file.exists(local)) return(normalizePath(local, winslash = "/"))

  on_path <- Sys.which(exe)[[1]]
  if (nzchar(on_path)) return(normalizePath(on_path, winslash = "/"))

  if (error) {
    cli::cli_abort(c(
      "Could not find {.val {exe}}.",
      "i" = "Install it with {.run aemetools::install_pest()}, or add an
             existing PEST++ installation to your {.envvar PATH}."
    ))
  }
  NA_character_
}

#' Is PEST++ available?
#' @inheritParams pest_exe_path
#' @return Logical.
#' @export
have_pest <- function(dir = pest_install_dir()) {
  !is.na(pest_exe_path("pestpp-ies", dir = dir, error = FALSE))
}

# Internal helpers -------------------------------------------------------

#' @noRd
.pest_bin_dir <- function(dir) file.path(dir, "bin")

#' @noRd
.pest_detect_os <- function() {
  switch(Sys.info()[["sysname"]],
         Windows = "win",
         Darwin = "mac",
         Linux = "linux",
         cli::cli_abort("Unsupported platform: {.val {Sys.info()[['sysname']]}}"))
}

#' Resolve a release tag and asset URL from the GitHub releases API.
#' Assets are named `pestpp-<tag>-<os>.zip` (Windows) or
#' `pestpp-<tag>-<os>.tar.gz` (linux/mac), but rather than constructing the
#' name we match against the actual asset list so that naming changes in
#' future releases do not silently break the installer.
#' @noRd
.pest_release_info <- function(version, os) {

  base <- "https://api.github.com/repos/usgs/pestpp/releases"
  req <- httr2::request(if (identical(version, "latest")) {
    paste0(base, "/latest")
  } else {
    paste0(base, "/tags/", version)
  })
  # Honour a token when present to dodge the 60/hr unauthenticated API limit.
  tok <- Sys.getenv("GITHUB_PAT", Sys.getenv("GITHUB_TOKEN", ""))
  if (nzchar(tok)) req <- httr2::req_auth_bearer_token(req, tok)

  rel <- tryCatch(httr2::resp_body_json(httr2::req_perform(req)),
                  error = function(e) {
                    cli::cli_abort(c(
                      "Could not query the PEST++ releases API.",
                      "x" = e$message,
                      "i" = "Check your connection, or download the release
                             manually and unpack it into
                             {.path {.pest_bin_dir(pest_install_dir())}}."
                    ))
                  })

  pat <- paste0("-", os, "\\.(zip|tar\\.gz)$")
  hit <- Filter(\(a) grepl(pat, a$name), rel$assets)

  if (length(hit) == 0) {
    have <- vapply(rel$assets, \(a) a$name, character(1))
    cli::cli_abort(c(
      "No {.val {os}} asset in PEST++ release {.val {rel$tag_name}}.",
      "i" = "Available: {.val {have}}"
    ))
  }
  hit <- hit[[1]]

  list(tag = rel$tag_name, asset = hit$name, size = hit$size,
       url = hit$browser_download_url)
}
