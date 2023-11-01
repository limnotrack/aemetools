#' Create temporary directories from model folders.
#'
#' @param model string; for which model. Options are c("dy_cd", "glm_aed" and
#'  "gotm_wet")
#' @param lake_dir filepath; to directory which contains the model
#' configuration.
#' @param n integer; number of directories to create.
#'
#' @return vector of temporary directories.
#' @noRd

make_temp_dir <- function(model, lake_dir, n = 2) {

  model_dir <- file.path(lake_dir, model)
  oldwd <- getwd()
  setwd(model_dir)
  on.exit(setwd(oldwd))
  fils <- list.files()
  fils <- fils[fils != "output"]
  fils <- fils[!grepl("restart", fils)]
  fils <- fils[!grepl("nc", fils)]

  temp_dirs <- sapply(1:n, \(n) {
    dir <- file.path(tempdir(), paste0("n_", n), basename(lake_dir), model)
    unlink(dir, recursive = TRUE, force = TRUE)
    dir.create(dir, recursive = TRUE)
    file.copy(fils, dir, recursive = TRUE)
    file.path(tempdir(), paste0("n_", n))
  })
}
