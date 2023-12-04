#' Extract data from ERA5 RDS and format for `LakeEnsemblR```
#'
#' Extract met data frame from closest grid to lat & lon
#'
#' @param lat numeric; Latitude
#' @param lon numeric; Longitude
#' @param years numeric; vector of years in numeric form to be extracted
#' @param variables vector; with lookup table for variable names
#' @param format string; the format for column headers. Can be "aeme" or "ler".
#' Defaults to "aeme".
#' @param parallel boolean; parallelise the download of ERA5 variables. Defaults
#' to FALSE.
#' @param ncores integer; number of cores to use for parallelisation. If
#' missing, defaults to `min(c(parallel::detectCores() - 1, length(variables)))`
#'
#' @importFrom sf st_as_sf
#' @importFrom stars st_extract
#' @importFrom parallel detectCores makeCluster stopCluster clusterExport
#' parLapply
#' @importFrom dplyr filter mutate across
#' @importFrom stats complete.cases na.exclude
#'
#' @return dataframe of daily ERA5 data.
#'
#' @export

get_era5_point <- function(lat, lon, years, variables, format = "aeme",
                           parallel = FALSE, ncores) {

  token_file <- system.file("extdata/token.rds", package = "aemetools")
  dtoken <- readRDS(token_file)
  db_path <- "lernzmp/nz_era5_daily_rds/"

  test <- tryCatch({
    suppressMessages(
      rdrop2::drop_exists("lernzmp", dtoken = dtoken)
    )
  }, error = function(e) FALSE)
  if (!test) {
    stop(strwrap("Current Dropbox token is not working. Please raise an issue
                 on our GitHub page: "),
         "\nhttps://github.com/limnotrack/aemetools/issues")
  }

  data("era5_ref_table", package = "AEME", envir = environment())

  # Catch for date
  if ("Date" %in% variables) {
    message("'Date' found in variables... Removing this from the vector.")
    variables <- variables[!variables %in% "Date"]
  }


  sel_vars <- era5_ref_table |>
    dplyr::filter(aeme %in% variables)

  # Error if no variables are found
  if (nrow(sel_vars) == 0)
    stop(strwrap("No variables found in lookup table. Ensure you use AEME
                 variable names."))

  # Check if point is in the grid
  coords <- check_point_in_grid(lat = lat, lon = lon, dtoken = dtoken,
                                db_path = db_path)


  if (parallel & length(variables) == 1) {
    if (missing(ncores)) {
      ncores <- min(c(parallel::detectCores() - 1, length(years)))
    }
    cl <- parallel::makeCluster(ncores)
    on.exit({
      parallel::stopCluster(cl)
    })
    parallel::clusterExport(cl, varlist = list("sel_vars", "coords", "db_path",
                                               "dtoken", "download_era5_point"),
                            envir = environment())
    message("Downloading ERA5 variable in parallel... ",
            paste0("[", Sys.time(), "]"))
    out <- parallel::parLapply(cl = cl, years, function(y) {
      download_era5_point(years = y, lat = coords$lat, lon = coords$lon,
                          variable = sel_vars$era5, db_path = db_path,
                          dtoken = dtoken)
    })
    df <- do.call(rbind, out)
  } else if (parallel & length(variables) > 1) {
    if (missing(ncores)) {
      ncores <- min(c(parallel::detectCores() - 1, length(variables)))
    }
    cl <- parallel::makeCluster(ncores)
    on.exit({
      parallel::stopCluster(cl)
    })
    parallel::clusterExport(cl, varlist = list("years", "coords", "db_path",
                                               "dtoken", "download_era5_point"),
                            envir = environment())
    message("Downloading ERA5 variables in parallel... ",
            paste0("[", Sys.time(), "]"))
    out <- parallel::parLapply(cl = cl, sel_vars$era5, function(v) {
      download_era5_point(years = years, lat = coords$lat, lon = coords$lon,
                          variable = v, db_path = db_path, dtoken = dtoken)
    })
    df <- Reduce(merge, out)
  } else {
    message("Downloading ERA5 variables... ",
            paste0("[", Sys.time(), "] (Have you tried parallelising?)"))

    out <- lapply(sel_vars$era5, function(v) {
      # print(v)
      download_era5_point(years = years, lat = coords$lat, lon = coords$lon,
                          variable = v, db_path = db_path, dtoken = dtoken)
    })
    df <- Reduce(merge, out)
  }

  message("Finished downloading ERA5 variables! ", paste0("[", Sys.time(), "]"))


  if ("t2m" %in% colnames(df) | "d2m" %in% colnames(df)) {
    sel_cols <- which(colnames(df) %in% c("t2m", "d2m"))
    df[, sel_cols] <- df[, sel_cols] - 273.15
  }
  if ("tp" %in% colnames(df) | "sf" %in% colnames(df)) {
    sel_cols <- which(colnames(df) %in% c("tp", "sf"))
    df[, sel_cols] <- df[, sel_cols] * 1000 # Convert to m
  }

  names(df) <- era5_ref_table[[format]][match(names(df), era5_ref_table$nc)]


  df <- df |>
    dplyr::mutate(dplyr::across(!dplyr::contains("date"), signif))

  na_rows <- which(!stats::complete.cases(df))
  if(length(na_rows) > 0) {
    message("Removing NA's on ", paste0(df[na_rows, 1], collapse = ", "))
    df <- stats::na.exclude(df)
  }
  df
  }

#' Download ERA5 data for a variable
#'
#' @inheritParams get_era5_point
#' @param variable A character vector of ERA5 variables to download.
#' @param db_path A character string of the path to the Dropbox folder.
#' @param dtoken A Dropbox token.
#'
#' @importFrom rdrop2 drop_exists drop_download
#' @importFrom stars st_extract
#' @importFrom dplyr select
#'
#' @noRd

download_era5_point <- function(years, lat, lon, variable, db_path, dtoken) {

  out <- lapply(years, function(y) {
    # print(y)
    db1 <- paste0(db_path, "nz_era5-land_", y, "_", variable, "_1234_daily.rds")
    db2 <- paste0(db_path, "nz_era5-land_", y, "_", variable, "_5678_daily.rds")
    db3 <- paste0(db_path, "nz_era5-land_", y, "_", variable,
                  "_9101112_daily.rds")

    chk1 <- rdrop2::drop_exists(path = db1, dtoken = dtoken)
    chk2 <- rdrop2::drop_exists(path = db2, dtoken = dtoken)
    chk3 <- rdrop2::drop_exists(path = db3, dtoken = dtoken)

    if (!all(c(chk1, chk2, chk3))) {
      message()
      if (y %in% 1999:2021) {
        err_msg <- paste0("Files:\n",
                          paste0(c(db1, db2, db3)[!c(chk1, chk2, chk3)],
                                 collapse = "\n"),
                          "\nare currently not available on Dropbox.")
      } else {
        err_msg <- "Files for selected years are not present currently in
                     dropbox. Current available years are 1999-2021."
      }
      stop(strwrap(err_msg))
    }

    f1 <- tempfile()
    rdrop2::drop_download(path = db1, local_path = f1, dtoken = dtoken,
                          progress = FALSE, verbose = FALSE)
    f2 <- tempfile()
    rdrop2::drop_download(path = db2, local_path = f2, dtoken = dtoken,
                          progress = FALSE, verbose = FALSE)
    f3 <- tempfile()
    rdrop2::drop_download(path = db3, local_path = f3, dtoken = dtoken,
                          progress = FALSE, verbose = FALSE)

    if (!all(file.exists(c(f1, f2, f3)))) {
      stop(strwrap("Files:\n'", f1, "'\n'", f2, "'\n'", f3, "'\n do not exist.
                     Download has failed. Check your internet connection."))
    }

    coords <- data.frame(lat = lat, lon = lon)

    coords_sf <- sf::st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)

    d1 <- readRDS(f1) |>
      stars::st_extract(coords_sf) |>
      as.data.frame() |>
      dplyr::select(-geometry)
    d1
    d2 <- readRDS(f2) |>
      stars::st_extract(coords_sf) |>
      as.data.frame() |>
      dplyr::select(-geometry)
    d3 <- readRDS(f3) |>
      stars::st_extract(coords_sf) |>
      as.data.frame() |>
      dplyr::select(-geometry)

    rbind(d1, d2, d3)
  })

  do.call(rbind, out)
}



#' Check if a point is in the ERA5 grid
#'
#' @inheritParams download_era5_point
#'
#' @importFrom rdrop2 drop_download
#' @importFrom stars st_extract
#' @importFrom dplyr select
#' @importFrom units set_units
#'
#' @noRd
check_point_in_grid <- function(lat, lon, dtoken, db_path) {

  db1 <- paste0(db_path, "nz_era5-land_2020_2m_temperature_1234_daily.rds")
  f1 <- tempfile()
  rdrop2::drop_download(path = db1, local_path = f1, dtoken = dtoken,
                        progress = FALSE, verbose = FALSE)


  coords <- data.frame(lat = lat, lon = lon)

  coords_sf <- sf::st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)

  strs <- readRDS(f1)
  d1 <- strs |>
    stars::st_extract(coords_sf) |>
    as.data.frame() |>
    dplyr::select(-geometry)

  if (all(is.na(d1[, 2]))) {
    message("Point is not in the grid. Identifying nearest point...")
    dat <- strs[1, 1, , ] |>
      # Convert stars object to sf
      sf::st_as_sf()

    p1 <- dat[sf::st_nearest_feature(coords_sf, dat), ] |>
      sf::st_centroid()

    dist <- sf::st_distance(coords_sf, p1)

    message("Nearest point identified. It is ",
            round(units::set_units(dist, "km"), 2), " km away.")

    p1 |>
      sf::st_coordinates() |>
      as.data.frame() |>
      dplyr::select(X, Y) |>
      setNames(c("lon", "lat"))


  } else {
    message("Point is in the grid.")
    coords
  }

}
