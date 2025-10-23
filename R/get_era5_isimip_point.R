# lon <- 13.064332
# lat <- 52.380551
# years <- 2015:2021
# vars <- c("MET_tmpair", "MET_pprain")

#' Get ERA5 data from ISIMIP3a for a point location
#'
#' @inheritParams get_era5_land_point_nz
#' @param download_path character; path to download the data. Default is
#' the temporary directory.
#'
#' @importFrom httr2 request req_body_json req_perform resp_status resp_body_json
#' @importFrom httr2 resp_body_string
#' @importFrom httr2 req_url_query req_url_path_append req_headers
#' @importFrom httr2 req_perform
#' @importFrom jsonlite toJSON
#' @importFrom terra rast values
#' @importFrom dplyr bind_rows full_join
#' @importFrom zip unzip
#' @importFrom logger log_info log_error
#'
#' @returns A data frame with the requested variables
#' @export
#'
#' @examples
#' \dontrun{
#' lon <- 13.064332
#' lat <- 52.380551
#' years <- 2015:2021
#' vars <- c("MET_tmpair", "MET_pprain")
#' get_era5_isimip_point(lon, lat, years, vars)
#' }

get_era5_isimip_point <- function(lon, lat, years,
                                  vars = c("MET_tmpair", "MET_pprain",
                                           "MET_wndspd", "MET_radswd",
                                           "MET_prsttn", "MET_radlwd",
                                           "MET_humrel"),
                                  download_path = tempdir()) {
  # Set up logging
  log_info <- function(...) logger::log_info(...)
  log_error <- function(...) logger::log_error(...)

  # ISIMIP API URL
  url <- "https://files.isimip.org/api/v2"

  # Variable checking and mapping
  vars <- check_vars(vars)

  paths <- build_paths(vars, years)
  # paths <- "ISIMIP3a/InputData/climate/atmosphere/obsclim/global/daily/historical/20CRv3-ERA5/20crv3-era5_obsclim_hurs_global_daily_200_2010.nc"

  data <- list(
    paths = paths,
    operations = list(
      list(
        operation = "select_bbox",
        # operation = "cutout_point",
        bbox = list(lon - 0.25, lon + 0.25, lat - 0.25, lat + 0.25)  # minx, maxx, miny, maxy
        # point = list(lon, lat),  # longitude, latitude
        # output_csv = output_csv  # optional: set to TRUE to get a CSV file instead of NetCDF
      )
    )
  )
  
  
  # Perform the initial request to the server
  req <- httr2::request(url) |>
    httr2::req_body_json(data)
  
  res <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      log_error("job submission failed", error = e$message)
      return(NULL)
    }
  )
  
  if (is.null(res)) return(invisible(NULL))
  
  if (httr2::resp_status(res) >= 200 && httr2::resp_status(res) < 300) {
    job <- httr2::resp_body_json(res)
    log_info("job submitted", id = job$id, status = job$status)
    
    # Poll until finished
    while (job$status %in% c("queued", "started")) {
      Sys.sleep(4)
      job_req <- httr2::request(job$job_url)
      job_res <- httr2::req_perform(job_req)
      job <- httr2::resp_body_json(job_res)
      log_info("job updated", id = job$id, status = job$status, meta = job$meta)
    }
    
    if (job$status == "finished") {
      # Download file
      zip_path <- file.path(download_path, job$file_name)
      dir.create(dirname(zip_path), showWarnings = FALSE, recursive = TRUE)
      log_info("downloading", file_url = job$file_url)
      download.file(job$file_url, zip_path, mode = "wb")
      
      # Extract zip file
      out_path <- sub("\\.zip$", "", zip_path)
      dir.create(out_path, showWarnings = FALSE, recursive = TRUE)
      log_info("extracting", zip_path = zip_path, out_path = out_path)
      zip::unzip(zip_path, exdir = out_path)
      
    } else {
      log_error("job did not finish successfully", status = job$status)
    }
  } else {
    log_error("job submission failed", error = httr2::resp_body_string(res))
  }

  out <- lapply(vars, \(v) {
    fils <- list.files(out_path, full.names = TRUE, pattern = paste0("_", v, "_"))
    df <- lapply(fils, \(f) {
      suppressWarnings({
        nc <- terra::rast(f)
      })
      vals <- terra::values(nc)
      dates <- terra::time(nc)
      data.frame(Date = dates, value = as.vector(vals))
      # read.csv(f, header = FALSE)
    }) |>
      dplyr::bind_rows()
    head(df)
    names(df) <- c("Date", v)
    return(df)
  })

  # Join all data frames by the "Date" column
  result <- Reduce(function(x, y) dplyr::full_join(x, y, by = "Date"), out)

  # Unit conversions
  # If "tas" is present, convert from K to C
  if ("tas" %in% colnames(result)) {
    result$tas <- result$tas - 273.15
  }

  # If "pr" is present, convert from kg m-2 s-1 to mm/day
  if ("pr" %in% colnames(result)) {
    result$pr <- result$pr * 86400
  }

  # Rename to AEME column names
  names(result) <- switch_vars(names(result))

  return(result)
}

#' Check and map variable names
#'
#' @param vars A character vector of variable names
#' @return A vector of variable names suitable for ISIMIP3a
#' @noRd
check_vars <- function(vars) {

  # Reference data frame for variable mapping
  vars_df <- data.frame(
    era5 = c("tas", "pr", "sfcwind", "rsds", "ps", "rlds", "hurs"),
    aeme = c("MET_tmpair", "MET_pprain", "MET_wndspd", "MET_radswd",
             "MET_prsttn", "MET_radlwd", "MET_humrel")
  )

  aeme_chk <- grepl("^MET_", vars)
  if (any(aeme_chk)) {
    if (!all(aeme_chk)) stop("Mixing of ERA5 and AEME variables is not allowed")
    vars <- vars_df |>
      dplyr::filter(aeme %in% vars) |>
      # arrange in the same order as vars
      dplyr::arrange(match(vars, aeme)) |>
      dplyr::pull(era5)
  } else {
    if (!all(vars %in% vars_df$era5)) stop("Invalid variable(s)")
  }
  if (length(vars) == 0) stop("No valid variables")
  return(vars)
}

#' Switch variable names
#'
#' @param vars A character vector of variable names
#' @return A vector of variable names suitable for ISIMIP3a or AEME
#' @noRd
switch_vars <- function(vars) {

  # Reference data frame for variable mapping
  mapping_df <- data.frame(
    era5 = c("tas", "pr", "sfcwind", "rsds", "ps", "rlds", "hurs"),
    aeme = c("MET_tmpair", "MET_pprain", "MET_wndspd", "MET_radswd",
             "MET_prsttn", "MET_radlwd", "MET_humrel")
  )
  # Create a named vector for mapping
  era5_to_aeme <- setNames(mapping_df$aeme, mapping_df$era5)
  aeme_to_era5 <- setNames(mapping_df$era5, mapping_df$aeme)

  aeme_chk <- grepl("^MET_", vars)
  if (any(aeme_chk)) {
    upd_vars <- sapply(vars, function(x) {
      if (x %in% names(aeme_to_era5)) aeme_to_era5[[x]] else x
    }, USE.NAMES = FALSE)
  } else {
    upd_vars <- sapply(vars, function(x) {
        if (x %in% names(era5_to_aeme)) era5_to_aeme[[x]] else x
      }, USE.NAMES = FALSE)
  }

  return(upd_vars)
}


#' Get decades for a given year for ISIMIP3a data
#'
#' @param years A numeric vector of years
#' @return A vector of decades
#' @noRd
get_decades <- function(years) {
  # Ensure the input is numeric
  years <- as.numeric(years)

  # Initialize a vector to store the results
  decades <- c()

  for (year in years) {
    if (year %% 10 == 1 && year > 2000) {
      # Handle single-year case (e.g., 2021)
      decade_str <- paste0(year, "_", year)
    } else {
      # Calculate the start of the decade
      decade_start <- floor((year - 1) / 10) * 10 + 1
      # Calculate the end of the decade
      decade_end <- decade_start + 9
      # Combine into a string
      decade_str <- paste0(decade_start, "_", decade_end)
    }
    decades <- c(decades, decade_str)
  }

  # Return unique decades
  return(unique(decades))
}

#' Build paths for ISIMIP3a data
#'
#' @param vars A character vector of variable names
#' @param years A numeric vector of years
#' @return A list of paths
#' @noRd
build_paths <- function(vars, years) {
  # Get the decades for the years
  decades <- get_decades(years)

  path_list <- lapply(vars, \(v) {
    paste0("ISIMIP3a/InputData/climate/atmosphere/obsclim/global/daily/historical/20CRv3-ERA5/20crv3-era5_obsclim_", v, "_global_daily_", decades, ".nc")
  })
  vec <- unlist(path_list)
  lst <- as.list(vec)
  return(lst)
}
