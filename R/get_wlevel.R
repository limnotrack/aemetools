#' Get modelled water level
#'
#' @param lake_dir lake directory
#' @param model string; model name.
#' @param nlev numeric; number of levels to extract.
#' @param return_df logical; return dataframe of water level. Default is FALSE.
#'
#' @return dataframe of water level.
#'
#' @importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
#' @importFrom dplyr case_when
#'
#' @export

get_wlevel <- function(lake_dir, model, nlev, return_df = FALSE) {
  out_file <- dplyr::case_when(model == "dy_cd" ~ file.path(lake_dir,
                                                            model, "DYsim.nc"),
                               model == "glm_aed" ~ file.path(lake_dir, model,
                                                              "output",
                                                              "output.nc"),
                               model == "gotm_pclake" ~ file.path(lake_dir,
                                                                  model,
                                                                  "output",
                                                                  "output.nc"),
                               model == "gotm_wet" ~ file.path(lake_dir, model,
                                                               "output",
                                                               "output.nc")
  )

  if(!file.exists(out_file)) {
    message("No ", out_file, " present.")
    return(NULL)
  }

  nc <- ncdf4::nc_open(out_file, return_on_error = TRUE)
  on.exit(ncdf4::nc_close(nc))
  if(nc$error) {
    stop("Could not open netCDF file: ", out_file)
  }

  if (model == 'gotm_pclake' | model == "gotm_wet") {
    out.steps <- ncdf4::ncvar_get(nc, "time")
    date.start <- ncdf4::ncatt_get(nc,'time','units')$value |>
      gsub("seconds since ", "", x = _) |>
      as.POSIXct() |>
      as.Date()
    dates <- seq.Date(date.start, by = 1, length.out = length(out.steps))
    # h <- ncdf4::ncvar_get(nc, "h")
    # z <- ncdf4::ncvar_get(nc, "z")
    zi <- ncdf4::ncvar_get(nc, "zi")
    if(is.null(dim(zi))) {
      return(NA)
    }
    # zeta <- ncdf4::ncvar_get(nc, "zeta")
    h.surf <- zi[nrow(zi), ] - zi[1, ]
  } else if (model == 'glm_aed') {
    h.surf <- ncdf4::ncvar_get(nc, "lake_level")
    h.surf <- c(h.surf[1], h.surf)
    hours.since  <- ncdf4::ncvar_get(nc, "time")
    date.start <- as.POSIXct(gsub("hours since ", "",
                                  ncdf4::ncatt_get(nc,'time','units')$value))
    dates <- as.POSIXct(hours.since * 3600 + date.start) |>
      as.Date()
    dates <- c(dates[1] - 1, dates)
  } else if (model == 'dy_cd') {
    time <- ncdf4::ncvar_get(nc, 'dyresmTime')
    time[time > 9.e+36] <- NA
    if (any(is.na(time))) {
      time <- seq(from = time[1], length.out = length(time), by = 1)
    }
    dates <- as.POSIXct((time - 2415018.5)*86400,
                        origin = "1899-12-30", tz = "UTC") |>
      as.Date()
    if (any(is.na(dates))) {

    }
    lyrs <- ncdf4::ncvar_get(nc, "dyresmLAYER_HTS_Var")
    h.surf <- apply(lyrs, 2, \(x) max(x, na.rm = TRUE))
  }

  h.surf[is.infinite(h.surf)] <- NA
  if(return_df) {
    df <- data.frame(Date = dates, lvl = h.surf)
    return(df)
  } else {
    return(h.surf)
  }
}
