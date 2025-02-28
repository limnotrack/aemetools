#' Get raster layer value for a given latitude and longitude
#'
#' @inheritParams get_era5_land_point_nz
#' @param layer_id numeric; layer ID value for the raster layer on the LINZ
#' data service. See \url{https://data.linz.govt.nz/}
#' @param key character; LINZ API key. This can be set as an environment variable
#' using the \code{add_linz_key()} function or passed as a character.
#' See \code{?add_linz_key} for more information with setting up the API key.
#'
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#'
#' @return numeric value or NA if outside extent
#' @export
#'

get_raster_layer_value <- function(lat, lon, layer_id, key = NULL) {

  # Argument checks
  stopifnot("`lat` must be a numeric." = is.numeric(lat))
  stopifnot("`lon` must be a numeric." = is.numeric(lon))
  stopifnot("`layer_id` must be a numeric." = is.numeric(layer_id))
  stopifnot("`key` must be a character or NULL." = is.character(key) |
              is.null(key))

  if (is.null(key)) {
    key <- Sys.getenv("LINZ_KEY")
    if (key == "") {
      stop("No LINZ API key found. See ?add_linz_key for more information.")
    }
  }

  req <- paste0("https://data.linz.govt.nz/services/query/v1/raster.json?key=",
                key, "&layer=", layer_id, "&x=", lon, "&y=", lat)
  res <- httr::GET(req)
  if (res$status_code == 200) {
    data <- jsonlite::fromJSON(rawToChar(res$content))

    if (data$rasterQuery$layers[[as.character(layer_id)]]$status == "outside-extent") {
      message("Outside extent for layer ID ", layer_id, ". Returning NA.")
      return(NA)
    }

    if (is.na(data$rasterQuery$layers[[as.character(layer_id)]][["bands"]])) {
      message("NA in the bands for ", layer_id, ". Returning NA.")
      return(NA)
    }


    val <- data$rasterQuery$layers[[as.character(layer_id)]][["bands"]][["value"]]
  } else {
    if (res$status_code == 403) {
      message("Status code ", res$status_code, "Invalid LINZ API key.")
    }
    if (res$status_code == 400) {
      message("Status code ", res$status_code, " for ", layer_id,
              ". Bad request. Check lat and lon are correct.")

    } else {
      message("Status code ", res$status_code, " for ", layer_id,
              ". Returning NA.")
    }
    val <- NA
  }
  val
}

#' Get DEM value for a given latitude and longitude
#'
#' This function uses the LINZ data service to obtain the elevation value for a
#' given latitude and longitude. The function \code{get_raster_layer_value()} is
#' used to obtain the value for the DEM layer (ID: 51768).
#'
#' @inheritParams get_raster_layer_value
#'
#' @return numeric value or NA if outside extent
#' @export
#'

get_dem_value <- function(lat, lon, key = NULL) {

  layer_id <- 51768

  get_raster_layer_value(lat = lat, lon = lon, layer_id = layer_id, key = key)
}

#' Add LINZ API key to environment variables
#'
#' @param key character; LINZ API key. An account is needed to obtain an API
#' key. The function \code{create_linz_key()} will open a browser window to
#' create an account and obtain an API key.
#'
#' @export
#'
#' @return NULL

add_linz_key <- function(key) {
  stopifnot("`key` must be a character." = is.character(key))
  Sys.setenv(LINZ_KEY = key)
}


#' Create LINZ API key
#'
#' This function will open a browser window to create a LINZ account and obtain
#' an API key. The API key will be copied to the clipboard and can be added to
#' the environment variables using the \code{add_linz_key()} function.
#'
#' @export

create_linz_key <- function() {
  browseURL("https://id.koordinates.com/signup/?next=%2Fo%2Fauthorize%2F%3Fclient_id%3Dt1RwFgXlDfvmPvqGaAoqj1GnULvYOGTOh81AuiS5%26response_type%3Dcode%26state%3DeyJjc3JmdG9rZW4iOiJ5eHRFRUZSYTNVYjBtdXk3OURHUXBtNjltRlMwN2NXVms1Vmk1WXF1YVZ5Qnp2bFY2V0RxZE5qZ3RzemhVTUlKIiwibmV4dCI6Ii8iLCJ3YXJlaG91c2VfaWQiOjIwMDEsImhhc2hlZF9zZXNzaW9uX2tleSI6ImNlOGI1ODJhM2UzNmExY2JhNDc0YmNlNzg5M2VkMzZkYzc0NWZhZjZhNWExYWYzYzBjODUxNDUwZjNhN2IwY2QifQ%253A1rDA4H%253AM77POBdI8sFwySjZI-tgXmmnTcmGWxMtxM3pqyTyoDY%26redirect_uri%3Dhttps%253A%252F%252Fdata.linz.govt.nz%252Flogin%252Foauth%252Fcallback%252F&_no_redirect=1")
}

