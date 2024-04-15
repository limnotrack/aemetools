#' Get a raster tile from LINZ
#'
#' @inheritParams maptiles::get_tiles
#' @inheritParams get_raster_layer_value
#' @param zoom numeric; zoom level for the tile. Default is 15.
#' @param key character; LINZ API key. If NULL, will look for the key in the
#'  LINZ_KEY environment variable. If that is not set, will throw an error.
#'  Use the \code{add_linz_key} function to set the key.
#'
#'  @importFrom maptiles create_provider get_tiles
#'  @importFrom terra subset
#'
#' @return a raster object
#' @export
#'

get_raster_tile <- function(x, layer_id, zoom = 15, key = NULL,
                            verbose = FALSE) {


  if (is.null(key)) {
    key <- Sys.getenv("LINZ_KEY")
    if (key == "") {
      stop("No LINZ API key found. See ?add_linz_key for more information.")
    }
  }

  prov <- maptiles::create_provider(name = "LINZ",
                                    url = paste0("https://tiles-cdn.koordinates.com/services;key=",
                                                 key, "/tiles/v4/layer=", layer_id,
                                                 "/EPSG:3857/{z}/{x}/{y}.png"),
                                    citation = "LINZ")
  maptiles::get_tiles(x = x, provider = prov, zoom = zoom,
                      verbose = verbose, forceDownload = TRUE)
}
