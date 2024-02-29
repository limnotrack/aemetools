#' Get LINZ basemap tile
#'
#' Get map tiles from the LINZ basemap service. This requires an API key which
#' can be obtained from: https://basemaps.linz.govt.nz
#'
#' @inheritParams maptiles::get_tiles
#' @inheritParams get_raster_tile
#'
#' @return A SpatRaster is returned.
#' @export
#'

get_linz_basemap_tile <- function(x, zoom = 16, key = NULL, verbose = FALSE) {

  if (is.null(key)) {
    key <- Sys.getenv("LINZ_BASEMAP_KEY")
    if (key == "") {
      stop(strwrap("No LINZ basemap key found. Please supply your key.\nKeys can
                   be generated from: https://basemaps.linz.govt.nz"))
    }
  }

  url <- paste0("https://basemaps.linz.govt.nz/v1/tiles/aerial/WebMercatorQuad/{z}/{x}/{y}.webp?api=", key)
  prov <- maptiles::create_provider(name = "LINZ",
                                    url = url,
                                    citation = "© LINZ CC BY 4.0 © Imagery Basemap contributors")
  catch <- tryCatch({
    tiles <- maptiles::get_tiles(x, provider = prov, zoom = zoom,
                                 forceDownload = TRUE, verbose = verbose)
    TRUE
  }, error = function(e) {
    FALSE
  })
  if (!catch) {
    return(NULL)
  }
  return(tiles)
}
