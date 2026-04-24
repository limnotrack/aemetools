#' Create a spatial HydroModel object
#'
#' Assembles the upstream river-channel network and catchment polygon for a
#' given reach ID, computes the total catchment area (excluding the lake
#' polygon), and returns a [HydroModel] object whose slots are focused on
#' spatial data.  The computed airGR model inputs are added in a subsequent
#' call to [make_GR_inputs()].
#'
#' @param id numeric; Reach ID (`nzsegment`).
#' @param reaches sf; object with reaches as linestrings.
#' @param lake sf; polygon of the target lake shore.
#' @param catchments sf; polygon of catchments (including sub-catchments).
#' @param dem ANY; optional DEM raster (e.g. a `terra::SpatRaster`).
#'   Defaults to `NULL`.
#' @param land_cover ANY; optional land cover data. Defaults to `NULL`.
#' @param soil ANY; optional soil data. Defaults to `NULL`.
#' @param outlet ANY; optional outlet point(s) as an `sf` object.
#'   Defaults to `NULL`.
#' @param plot logical; plot the assembled reaches, lake and catchment?
#'   Defaults to `FALSE`.
#'
#' @importFrom methods new
#' @importFrom sf st_crs st_difference st_union st_area
#' @importFrom dplyr filter
#' @importFrom units drop_units
#'
#' @return A [HydroModel] object with spatial slots populated and
#'   `catchment_area` computed.  The airGR input slots (`inputs_model`,
#'   `data`, `start`, `fun_mod`, `fun_mod_name`) are `NULL` / `NA` until
#'   [make_GR_inputs()] is called.
#' @export

make_hydro_model <- function(id, reaches, lake, catchments,
                             dem = NULL, land_cover = NULL, soil = NULL,
                             outlet = NULL, plot = FALSE) {

  if (!(sf::st_crs(reaches) == sf::st_crs(lake) &&
        sf::st_crs(reaches) == sf::st_crs(catchments))) {
    stop(strwrap("Coordinate reference systems differ between reaches, lake and
                 catchments. Ensure they share the same CRS."))
  }

  model_crs <- sf::st_crs(reaches)

  hyd_id <- reaches$HydroID[reaches$nzsegment == id]
  upstr  <- get_upstream_rec(HydroID = hyd_id, reaches = reaches)
  upstr  <- sf::st_difference(upstr, lake)

  sub_catch  <- catchments |>
    dplyr::filter(nzsegment %in% upstr$nzsegment)

  tot_catchm <- sf::st_union(sub_catch)
  tot_catchm <- sf::st_difference(tot_catchm, lake)

  tot_rivers <- sf::st_union(upstr)

  if (plot) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = lake,       fill = "cyan") +
      ggplot2::geom_sf(data = tot_catchm, fill = "#EDB48E") +
      ggplot2::geom_sf(data = tot_rivers, colour = "blue") +
      ggplot2::theme_bw()
    print(p)
  }

  catch_area <- units::drop_units(sf::st_area(tot_catchm))

  methods::new(
    "HydroModel",
    channels       = tot_rivers,
    lake           = lake,
    catchments     = tot_catchm,
    dem            = dem,
    land_cover     = land_cover,
    soil           = soil,
    outlet         = outlet,
    crs            = model_crs,
    catchment_area = catch_area
  )
}
