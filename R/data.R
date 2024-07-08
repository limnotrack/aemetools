#' ERA5 reference table
#'
#' A reference table for variable names between ERA5, LER and AEME.
#'
#' @format ## `era5_ref_table`
#' A data frame with 9 rows and 6 columns:
#' \describe{
#'   \item{variable}{Variable name}
#'   \item{era5}{ERA5 variable names}
#'   \item{nc}{ERA nc variable names}
#'   \item{ler}{LakeEnsemblR variable names}
#'   \item{aeme}{AEME variable names}
#'   \item{nc}{ERA nc variable names}
#' }
#' @source Package development.
"era5_ref_table"

#' sf object of Digital Elevation Model (DEM) metadata for New Zealand.
#'
#' This can be used to query DEM coverage and metadata for New Zealand.
#'
#' @format ## `nz_dem_metadata`
#' A sf data frame with 230 rows and 8 columns:
#' \describe{
#'   \item{layer_id}{Layer ID of the DEM}
#'   \item{title}{Title of the DEM}
#'   \item{abstract}{Abstract of the DEM}
#'   \item{region}{Region of the DEM}
#'   \item{res}{Resolution of the DEM. Units are in denoted in the units column.}
#'   \item{units}{Units of the resolution DEM}
#'   \item{year}{Year of the DEM}
#'   \item{geometry}{Geometry of the DEM}
#' }
#' @source Package development.
"nz_dem_metadata"

#' sf object of aerial imagery metadata for New Zealand.
#'
#' This can be used to query aerial imagery coverage and metadata for New
#' Zealand.
#'
#' @format ## `nz_aerial_imagery_metadata`
#' A sf data frame with 230 rows and 8 columns:
#' \describe{
#'   \item{layer_id}{Layer ID of the aerial imagery}
#'   \item{title}{Title of the aerial imagery}
#'   \item{abstract}{Abstract of the aerial imagery}
#'   \item{region}{Region of the aerial imagery}
#'   \item{res}{Resolution of the aerial imagery. Units are in denoted in the units column.}
#'   \item{units}{Units of the resolution aerial imagery}
#'   \item{year}{Year of the aerial imagery}
#'   \item{geometry}{Geometry of the aerial imagery}
#' }
#' @source Package development.
"nz_aerial_imagery_metadata"


