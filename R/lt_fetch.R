#' Query LimnoTrack database with flexible parameters
#' 
#' This function allows you to query any table in the LimnoTrack database with optional parameters for selecting columns, filtering rows, ordering results, and pagination. It automatically detects if the table contains geometry data and returns an `sf` object if so, or a regular tibble otherwise.
#'
#' @param table Name of the table to query (e.g., "lake_contours").
#' @param select Optional string of comma-separated column names to select (e.g., "col1,col2"). If NULL, all columns are returned.
#' @param filter Optional named list of filter conditions, where names are column names and values are filter expressions (e.g., list(lernzmp_id = "eq.LID40188")). You can use the `lt_filter()` helper to construct this list from R expressions.
#' @param order Optional string specifying the order of results (e.g., "col1.asc,col2.desc"). If NULL, no specific ordering is applied.
#' @param limit Number of records to return. Default is 1000.
#' @param offset Number of records to skip for pagination. Default is 0.
#' @param base_url Base URL of the LimnoTrack API. Default is "https://api.limnotrack.com/postgrest".
#'
#' @returns A tibble or an `sf` object containing the query results, depending 
#' on whether the table has geometry data.
#' @export
#'
#' @examples
#' #' # Query lake contours with a specific lernzmp_id
#' contours <- lt_fetch(
#'  table = "lake_contours",
#'  filter = lt_filter(lernzmp_id == "LID40188")
#'  )
#'  
lt_fetch <- function(
    table,
    select   = NULL,
    filter   = NULL,
    order    = NULL,
    limit    = 1000,
    offset   = 0,
    base_url = "https://api.limnotrack.com/postgrest"
) {
  geom_info <- lt_detect_geom(table, base_url)
  has_geom  <- !is.null(geom_info)
  if (!is.null(select) && has_geom) {
    # ensure geometry column is always included for GeoJSON requests
    if (!geom_info$col %in% select) {
      select <- c(select, geom_info$col)
    }
  }
  
  accept_type <- if (has_geom) "application/geo+json" else "application/json"
  
  req <- httr2::request(base_url) |>
    httr2::req_url_path_append(table) |>
    httr2::req_headers(Accept = accept_type)
  
  params <- list(limit = limit, offset = offset)
  if (!is.null(select)) params$select <- select
  if (!is.null(order))  params$order  <- order
  if (!is.null(filter)) params <- c(params, filter)
  
  req <- httr2::req_url_query(req, !!!params, .multi = "comma")
  resp <- httr2::req_perform(req)
  
  if (has_geom) {
    sf_obj <- geojsonsf::geojson_sf(httr2::resp_body_string(resp))
    if (geom_info$crs != 4326L) {
      suppressWarnings({
        sf::st_crs(sf_obj) <- geom_info$crs
      })
    }
    sf_obj
  } else {
    tibble::as_tibble(httr2::resp_body_json(resp, simplifyVector = TRUE))
  }
}

#' Reset the cached PostgREST schema
#' @export
lt_cache_reset <- function() {
  rm(list = ls(envir = .lt_schema_cache), envir = .lt_schema_cache)
  invisible(NULL)
}

#' @noRd
lt_detect_geom <- function(table, base_url = "https://api.limnotrack.com/postgrest") {
  cache_key <- paste0(base_url, ":spec")
  if (!exists(cache_key, envir = .lt_schema_cache)) {
    resp <- httr2::request(base_url) |>
      httr2::req_headers(Accept = "application/openapi+json") |>
      httr2::req_perform()
    assign(cache_key, httr2::resp_body_json(resp), envir = .lt_schema_cache)
  }
  
  spec  <- get(cache_key, envir = .lt_schema_cache)
  props <- spec$definitions[[table]]$properties
  if (is.null(props)) return(NULL)
  
  geom_cols <- Filter(function(p) isTRUE(grepl("geometry", p$format)), props)
  if (length(geom_cols) == 0) return(NULL)
  
  fmt       <- geom_cols[[1]]$format
  crs_match <- regmatches(fmt, regexpr("\\d+(?=\\))", fmt, perl = TRUE))
  crs       <- if (length(crs_match) == 1) as.integer(crs_match) else 4326L
  
  list(col = names(geom_cols)[[1]], crs = crs)
}

#' Helper to construct filter parameters for lt_fetch
#' 
#' @description
#' This function takes R expressions like `col == "value"` and converts them 
#' into the format expected by the API (e.g., `list(col = "eq.value")`). It 
#' supports basic comparison operators (`==`, `!=`, `>`, `<`, `>=`, `<=`) and 
#' `%in%`. It also handles `is.na(col)` and `!is.na(col)` for null checks.
#' 
#' @param ... R expressions representing filter conditions (e.g., `col == "value"`).
#' 
#' @returns A named list of filter conditions formatted for the API query.
#' 
#' @examples
#' # Example usage of lt_filter - Get Lake Rotoehu contours with a specific 
#' # lernzmp_id
#' filters <- lt_filter(lernzmp_id == "LID40188")
#' 
#' @export
lt_filter <- function(...) {
  exprs <- match.call(expand.dots = FALSE)$`...`
  do.call(c, lapply(exprs, parse_expr))
}

#' @noRd
parse_expr <- function(expr) {
  if (!is.call(expr)) stop("Expected a filter expression e.g. col == 'value'")
  
  op  <- as.character(expr[[1]])
  col <- as.character(expr[[2]])  # left-hand side as a string, not evaluated
  
  # Handle is.na(col) and !is.na(col) — different structure
  if (op == "is.na")  return(stats::setNames(list("is.null"),     col))
  if (op == "!")      return(stats::setNames(list("not.is.null"), as.character(expr[[2]][[2]])))
  
  val <- eval(expr[[3]], parent.frame(2))  # right-hand side, evaluated
  
  pg_val <- switch(op,
                   "=="   = paste0("eq.", val),
                   "!="   = paste0("neq.", val),
                   ">"    = paste0("gt.", val),
                   ">="   = paste0("gte.", val),
                   "<"    = paste0("lt.", val),
                   "<="   = paste0("lte.", val),
                   "%in%" = paste0("in.(", paste(val, collapse = ","), ")"),
                   stop("Unsupported operator: ", op)
  )
  
  stats::setNames(list(pg_val), col)
}


#' List available tables in the Limnotrack database
#'
#' @param base_url character; base URL of the PostgREST API
#' @return a character vector of table names
#' @export
lt_tables <- function(base_url = "https://api.limnotrack.com/postgrest") {
  cache_key <- paste0(base_url, ":spec")
  if (!exists(cache_key, envir = .lt_schema_cache)) {
    resp <- httr2::request(base_url) |>
      httr2::req_headers(Accept = "application/openapi+json") |>
      httr2::req_perform()
    assign(cache_key, httr2::resp_body_json(resp), envir = .lt_schema_cache)
  }
  
  spec <- get(cache_key, envir = .lt_schema_cache)
  sort(names(spec$definitions))
}

#' Inspect the schema for a table in the Limnotrack database
#'
#' @param table character; table name
#' @param base_url character; base URL of the PostgREST API
#' @return a tibble with column names, types, formats and descriptions
#' @export
lt_schema <- function(table, base_url = "https://api.limnotrack.com/postgrest") {
  cache_key <- paste0(base_url, ":spec")
  if (!exists(cache_key, envir = .lt_schema_cache)) {
    resp <- httr2::request(base_url) |>
      httr2::req_headers(Accept = "application/openapi+json") |>
      httr2::req_perform()
    assign(cache_key, httr2::resp_body_json(resp), envir = .lt_schema_cache)
  }
  
  spec  <- get(cache_key, envir = .lt_schema_cache)
  props <- spec$definitions[[table]]$properties
  
  if (is.null(props)) {
    stop("Table '", table, "' not found. Use lt_tables() to see available tables.")
  }
  
  tibble::tibble(
    column      = names(props),
    type        = vapply(props, function(p) p$type %||% NA_character_,        character(1)),
    format      = vapply(props, function(p) p$format %||% NA_character_,      character(1)),
    description = vapply(props, function(p) p$description %||% NA_character_, character(1))
  )
}
