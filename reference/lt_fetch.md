# Query LimnoTrack database with flexible parameters

This function allows you to query any table in the LimnoTrack database
with optional parameters for selecting columns, filtering rows, ordering
results, and pagination. It automatically detects if the table contains
geometry data and returns an `sf` object if so, or a regular tibble
otherwise.

## Usage

``` r
lt_fetch(
  table,
  select = NULL,
  filter = NULL,
  order = NULL,
  limit = 1000,
  offset = 0,
  base_url = "https://api.limnotrack.com/postgrest"
)
```

## Arguments

- table:

  Name of the table to query (e.g., "lake_contours").

- select:

  Optional string of comma-separated column names to select (e.g.,
  "col1,col2"). If NULL, all columns are returned.

- filter:

  Optional named list of filter conditions, where names are column names
  and values are filter expressions (e.g., list(lernzmp_id =
  "eq.LID40188")). You can use the [`lt_filter()`](lt_filter.md) helper
  to construct this list from R expressions.

- order:

  Optional string specifying the order of results (e.g.,
  "col1.asc,col2.desc"). If NULL, no specific ordering is applied.

- limit:

  Number of records to return. Default is 1000.

- offset:

  Number of records to skip for pagination. Default is 0.

- base_url:

  Base URL of the LimnoTrack API. Default is
  "https://api.limnotrack.com/postgrest".

## Value

A tibble or an `sf` object containing the query results, depending on
whether the table has geometry data.

## Examples

``` r
#' # Query lake contours with a specific lernzmp_id
contours <- lt_fetch(
 table = "lake_contours",
 filter = lt_filter(lernzmp_id == "LID40188")
 )
 
```
