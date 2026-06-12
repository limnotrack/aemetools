# Helper to construct filter parameters for lt_fetch

This function takes R expressions like `col == "value"` and converts
them into the format expected by the API (e.g.,
`list(col = "eq.value")`). It supports basic comparison operators (`==`,
`!=`, `>`, `<`, `>=`, `<=`) and `%in%`. It also handles `is.na(col)` and
`!is.na(col)` for null checks.

## Usage

``` r
lt_filter(...)
```

## Arguments

- ...:

  R expressions representing filter conditions (e.g., `col == "value"`).

## Value

A named list of filter conditions formatted for the API query.

## Examples

``` r
# Example usage of lt_filter - Get Lake Rotoehu contours with a specific 
# lernzmp_id
filters <- lt_filter(lernzmp_id == "LID40188")
```
