# Inspect the schema for a table in the Limnotrack database

Inspect the schema for a table in the Limnotrack database

## Usage

``` r
lt_schema(table, base_url = "https://api.limnotrack.com/postgrest")
```

## Arguments

- table:

  character; table name

- base_url:

  character; base URL of the PostgREST API

## Value

a tibble with column names, types, formats and descriptions
