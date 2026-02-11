# Get the Pareto front from a data frame based on specified objective columns.

Get the Pareto front from a data frame based on specified objective
columns.

## Usage

``` r
get_pareto_front(df, obj_cols)
```

## Arguments

- df:

  A data frame containing the results of a multi-objective optimization.

- obj_cols:

  A character vector of column names in df that represent the objective
  values to be minimized. The function will identify the rows that are
  not dominated by any other row in terms of these objective values.

## Value

A data frame containing only the rows that are on the Pareto front.
