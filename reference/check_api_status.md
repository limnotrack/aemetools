# Check API status

This function checks the availability of the LimnoTrack API by sending a
request to the health endpoint. It returns TRUE if the API is available
(HTTP 200), FALSE otherwise, and provides informative messages about the
status.

## Usage

``` r
check_api_status(api_url = "https://api.limnotrack.com")
```

## Arguments

- api_url:

  character; base URL of the API.

## Value

TRUE if the API is available, FALSE otherwise.

## Examples

``` r
check_api_status()
#> API is available
#> Database connection is healthy
#> [1] TRUE
```
