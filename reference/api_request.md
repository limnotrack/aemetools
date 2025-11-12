# Request data from the LERNZMP API

Handles authentication, error checking, and returns the raw response.

## Usage

``` r
api_request(
  api_url,
  endpoint,
  query = list(),
  api_key = NULL,
  headers = list()
)
```

## Arguments

- api_url:

  character; base URL of the API.

- endpoint:

  character; specific API endpoint to call.

- query:

  list; named list of query parameters.

- api_key:

  character; API key for authentication. If NULL, will look for the key
  in the LERNZMP_KEY environment variable. If that is not set, will
  throw an error.

- headers:

  list; additional headers to include in the request. Defaults to an
  empty list.

## Value

The raw response object from httr2; parsing is left to the caller.
