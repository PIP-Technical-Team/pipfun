# Filter log entries

Subsets a log by event type, function name, or time range. Returns a new
`piplog` object without modifying the original.

## Usage

``` r
log_filter(
  name = getOption("pipfun.log.default"),
  event = NULL,
  fun = NULL,
  after = NULL,
  before = NULL
)
```

## Arguments

- name:

  Name of the log (default: `getOption("pipfun.log.default")`)

- event:

  Type of event to filter (e.g., `"info"`, `"warning"`, `"error"`). Can
  be a character vector to match multiple event types.

- fun:

  Optional: function name(s) to filter as a character vector.

- after:

  Optional: filter entries after this datetime. Coerced to `POSIXct` if
  needed.

- before:

  Optional: filter entries before this datetime. Coerced to `POSIXct` if
  needed.

## Value

A filtered `piplog` object (a `data.table` with class `piplog`). If no
rows match the filters, returns an empty `piplog`.

## Examples

``` r
if (FALSE) { # \dontrun{
log_filter(name = "mylog", event = "error")
log_filter(name = "mylog", fun = "my_function", event = c("warning", "error"))
} # }
```
