# Check whether a log contains any errors

Check whether a log contains any errors

## Usage

``` r
log_has_errors(name = getOption("pipfun.log.default"), show = FALSE)
```

## Arguments

- name:

  Name of the log (default: `pipfun.log.default`)

- show:

  Logical: whether to return the filtered error log (default: FALSE).

## Value

Logical if `show = FALSE`; a filtered `piplog` object if `show = TRUE`.
