# Get a log

Retrieves a named log from the internal `.piplogenv` environment.
Restores the `piplog` class if it was dropped by data.table operations.

## Usage

``` r
log_get(name = getOption("pipfun.log.default"))
```

## Arguments

- name:

  Name of the log (default: `getOption("pipfun.log.default")`)

## Value

Invisibly returns the `piplog` object as a `data.table` with all log
entries.

## Examples

``` r
if (FALSE) { # \dontrun{
log_init("mylog")
my_log <- log_get("mylog")
} # }
```
