# Reset or delete a log from memory

Clears a named log from the internal `.piplogenv` environment. Use this
to start fresh or free memory.

## Usage

``` r
log_reset(name = getOption("pipfun.log.default", "default"))
```

## Arguments

- name:

  Name of the log to remove (default:
  `getOption("pipfun.log.default", "default")`).

## Value

Invisibly returns `TRUE` if the log was successfully removed, `FALSE` if
the log did not exist.

## Examples

``` r
if (FALSE) { # \dontrun{
log_init("mylog")
log_reset("mylog")
} # }
```
