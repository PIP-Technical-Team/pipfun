# Log an error, warning, or info event

These are wrapper functions for
[`log_add()`](https://pip-technical-team.github.io/pipfun/reference/log_add.md)
to log events of type "error", "warning", or "info". They automatically
capture arguments from the parent function and include them in the log.

## Usage

``` r
log_info(
  message,
  name = getOption("pipfun.log.default"),
  output = NULL,
  .trace = NULL,
  .env = parent.frame(),
  logmeta = NULL
)

log_warn(
  message,
  name = getOption("pipfun.log.default"),
  output = NULL,
  .trace = NULL,
  .env = parent.frame(),
  logmeta = NULL
)

log_error(
  message,
  name = getOption("pipfun.log.default"),
  output = NULL,
  .trace = NULL,
  .env = parent.frame(),
  logmeta = NULL
)
```

## Arguments

- message:

  The log message.

- name:

  Name of the log (default: `pipfun.log.default`).

- output:

  Optional result or return value to include in the log.

- .trace:

  Optional trace object or call stack override.

- .env:

  Environment from which to capture arguments (default:
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html)).

- logmeta:

  Optional named list of metadata to include (tags, user info, etc.).

## Value

Invisibly returns TRUE after updating the log.

## Examples

``` r
log_init("mylog", overwrite = TRUE)

# Simulate calling context
my_function <- function(a = 1, b = 2, ...) {
  log_info("This is an info message", name = "mylog")
}
my_function(x = 42)

log_get("mylog")

log_reset("mylog")
#> ✔ Log mylog has been reset.
```
