# Add a log entry

Adds a structured entry to a named log, capturing metadata such as
timestamp, calling function, event type, message, arguments used,
optional output, and more.

## Usage

``` r
log_add(
  event,
  message,
  name = getOption("pipfun.log.default"),
  args = NULL,
  logmeta = NULL,
  output = NULL,
  .trace = NULL,
  .env = rlang::caller_env()
)
```

## Arguments

- event:

  Type of event (e.g. `"error"`, `"info"`, `"warning"`).

- message:

  Description of the log entry.

- name:

  Name of the log (default: `options("pipfun.log.default")`).

- args:

  Optional list of captured arguments (default: auto-captured). If
  `NULL`, arguments are automatically extracted from the calling
  function.

- logmeta:

  Optional named list of metadata to attach. Merged with `args` in the
  final log entry.

- output:

  Optional result or return value to store in the log.

- .trace:

  Optional call stack or trace override. If `NULL`, uses `sys.call(-1)`.

- .env:

  Internal use. Calling environment (default:
  [`rlang::caller_env()`](https://rlang.r-lib.org/reference/stack.html)).

## Value

Invisibly returns `TRUE` on success.

## Details

This function automatically captures all arguments from the calling
function, including `...`. You can also manually add custom metadata
using the `logmeta` argument. The `logmeta` list is merged with captured
`args`.

## Examples

``` r
log_init("demo_log", overwrite = TRUE)

# Automatically captures arguments from the caller:
my_fun <- function(x, y = 1, ...) {
  result <- x + y
  log_info("Ran my_fun", name = "demo_log", output = result)
  return(result)
}
my_fun(3, z = 9)
#> [1] 4

# Add custom metadata manually:
log_info("Logging manually", name = "demo_log",
         logmeta = list(stage = "processing", user = "analyst"))
```
