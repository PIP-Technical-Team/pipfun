# Capture arguments for logging helpers

Captures arguments from the parent function (one level up from the
helper), including `...` if present, or all visible objects in `.env`
for interactive use.

## Usage

``` r
capture_log_args(helper_name, .env)
```

## Arguments

- helper_name:

  The function object of the logging helper (e.g., `log_info`). Used to
  identify and skip the helper in the call stack.

- .env:

  The environment from which to capture arguments. Usually
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) of the
  helper.

## Value

A named list of captured arguments. If called inside a function, returns
all named and `...` arguments from the parent function. If called
interactively, returns all visible objects in `.env`.

## Details

This function is designed to be called inside logging helpers such as
`log_info`, `log_warn`, and `log_error`. It inspects the call stack to
find the true parent function (the function that called the helper), and
captures all its arguments, including any `...` arguments. If called
interactively (i.e., not inside another function), it captures all
visible objects in the provided environment, excluding hidden variables
(those starting with a dot).

## Examples

``` r
# Inside a function:
if (FALSE) { # \dontrun{
my_fun <- function(x, y = 1, ...) {
  pipfun:::capture_log_args(log_info, environment())
}
my_fun(3, z = 9)

# Interactive use:
a <- 1; b <- 2
pipfun:::capture_log_args(log_info, environment())
} # }
```
