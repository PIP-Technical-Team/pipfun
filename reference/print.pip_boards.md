# Print method for pip_boards S3 class

Displays a summary of PIP pins boards in a beautiful cli output. Shows
the common directory path, each board's name, its relative path, and
cache size.

## Usage

``` r
# S3 method for class 'pip_boards'
print(x, ...)
```

## Arguments

- x:

  A `pip_boards` object, as returned by `set_pip_boards()`.

- ...:

  Additional arguments (ignored).

## Value

Invisibly returns the input `pip_boards` object.

## Examples

``` r
if (FALSE) { # \dontrun{
boards <- set_pip_boards()
print(boards)
} # }
```
