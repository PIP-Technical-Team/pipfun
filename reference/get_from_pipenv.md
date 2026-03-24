# Get a value from .pipenv

Get a value from .pipenv

## Usage

``` r
get_from_pipenv(key)
```

## Arguments

- key:

  A character string representing the key

## Value

The value associated with the key in .pipenv

## Examples

``` r
set_in_pipenv("example_key", 42)
get_from_pipenv("example_key") # returns 42
#> [1] 42
```
