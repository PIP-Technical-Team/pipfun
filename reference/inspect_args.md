# Argument Inspector

introspects how arguments are being resolved - showing what was passed
explicitly, what was set by default, and what's visible in the
environment at runtime. This is super helpful when trying to debug or
understand how log_add() captures arguments using environment()

## Usage

``` r
inspect_args(.env = parent.frame())
```

## Arguments

- .env:

  environment where the arguments are coming from

## Value

`inspect_args` class object
