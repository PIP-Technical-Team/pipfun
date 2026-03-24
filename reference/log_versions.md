# List available versions of a saved log

Lists all saved versions of a log stored on disk using stamp.

## Usage

``` r
log_versions(dir, id, format = "qs2")
```

## Arguments

- dir:

  Directory where the log is stored.

- id:

  File identifier (without extension).

- format:

  File format (default: "qs2").

## Value

A data.table of available versions.
