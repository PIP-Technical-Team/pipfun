# Load a log from disk

Loads a previously saved `piplog` from disk using the stamp package.
Validates the loaded object and restores it to `.piplogenv` with a given
name. If `id` lacks an extension, one is added based on the `format`
parameter.

## Usage

``` r
log_load(
  id,
  name = id,
  version = NULL,
  format = "qs2",
  overwrite = FALSE,
  verbose = TRUE,
  alias = NULL
)
```

## Arguments

- id:

  File identifier or path (extension optional).

- name:

  Name to assign to the log in memory (default: `id`).

- version:

  Optional version identifier or `"available"` to list versions. Passed
  to
  [`stamp::st_load()`](https://randrescastaneda.github.io/stamp/reference/st_load.html).
  Default loads the latest version.

- format:

  File format (default: `"qs2"`). Used to add extension if `id` lacks
  one.

- overwrite:

  Logical: whether to overwrite an existing log in memory with the same
  name (default: `FALSE`).

- verbose:

  Logical: whether to announce loading progress (default: `TRUE`).

- alias:

  Optional stamp alias to select which catalog/versions to use.

## Value

The loaded `piplog` object (visibly), or if `version = "available"`, a
`data.table` of available versions with a `vintage` column.

## Details

Use `version = "available"` to list all available versions of the log
file without loading it.

## Examples

``` r
if (FALSE) { # \dontrun{
log_load("results", name = "loaded_log")
log_load("results", version = "available")  # List versions
} # }
```
