# Save a log to disk

Saves a log from `.piplogenv` to disk using the stamp package.
Optionally attach metadata and code hashes. If `id` lacks an extension,
one is added based on the `format` parameter.

## Usage

``` r
log_save(
  name = getOption("pipfun.log.default", "default"),
  id = name,
  format = "qs2",
  metadata = list(),
  code = NULL,
  alias = NULL,
  ...
)
```

## Arguments

- name:

  Name of the log in memory (default:
  `getOption("pipfun.log.default", "default")`).

- id:

  File identifier or path (extension optional). Defaults to `name`.

- format:

  File format (default: `"qs2"`). Used to add extension if `id` lacks
  one.

- metadata:

  Optional named list of metadata to attach to the saved file.

- code:

  Optional code object whose hash will be stored in metadata.

- alias:

  Optional stamp alias to select which catalog/versions to use.

- ...:

  Additional arguments forwarded to
  [`stamp::st_save()`](https://randrescastaneda.github.io/stamp/reference/st_save.html).

## Value

Invisibly, the result returned by
[`stamp::st_save()`](https://randrescastaneda.github.io/stamp/reference/st_save.html).

## Details

The log is saved with metadata including its class (`"piplog"`), the log
name, and the save timestamp. Additional metadata can be provided via
the `metadata` parameter.

## Examples

``` r
if (FALSE) { # \dontrun{
log_init("mylog")
log_save("mylog", id = "results", metadata = list(run_id = "exp_001"))
} # }
```
