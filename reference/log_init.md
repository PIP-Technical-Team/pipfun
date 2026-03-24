# Initialize a new log

Creates a new named log as an empty `piplog` data.table to store log
entries. If the log already exists, it will be reset (unless
`overwrite = FALSE`).

## Usage

``` r
log_init(
  name = getOption("pipfun.log.default"),
  overwrite = getOption("pipfun.log_init.ow")
)
```

## Arguments

- name:

  Name of the log to create (default:
  `getOption("pipfun.log.default")`).

- overwrite:

  Whether to overwrite an existing log with the same name (default:
  `getOption("pipfun.log_init.ow")`).

## Value

Invisibly returns the initialized log name as a character string.

## Details

The log is stored in the internal `.piplogenv` environment and can be
retrieved with
[`log_get()`](https://pip-technical-team.github.io/pipfun/reference/log_get.md),
filtered with
[`log_filter()`](https://pip-technical-team.github.io/pipfun/reference/log_filter.md),
saved with
[`log_save()`](https://pip-technical-team.github.io/pipfun/reference/log_save.md),
or reset with
[`log_reset()`](https://pip-technical-team.github.io/pipfun/reference/log_reset.md).

## Examples

``` r
if (FALSE) { # \dontrun{
log_init("testlog")
# This checks whether it already exists
log_init("testlog", overwrite = FALSE)
} # }
```
