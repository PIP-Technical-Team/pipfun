# Add gls list to the global envirnment. To be used in zzz.R in other packages

if you don't the official value in `Sys.getenv("PIP_ROOT_DIR")` you can
provide the object `root_dir <- "<you directory>"` before executing the
first function. In this way, object `gls`, which is a promise, will be
created using with you `root_dir`. Otherwise, you can specify the
complete directory path for each function.

## Usage

``` r
add_gls_to_env(
  root_dir = NULL,
  out_dir = NULL,
  vintage = "latest",
  clean = FALSE
)
```

## Arguments

- root_dir:

  character: root directory of the PIP data

- out_dir:

  character: Output Directory. Default is `root_dir`

- vintage:

  character: name of output folder. It could be "latest", "new", or any
  other name. if it is "latest" (default), the most recent version
  available in the vintage directory of the form "%Y%m%d" will be used.
  If it is "new", a new folder with a name of the form "%Y%m%d" will be
  created. All the names will be coerced to lower cases

- clean:

  logical: if TRUE it cleans all empty directories that have been
  created by mistake. Default is FALSE.

## Value

TRUE
