# Create auxiliary directories for new release

This function is an informative wrapper around
[fs::dir_create](https://fs.r-lib.org/reference/create.html)

## Usage

``` r
create_aux_dir(
  aux_versions,
  working_dir = fs::path(Sys.getenv("PIP_ROOT_DIR"), getOption("pipfun.working_dir"))
)

create_pc_dir(
  pc_versions,
  working_dir = fs::path(Sys.getenv("PIP_ROOT_DIR"), getOption("pipfun.working_dir"))
)

create_dir(wdir, dirs, verbose = getOption("pipfun.verbose"))

remove_dir(wdir, dirs, verbose = getOption("pipfun.verbose"))
```

## Arguments

- aux_versions:

  character: name of auxiliary folders. they must come in the form
  "%Y%m%d\_`identify`", where `identify` stands for
  `c("PROD", "INT", "TEST")`

- working_dir:

  character: Working directory where files will be created. Defaults to
  a subdirectory of `root_dir`

- pc_versions:

  character: name of auxiliary folders. they must come in the form
  "%Y%m%d_YYYY_MM_AA\_`identify`", where `YYYY` stands for the PPP year,
  `MM` stands for the master version of the PPPs, `AA` refers to the
  adaptation version of the PPPs, and `identify` stands for one of
  `c("PROD", "INT", "TEST")`

- wdir:

  chracter: working directory path

- dirs:

  chracter: directories to be created inside or removed from `wdir`

- verbose:

  A logical: whether to print detailed messages about the process. The
  default is `TRUE`

## Value

logical vector. the names of the elements correspond to the directory
paths
