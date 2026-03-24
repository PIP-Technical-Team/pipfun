# Get all files from folder in Github

Get all files from folder in Github

## Usage

``` r
load_all_from_gh(
  owner = getOption("pipfun.ghowner"),
  repo,
  branch = "main",
  folder_path,
  output_path
)
```

## Arguments

- owner:

  character: owner of repo

- repo:

  character: repository name

- branch:

  character: branch where the file or folder is

- folder_path:

  character: folder path

- output_path:

  character: folder path

## Value

file of extension in path

## Examples

``` r
if (FALSE) { # \dontrun{
load_all_from_gh(owner     = getOption("pipfun.ghowner"),
                 repo      = "pipfaker",
                 folder_path = "data/20240627_2017_01_02_PROD/_aux",
                 branch    = "aux_estimations",
                 output_path = getwd())} # }
```
