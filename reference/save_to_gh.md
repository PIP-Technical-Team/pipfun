# Save to GitHub

This function uploads or updates a file in a GitHub repository. If the
file does not already exist, a new file will be created. If the file
already exists, it will be updated with the new data.

## Usage

``` r
save_to_gh(
  df,
  repo,
  owner = getOption("pipfun.ghowner"),
  branch = "DEV",
  filename = repo,
  ext = "csv",
  metadata = NULL,
  verbose = TRUE,
  message = paste("Updating data via R script on", Sys.time())
)
```

## Arguments

- df:

  A dataframe containing the data to be uploaded or used to update an
  existing file. The dataframe will be converted into a base64-encoded
  string before being uploaded

- repo:

  A character string specifying the name of the GitHub repo where the
  file will be uploaded or updated

- owner:

  A character string specifying the GitHub username or organization that
  owns the repository. Defaults to `pipfun.ghowner` option

- branch:

  A character string specifying the branch of the repository where the
  file should be uploaded or updated. The default is `DEV` branch

- filename:

  A character string specifying the name of the file to be created or
  updated in the GitHub repository. If not provided, it defaults to repo
  name

- ext:

  A character string representing the file extension (e.g., `.csv`).
  Default is `csv`

- metadata:

  A list containing metadata for an existing file in the repository.
  Usually from
  [get_pip_releases](https://pip-technical-team.github.io/pipfun/reference/get_pip_releases.md)
  It should contain `sha` (the SHA hash of the file) and `path` (the
  file path in the repository). If `NULL`, the function will check
  whether the file exists and retrieve the metadata

- verbose:

  A logical: whether to print detailed messages about the process. The
  default is `TRUE`

- message:

  A character string specifying the commit message for the GitHub upload
  or update. The default is a message with the current timestamp

## Value

Returns `invisible(NULL)`. The function primarily performs an upload or
update operation and does not return any value other than invisibly
indicating the completion of the task.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Create a new file on GitHub
  df <- data.frame(a = 1:5, b = letters[1:5])
  save_to_gh(df = df, repo = "aux_test", filename = "data_example", ext = "csv")

  # Update an existing file on GitHub
  df <- data.frame(a = 6:10, b = letters[6:10])
  save_to_gh(df = df, repo = "aux_test", filename = "data_example", ext = "csv")
} # }
```
