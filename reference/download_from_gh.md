# Download file from Github

Download file from Github

## Usage

``` r
download_from_gh(url, temp_file, creds = NULL)
```

## Arguments

- url:

  character: url of file. usually it comes
  `get_file_info_from_gh()$download_url`

- temp_file:

  [`tempfile()`](https://rdrr.io/r/base/tempfile.html) where new file
  will be saved

- creds:

  list. Basically, it is
  [`get_github_creds()`](https://pip-technical-team.github.io/pipfun/reference/get_github_creds.md)

## Value

file of extension in path
