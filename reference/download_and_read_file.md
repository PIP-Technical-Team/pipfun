# Download and read file

Helper function to handle file downloads and reading

## Usage

``` r
download_and_read_file(url, creds = NULL)
```

## Arguments

- url:

  character: url of file. usually it comes
  `get_file_info_from_gh()$download_url`

- creds:

  list. Basically, it is
  [`get_github_creds()`](https://pip-technical-team.github.io/pipfun/reference/get_github_creds.md)

## Value

data in data.table format
