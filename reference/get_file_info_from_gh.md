# Get info of a file or files within a folder in a Github repo

Get info of a file or files within a folder in a Github repo

## Usage

``` r
get_file_info_from_gh(
  owner = getOption("pipfun.ghowner"),
  repo,
  branch = "main",
  file_path
)
```

## Arguments

- owner:

  character: owner of repo

- repo:

  character: repository name

- branch:

  character: branch where the file or folder is

- file_path:

  character: file or folder path

## Value

Complete response from GET method of Github API

## Examples

``` r
get_file_info_from_gh(owner     = getOption("pipfun.ghowner"),
                      repo      = "pip_info",
                      file_path = "releases.csv",
                      branch    = "releases")
#> Git credentials are missing or invalid in non-interactive mode.
#> $name
#> [1] "releases.csv"
#> 
#> $path
#> [1] "releases.csv"
#> 
#> $sha
#> [1] "6c786066f04c39724441d833b334faf37321615f"
#> 
#> $size
#> [1] 367
#> 
#> $url
#> [1] "https://api.github.com/repos/PIP-Technical-Team/pip_info/contents/releases.csv?ref=releases"
#> 
#> $html_url
#> [1] "https://github.com/PIP-Technical-Team/pip_info/blob/releases/releases.csv"
#> 
#> $git_url
#> [1] "https://api.github.com/repos/PIP-Technical-Team/pip_info/git/blobs/6c786066f04c39724441d833b334faf37321615f"
#> 
#> $download_url
#> [1] "https://raw.githubusercontent.com/PIP-Technical-Team/pip_info/releases/releases.csv"
#> 
#> $type
#> [1] "file"
#> 
#> $content
#> [1] "cmVsZWFzZSxpZGVudGl0eQoyMDI0MDMyNixQUk9ECjIwMjQxMTAxLFBST0QK\nMjAyNTAyMDMsVEVTVAoyMDI1MDUwMSxURVNUCjIwMjUwNzExLFRFU1QKMjAy\nNTA3MTcsVEVTVAoyMDI1MDcxOCxURVNUCjIwMjUwNzIxLFRFU1QKMjAyNTA3\nMjUsVEVTVAoyMDI1MDczMCxURVNUCjIwMjUwODA1LFRFU1QKMjAyNTA4MDYs\nVEVTVAoyMDI1MDgwNyxURVNUCjIwMjUwODA4LFRFU1QKMjAyNTA4MTAsVEVT\nVAoyMDI1MDgxMSxURVNUCjIwMjUxMjA1LFRFU1QKMjAyNTEyMTEsVEVTVAoy\nMDI2MDIwMyxURVNUCjIwMjYwMjA0LFRFU1QKMjAyNjAyMDYsVEVTVAoyMDI2\nMDIwMixURVNUCjIwMjYwMTAxLFRFU1QKMjAyNjAyMjEsVEVTVAoyMDI2MDIy\nMyxURVNUCg==\n"
#> 
#> $encoding
#> [1] "base64"
#> 
#> $`_links`
#> $`_links`$self
#> [1] "https://api.github.com/repos/PIP-Technical-Team/pip_info/contents/releases.csv?ref=releases"
#> 
#> $`_links`$git
#> [1] "https://api.github.com/repos/PIP-Technical-Team/pip_info/git/blobs/6c786066f04c39724441d833b334faf37321615f"
#> 
#> $`_links`$html
#> [1] "https://github.com/PIP-Technical-Team/pip_info/blob/releases/releases.csv"
#> 
#> 
#> $owner
#> [1] "PIP-Technical-Team"
#> 
#> $repo
#> [1] "pip_info"
#> 
#> $branch
#> [1] "releases"
#> 

get_file_info_from_gh(owner     = getOption("pipfun.ghowner"),
                      repo      = "pipfaker",
                      file_path = "data/20240627_2017_01_02_PROD/_aux",
                      branch    = "aux_estimations")
#> Git credentials are missing or invalid in non-interactive mode.
#> {
#>   "aux_versions.fst": {
#>     "name": "aux_versions.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/aux_versions.fst",
#>     "sha": "a044cf589ac5c295f05af86594b8882be5a86761",
#>     "size": 1066,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/aux_versions.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/aux_versions.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/a044cf589ac5c295f05af86594b8882be5a86761",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/aux_versions.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/aux_versions.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/a044cf589ac5c295f05af86594b8882be5a86761",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/aux_versions.fst"
#>     }
#>   },
#>   "countries.fst": {
#>     "name": "countries.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/countries.fst",
#>     "sha": "b110f1567e88d42ba6c81c604e13be91f6ba2b20",
#>     "size": 15718,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/countries.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/countries.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/b110f1567e88d42ba6c81c604e13be91f6ba2b20",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/countries.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/countries.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/b110f1567e88d42ba6c81c604e13be91f6ba2b20",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/countries.fst"
#>     }
#>   },
#>   "country_coverage.fst": {
#>     "name": "country_coverage.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/country_coverage.fst",
#>     "sha": "7557bec597aa4fba5d5af028266e656c405430fa",
#>     "size": 1037046,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/country_coverage.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/country_coverage.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/7557bec597aa4fba5d5af028266e656c405430fa",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/country_coverage.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/country_coverage.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/7557bec597aa4fba5d5af028266e656c405430fa",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/country_coverage.fst"
#>     }
#>   },
#>   "country_list.fst": {
#>     "name": "country_list.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/country_list.fst",
#>     "sha": "5150fa3472857799fd7ee85385946e8f44797c2c",
#>     "size": 27447,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/country_list.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/country_list.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/5150fa3472857799fd7ee85385946e8f44797c2c",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/country_list.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/country_list.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/5150fa3472857799fd7ee85385946e8f44797c2c",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/country_list.fst"
#>     }
#>   },
#>   "cpi.fst": {
#>     "name": "cpi.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/cpi.fst",
#>     "sha": "08460d56292db8ad4b67e3c1d5ee482de364f9a4",
#>     "size": 70842,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/cpi.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/cpi.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/08460d56292db8ad4b67e3c1d5ee482de364f9a4",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/cpi.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/cpi.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/08460d56292db8ad4b67e3c1d5ee482de364f9a4",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/cpi.fst"
#>     }
#>   },
#>   "decomposition.fst": {
#>     "name": "decomposition.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/decomposition.fst",
#>     "sha": "bfc069e9578f66d48e7c44f4bf6d94af978cbe0d",
#>     "size": 557,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/decomposition.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/decomposition.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/bfc069e9578f66d48e7c44f4bf6d94af978cbe0d",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/decomposition.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/decomposition.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/bfc069e9578f66d48e7c44f4bf6d94af978cbe0d",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/decomposition.fst"
#>     }
#>   },
#>   "dictionary.fst": {
#>     "name": "dictionary.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/dictionary.fst",
#>     "sha": "d420585bf9c17b2f170abca5eba542d2a6dd96f3",
#>     "size": 3848,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/dictionary.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/dictionary.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/d420585bf9c17b2f170abca5eba542d2a6dd96f3",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/dictionary.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/dictionary.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/d420585bf9c17b2f170abca5eba542d2a6dd96f3",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/dictionary.fst"
#>     }
#>   },
#>   "framework.fst": {
#>     "name": "framework.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/framework.fst",
#>     "sha": "d8d8999bbd69ef46f5e0b1fc42828bd8c323f154",
#>     "size": 597194,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/framework.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/framework.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/d8d8999bbd69ef46f5e0b1fc42828bd8c323f154",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/framework.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/framework.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/d8d8999bbd69ef46f5e0b1fc42828bd8c323f154",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/framework.fst"
#>     }
#>   },
#>   "gdp.fst": {
#>     "name": "gdp.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/gdp.fst",
#>     "sha": "69b67353fa917d8fe0e2c5247d24890683a544d4",
#>     "size": 91616,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/gdp.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/gdp.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/69b67353fa917d8fe0e2c5247d24890683a544d4",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/gdp.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/gdp.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/69b67353fa917d8fe0e2c5247d24890683a544d4",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/gdp.fst"
#>     }
#>   },
#>   "incgrp_coverage.fst": {
#>     "name": "incgrp_coverage.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/incgrp_coverage.fst",
#>     "sha": "4d893cd8c4304f6a6adfcf99f1bbe1fa00f3c169",
#>     "size": 1713,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/incgrp_coverage.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/incgrp_coverage.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/4d893cd8c4304f6a6adfcf99f1bbe1fa00f3c169",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/incgrp_coverage.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/incgrp_coverage.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/4d893cd8c4304f6a6adfcf99f1bbe1fa00f3c169",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/incgrp_coverage.fst"
#>     }
#>   },
#>   "indicators.fst": {
#>     "name": "indicators.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/indicators.fst",
#>     "sha": "eb48461ebda5a537985734fffcce30666020035d",
#>     "size": 24083,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/indicators.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/indicators.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/eb48461ebda5a537985734fffcce30666020035d",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/indicators.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/indicators.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/eb48461ebda5a537985734fffcce30666020035d",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/indicators.fst"
#>     }
#>   },
#>   "interpolated_means.fst": {
#>     "name": "interpolated_means.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/interpolated_means.fst",
#>     "sha": "740bec86f0f6b977ea5b6cb414a68dff42808b96",
#>     "size": 3077700,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/interpolated_means.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/interpolated_means.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/740bec86f0f6b977ea5b6cb414a68dff42808b96",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/interpolated_means.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/interpolated_means.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/740bec86f0f6b977ea5b6cb414a68dff42808b96",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/interpolated_means.fst"
#>     }
#>   },
#>   "metaregion.fst": {
#>     "name": "metaregion.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/metaregion.fst",
#>     "sha": "203ee4d70bd010f4a00518922ccf9eb11b8ea01c",
#>     "size": 560,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/metaregion.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/metaregion.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/203ee4d70bd010f4a00518922ccf9eb11b8ea01c",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/metaregion.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/metaregion.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/203ee4d70bd010f4a00518922ccf9eb11b8ea01c",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/metaregion.fst"
#>     }
#>   },
#>   "missing_data.fst": {
#>     "name": "missing_data.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/missing_data.fst",
#>     "sha": "43ffc78d6a41a7bea4d4c74a7c40c9f5b87bdad3",
#>     "size": 67694,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/missing_data.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/missing_data.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/43ffc78d6a41a7bea4d4c74a7c40c9f5b87bdad3",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/missing_data.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/missing_data.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/43ffc78d6a41a7bea4d4c74a7c40c9f5b87bdad3",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/missing_data.fst"
#>     }
#>   },
#>   "national_poverty_lines.fst": {
#>     "name": "national_poverty_lines.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/national_poverty_lines.fst",
#>     "sha": "5c6b815d64e7354203d2cc890cd2ca50f20809c5",
#>     "size": 329994,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/national_poverty_lines.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/national_poverty_lines.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/5c6b815d64e7354203d2cc890cd2ca50f20809c5",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/national_poverty_lines.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/national_poverty_lines.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/5c6b815d64e7354203d2cc890cd2ca50f20809c5",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/national_poverty_lines.fst"
#>     }
#>   },
#>   "pce.fst": {
#>     "name": "pce.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/pce.fst",
#>     "sha": "90c24ee66a4154bf5511e7e7c083a02cb79888f4",
#>     "size": 74439,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/pce.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/pce.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/90c24ee66a4154bf5511e7e7c083a02cb79888f4",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/pce.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/pce.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/90c24ee66a4154bf5511e7e7c083a02cb79888f4",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/pce.fst"
#>     }
#>   },
#>   "pg_lnp.fst": {
#>     "name": "pg_lnp.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/pg_lnp.fst",
#>     "sha": "ed39ec2f50c8ac1533d2a3dc5856ced511238dd5",
#>     "size": 127913,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/pg_lnp.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/pg_lnp.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/ed39ec2f50c8ac1533d2a3dc5856ced511238dd5",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/pg_lnp.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/pg_lnp.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/ed39ec2f50c8ac1533d2a3dc5856ced511238dd5",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/pg_lnp.fst"
#>     }
#>   },
#>   "pg_svy.fst": {
#>     "name": "pg_svy.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/pg_svy.fst",
#>     "sha": "cfcc7234c8df3714553210bb3d2e8e2d23b1a2da",
#>     "size": 47033,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/pg_svy.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/pg_svy.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/cfcc7234c8df3714553210bb3d2e8e2d23b1a2da",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/pg_svy.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/pg_svy.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/cfcc7234c8df3714553210bb3d2e8e2d23b1a2da",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/pg_svy.fst"
#>     }
#>   },
#>   "pop.fst": {
#>     "name": "pop.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/pop.fst",
#>     "sha": "a2d6dfb9da55bab0ac419d9fcddf44d50a2215be",
#>     "size": 263740,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/pop.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/pop.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/a2d6dfb9da55bab0ac419d9fcddf44d50a2215be",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/pop.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/pop.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/a2d6dfb9da55bab0ac419d9fcddf44d50a2215be",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/pop.fst"
#>     }
#>   },
#>   "pop_region.fst": {
#>     "name": "pop_region.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/pop_region.fst",
#>     "sha": "d6184f3db5c420516386498d95772a7a356909e5",
#>     "size": 10359,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/pop_region.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/pop_region.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/d6184f3db5c420516386498d95772a7a356909e5",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/pop_region.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/pop_region.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/d6184f3db5c420516386498d95772a7a356909e5",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/pop_region.fst"
#>     }
#>   },
#>   "poverty_lines.fst": {
#>     "name": "poverty_lines.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/poverty_lines.fst",
#>     "sha": "f5a747a4fa123f7cdbbdc0dcc5d4260dba80ccbe",
#>     "size": 3285,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/poverty_lines.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/poverty_lines.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/f5a747a4fa123f7cdbbdc0dcc5d4260dba80ccbe",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/poverty_lines.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/poverty_lines.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/f5a747a4fa123f7cdbbdc0dcc5d4260dba80ccbe",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/poverty_lines.fst"
#>     }
#>   },
#>   "ppp.fst": {
#>     "name": "ppp.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/ppp.fst",
#>     "sha": "b8d71b50ca3bdbb81c433d4672dd2a081e1a82bd",
#>     "size": 6232,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/ppp.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/ppp.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/b8d71b50ca3bdbb81c433d4672dd2a081e1a82bd",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/ppp.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/ppp.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/b8d71b50ca3bdbb81c433d4672dd2a081e1a82bd",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/ppp.fst"
#>     }
#>   },
#>   "region_coverage.fst": {
#>     "name": "region_coverage.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/region_coverage.fst",
#>     "sha": "fe2a42df806a18f511142502b10257d636b5586f",
#>     "size": 11665,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/region_coverage.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/region_coverage.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/fe2a42df806a18f511142502b10257d636b5586f",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/region_coverage.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/region_coverage.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/fe2a42df806a18f511142502b10257d636b5586f",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/region_coverage.fst"
#>     }
#>   },
#>   "regions.fst": {
#>     "name": "regions.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/regions.fst",
#>     "sha": "a3f610a0664b22f765f290999af5dd1740ce2286",
#>     "size": 891,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/regions.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/regions.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/a3f610a0664b22f765f290999af5dd1740ce2286",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/regions.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/regions.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/a3f610a0664b22f765f290999af5dd1740ce2286",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/regions.fst"
#>     }
#>   },
#>   "spr_lnp.fst": {
#>     "name": "spr_lnp.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/spr_lnp.fst",
#>     "sha": "cb4d00d551bfdef62b05a3f1f4256ba734448f7e",
#>     "size": 241850,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/spr_lnp.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/spr_lnp.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/cb4d00d551bfdef62b05a3f1f4256ba734448f7e",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/spr_lnp.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/spr_lnp.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/cb4d00d551bfdef62b05a3f1f4256ba734448f7e",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/spr_lnp.fst"
#>     }
#>   },
#>   "spr_svy.fst": {
#>     "name": "spr_svy.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/spr_svy.fst",
#>     "sha": "8a0075f69f28e7796ba970c0bd0d49e34b38dd21",
#>     "size": 89290,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/spr_svy.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/spr_svy.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/8a0075f69f28e7796ba970c0bd0d49e34b38dd21",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/spr_svy.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/spr_svy.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/8a0075f69f28e7796ba970c0bd0d49e34b38dd21",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/spr_svy.fst"
#>     }
#>   },
#>   "survey_means.fst": {
#>     "name": "survey_means.fst",
#>     "path": "data/20240627_2017_01_02_PROD/_aux/survey_means.fst",
#>     "sha": "e9fc87488d2d77101db80b185621c0c229669615",
#>     "size": 782510,
#>     "url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/survey_means.fst?ref=aux_estimations",
#>     "html_url": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/survey_means.fst",
#>     "git_url": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/e9fc87488d2d77101db80b185621c0c229669615",
#>     "download_url": "https://raw.githubusercontent.com/PIP-Technical-Team/pipfaker/aux_estimations/data/20240627_2017_01_02_PROD/_aux/survey_means.fst",
#>     "type": "file",
#>     "_links": {
#>       "self": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/contents/data/20240627_2017_01_02_PROD/_aux/survey_means.fst?ref=aux_estimations",
#>       "git": "https://api.github.com/repos/PIP-Technical-Team/pipfaker/git/blobs/e9fc87488d2d77101db80b185621c0c229669615",
#>       "html": "https://github.com/PIP-Technical-Team/pipfaker/blob/aux_estimations/data/20240627_2017_01_02_PROD/_aux/survey_means.fst"
#>     }
#>   }
#> } 
```
