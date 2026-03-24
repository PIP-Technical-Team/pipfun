# Retrieve and assign the active working release

Utility for functions that need access to the currently configured
working release. The function fetches the `wrk_release` object from the
package `.pipenv` environment and assigns it into the caller's frame
under `name` (default: `wrk_release`). If no working release is set, the
function aborts with an informative message.

## Usage

``` r
get_wrk_release(name = "wrk_release", verbose = getOption("pipfun.verbose"))
```

## Arguments

- name:

  character: name to assign the working release object to in the calling
  environment. Defaults to `"wrk_release"`.

- verbose:

  logical: whether to print a short confirmation message showing the
  active release.

## Value

Invisibly returns `NULL`. The primary effect is that a variable called
`name` is assigned in the calling frame containing the working release
list (with `release`, `identity`, and `ppp`).

## Examples

``` r
if (FALSE) { # \dontrun{
hello <- function() {
  get_wrk_release()
  invisible(wrk_release)
}
setup_working_release()
print(hello())
} # }
```
