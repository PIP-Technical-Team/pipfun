# Working with PIP releases

## Introduction

The PIP workflow is built around the concept of a working release. A
working release defines:

- the release identifier

- the identity (e.g., production, testing)

- the PPP round

- the folder structure on disk

- the registered stamp aliases

- global metadata required by downstream functions

Setting a working release ensures a consistent, reproducible, and
isolated data layout for each release.

All release-related state is stored internally in the package’s private
.pipenv environment.

## 1. Set up a release

The entry point is the function
[`setup_working_release()`](https://pip-technical-team.github.io/pipfun/reference/setup_working_release.md),
which takes care of all the necessary steps to set up a working release
environment. You can specify the release identifier, identity, PPP
round, and other parameters, or simply use the defaults to set up the
latest release.

- identity must match one of getOption(“pipfun.identities”)

- ppp must match one of getOption(“pipfun.ppps”)

- Invalid values trigger informative errors.

``` r
creds <- get_github_creds()
(wr <- setup_working_release(creds = creds))
```

Notice that, by default, the working release will correspond to the
latest release of the most recent PPP round. If you try to setup the
same release more than once, you’ll get an error because there is
release already in the `.pipenv` environment.

``` r
(wr <- setup_working_release(creds = creds))
```

If you need to set the environment again, you need to either restart
your R session or use argument `force = TRUE`

## 2. What happens internally?

[`setup_working_release()`](https://pip-technical-team.github.io/pipfun/reference/setup_working_release.md)
deliberately separates responsibilities:

**Resolve release metadata**

[`get_latest_pip_release()`](https://pip-technical-team.github.io/pipfun/reference/get_latest_pip_release.md)
or
[`get_pip_releases()`](https://pip-technical-team.github.io/pipfun/reference/get_pip_releases.md) +
[`find_release()`](https://pip-technical-team.github.io/pipfun/reference/find_release.md)

**Create global metadata**

`pip_create_globals(create_dir = FALSE, ...)`

**Create folders on disk**

[`set_pip_folders()`](https://pip-technical-team.github.io/pipfun/reference/set_pip_folders.md)

**Register aliases**

[`init_pip_aliases()`](https://pip-technical-team.github.io/pipfun/reference/init_pip_aliases.md)

**Persist state**

Objects are stored in .pipenv:

`stamp_root`

`wrk_release`

`gls`

`folder_paths`

`pip_aliases`

This separation makes the system easier to test and reason about.

### 2.1 Directory structure

The function
[`set_pip_folders()`](https://pip-technical-team.github.io/pipfun/reference/set_pip_folders.md)
creates and returns the canonical folder layout for a release.

Release-specific folders use the suffix:

\_

This allows multiple releases to coexist safely.

The returned object contains:

- `aux_data`
- `aux_metadata`
- `dlw_data`
- `dlw_inventory`
- `dlw_metadata`
- `pip_data`
- `pip_metadata`
- `pip_inventory`
- `pip_master_inventory`
- `stamp_root`

You do not call
[`set_pip_folders()`](https://pip-technical-team.github.io/pipfun/reference/set_pip_folders.md)
directly — it is called automatically by
[`setup_working_release()`](https://pip-technical-team.github.io/pipfun/reference/setup_working_release.md).

## 3. Stamp aliases

Each folder is registered as a stamp alias.

Example aliases:

| Folder key    | Alias    |
|---------------|----------|
| aux_data      | aux      |
| pip_metadata  | pip_meta |
| pip_inventory | pip_inv  |

Aliases allow downstream code to reference repositories by short names
instead of full paths.

If you want release-specific aliases:

``` r
setup_working_release(alias_include_release = TRUE)
```

Then aliases like:

`pip_meta_20251211`

are created for release-specific folders.

## 4. Accessing the active release

### 4.1 Retrieve working release

``` r
get_wrk_release()
```

This assigns `wrk_release` in the calling environment:

`wrk_release$release` `wrk_release$identity` `wrk_release$ppp`

If no release has been set, the function aborts with a clear message.

### 4.2 Retrieve folder paths

``` r
get_pip_folders()
```

This assigns `pip_folders` in your environment.

To retrieve a specific folder:

``` r
get_pip_folders("aux_data")
```

### 4.3 Retrieve aliases

``` r
get_pip_aliases()
```

To retrieve a specific alias:

``` r
get_pip_aliases("pip_metadata")
```

## 5. The .pipenv environment

All state is stored in the internal .pipenv environment.

Stored objects include:

- `wrk_release`

- `gls`

- `folder_paths`

- `pip_aliases`

- `stamp_root`

These are accessed indirectly through helper functions such as:

``` r
get_from_pipenv("wrk_release")
get_from_pipenv("gls")
```

Users should generally rely on the public accessors instead of
interacting with .pipenv directly.

## 6. Recommended workflow

A typical session:

#### 1. Set release

[`setup_working_release()`](https://pip-technical-team.github.io/pipfun/reference/setup_working_release.md)

#### 2. Retrieve release info when needed

[`get_wrk_release()`](https://pip-technical-team.github.io/pipfun/reference/get_wrk_release.md)

#### 3. Access folders or aliases

[`get_pip_folders()`](https://pip-technical-team.github.io/pipfun/reference/get_pip_folders.md)
[`get_pip_aliases()`](https://pip-technical-team.github.io/pipfun/reference/get_pip_aliases.md)

All downstream PIP functions assume that a working release has already
been set.

### Summary

The working release system provides:

- Controlled vintage management

- Explicit folder isolation per release

- Automatic directory creation

- Registered stamp aliases

- Centralized state management via .pipenv

This design ensures that PIP workflows remain reproducible, explicit,
and easy to maintain across multiple releases.
