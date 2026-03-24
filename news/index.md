# Changelog

## pipfun 0.3.11

- Fix tests of branch management.

## pipfun 0.3.10

- Add `pip_master_inventory` to pip boards.

## pipfun 0.3.9

- Add `dlw_metadata` to pip boards.

## pipfun 0.3.8

- new board structure of aux data and metadata. now it has to boards:
  aux_data and aux_metadata.

## pipfun 0.3.7

- deprecate `path` in
  [`log_save()`](https://pip-technical-team.github.io/pipfun/reference/log_save.md).
  Now we use pins boards.
- Fix issue with logging system and its helpers.

## pipfun 0.3.6

- Add tests and add functionality to retrieve specif boards in
  `get_pins_boards()`

## pipfun 0.3.5

- Fix big bug

## pipfun 0.3.4

- Include pins boards in working release. Now, they can be retrieve with
  `get_pins_boards()`

## pipfun 0.3.3

- Fix all tests

## pipfun 0.3.2

- Fix issue with logmeta and arg in logging system.

## pipfun 0.3.1

- add layers for security before deleting releases folders

## pipfun 0.3.0

## pipfun 0.2.2

- Add full battery of functions for the logging system. Refer to the
  vignette to learn more about them.

## pipfun 0.2.1

- add get_wrk_release()

- add GH_PASS to Renviron non-interactive session

## pipfun 0.2.0

From 0.2.0 onward the development is the new PIP pipeline and should be
merged into DEV_v2

## pipfun 0.1.0

- add new suit of function to interact with GitHub.

## pipfun 0.0.2

- Add
  [`check_pkg_active()`](https://pip-technical-team.github.io/pipfun/reference/check_pkg_active.md)
  to check if package is active

- Change max country and years in
  [`pip_create_globals()`](https://pip-technical-team.github.io/pipfun/reference/pip_create_globals.md)

- Add
  [`save_to_gh()`](https://pip-technical-team.github.io/pipfun/reference/save_to_gh.md)
  function

- add `convert_df_to_base64()` from `{pipaux}`

## pipfun 0.0.1

- Add `save_to_gh` function
- add checks for active package
  [`check_pkg_active()`](https://pip-technical-team.github.io/pipfun/reference/check_pkg_active.md)
