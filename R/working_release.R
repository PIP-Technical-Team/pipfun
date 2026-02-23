#' Set up a PIP working release in the local environment
#'
#' This function resolves a release (either the latest or a specified
#' one), initializes package globals for that release, creates the
#' required filesystem layout, registers stamp aliases for each
#' repository folder, and persists the resulting state into the
#' in-memory `.pipenv` environment used by the package.
#'
#' The function intentionally separates concerns:
#' - release metadata resolution is handled via GitHub helper functions;
#' - global variables are created without creating directories in
#'   `pip_create_globals()` (so folder creation is explicit and
#'   controlled here);
#' - `stamp` aliases are registered for each repository folder so
#'   downstream code can refer to those repositories by short alias.
#'
#' @param release character|null: specific release identifier (e.g.
#'   a date string) to use. If `NULL`, the latest release is resolved
#'   via `get_latest_pip_release()`.
#' @param identity character: identity token for the release. The
#'   default is read from `getOption("pipfun.identities")`. One of the
#'   allowed identity values must be supplied.
#' @param force logical: currently reserved for future use (kept for
#'   backward compatibility).
#' @param owner character: GitHub owner that contains the pip_info repo.
#' @param repo character: GitHub repository name that contains release
#'   metadata. Defaults to `"pip_info"`.
#' @param file_path character: path inside the repo to the releases file.
#' @param branch character: branch name to read the releases file from.
#' @param verbose logical: print progress messages when `TRUE`.
#' @param ppp numeric: PPP year to use; defaults to the option
#'   `pipfun.ppps` and is validated against it.
#' @param creds optional GitHub credentials object passed to GitHub helpers.
#' @param main_dir character: top-level directory where all PIP data
#'   repositories live. Defaults to `getOption("pipfun.main_dir")`.
#' @param alias_include_release logical: when `TRUE`, release-specific
#'   aliases will include the release string (useful for coexisting
#'   releases in a single stamp root).
#' @param ... Additional arguments passed to `pip_create_globals()`.
#'
#' @return Invisibly returns a list with elements `release`, `identity`
#'   and `ppp` describing the working release that was set. The function
#'   also has side effects: it writes objects to the package `.pipenv`
#'   environment (`stamp_root`, `wrk_release`, `gls`, `folder_paths`,
#'   and `pip_aliases`).
#'
#' @examples
#'
#' \dontrun{
#' setup_working_release()
#' }
#'
#' @export
setup_working_release <- function(release  = NULL,
                                  identity  = getOption("pipfun.identities"),
                                  force     = FALSE,
                                  owner     = getOption("pipfun.ghowner"),
                                  repo      = "pip_info",
                                  file_path = "releases.csv",
                                  branch    = "releases",
                                  verbose   = getOption("pipfun.verbose"),
                                  ppp       = getOption("pipfun.ppps"),
                                  creds     = NULL,
                                  main_dir  = getOption("pipfun.main_dir"),
                                  alias_include_release = FALSE,
                                  ...) {

  # Validate and normalise inputs
  identity <- match.arg(identity)
  ppp <- ppp[1]

  if (!ppp %in% getOption("pipfun.ppps")) {
    cli::cli_abort(c(
      x = "Wrong PPP value",
      i = "PPP values must be one of {.or {getOption(\"pipfun.ppps\")}}"
    ))
  }

  # ------------------------------------------------------------------
  # Resolve release metadata (either latest or specified release)
  # - We keep release resolution separate to make testing easier.
  # ------------------------------------------------------------------
  pr <- if (is.null(release)) {
    get_latest_pip_release(
      identity  = identity,
      owner     = owner,
      repo      = repo,
      file_path = file_path,
      branch    = branch,
      verbose   = verbose,
      creds     = creds
    )
  } else {
    get_pip_releases(
      owner     = owner,
      repo      = repo,
      file_path = file_path,
      branch    = branch,
      verbose   = verbose,
      creds     = creds
    ) |> 
      find_release(release = release, identity = identity)
  }

  # ------------------------------------------------------------------
  # Create globals (do not create directories here)
  # - Passing create_dir = FALSE ensures folder creation is explicit
  #   and remains under the control of this function.
  # ------------------------------------------------------------------
  gls <- pip_create_globals(
    create_dir = FALSE,
    vintage = list(
      release  = release,
      ppp_year = ppp,
      identity = identity
    ),
    verbose = verbose,
    ...
  )

  # Working release descriptor used by callers and persisted into .pipenv
  wr <- list(
    release  = pr[, release],
    identity = pr[, identity],
    ppp      = ppp
  )

  # ------------------------------------------------------------------
  # Create folders on disk for this release
  # - set_pip_folders() will create any missing directories and
  #   initialize stamp at the project root if necessary.
  # ------------------------------------------------------------------
  folder_paths <- set_pip_folders(
    main_dir = main_dir,
    release  = pr[, release],
    identity = pr[, identity]
  )

  # ------------------------------------------------------------------
  # Register stamp aliases for each repository folder so that code can
  # reference data by alias (e.g. "aux", "pip_meta").
  # ------------------------------------------------------------------
  aliases <- init_pip_aliases(
    folder_paths,
    include_release = alias_include_release,
    release = pr[, release]
  )

  # ------------------------------------------------------------------
  # Persist state in the package .pipenv environment for quick access
  # by other pipfun functions.
  # ------------------------------------------------------------------
  rlang::env_poke(.pipenv, "stamp_root", main_dir)
  rlang::env_poke(.pipenv, "wrk_release", wr)
  rlang::env_poke(.pipenv, "gls", gls)
  rlang::env_poke(.pipenv, "folder_paths", folder_paths)
  rlang::env_poke(.pipenv, "pip_aliases", aliases)

  if (verbose) {
    cli::cli_alert_info(
      "PIP working release set to {.field {wr$release}-{wr$identity}}"
    )
    print(folder_paths)
    cli::cli_alert_info("Registered PIP aliases:")
    print(aliases)
  }

  invisible(wr)
}






#' Create and return directory paths for a PIP release
#'
#' This function ensures the on-disk layout required by pipfun exists and
#' returns a named list with the canonical paths. The function will create
#' missing directories and initialize a `stamp` root at `main_dir` if one
#' has not already been initialized. The `rt` suffix (release_identity)
#' is appended to release-specific folders so multiple releases can coexist.
#'
#' @param main_dir character: top-level directory where PIP repositories are stored.
#' @param release character: release identifier (e.g. a date string). If
#'   `NULL`, the latest release is resolved via `get_latest_pip_release()`.
#' @param identity character: identity token used as suffix. Default comes
#'   from `getOption("pipfun.identities")` and is validated with `match.arg()`.
#'
#' @return A named list of paths (class `pip_folder_paths`) containing
#'   `stamp_root`, `aux_data`, `aux_metadata`, `dlw_data`, `dlw_inventory`,
#'   `dlw_metadata`, `pip_data`, `pip_metadata`, `pip_inventory`, and
#'   `pip_master_inventory`.
#'
#' @examples
#' 
#' \dontrun{
#' set_pip_folders(main_dir = "~/pip_data", release = "20251211")
#' }
#'
#' @export
set_pip_folders <- function(main_dir  = getOption("pipfun.main_dir"),
                            release  = NULL,
                            identity  = getOption("pipfun.identities")) {

  # Validate inputs
  identity <- match.arg(identity)
  if (is.null(release)) {
    release <- get_latest_pip_release() |> _[, release]
  }

  # Release-specific trailing component used to isolate per-release dirs
  rt <- glue("{release}_{identity}")

  # ------------------------------------------------------------------
  # Create auxiliary repository directories (two internal folders)
  # - returns a two-element vector for aux_internal paths
  # ------------------------------------------------------------------
  aux_internal <- c("aux_data", "aux_metadata")
  aux_dir <- fs::path(main_dir, "aux_repository", aux_internal, rt) |>
    fs::dir_create()

  # ------------------------------------------------------------------
  # DLW (data-lake/work) repository layout
  # - create top-level DLW directory and subfolders used by pipeline
  # ------------------------------------------------------------------
  dlw_dir           <- fs::path(main_dir, "dlw_repository") |>
    fs::dir_create(recurse = TRUE)
  dlw_data_dir      <- fs::path(dlw_dir, "dlw_data") |>
    fs::dir_create(recurse = TRUE)
  dlw_inventory_dir <- fs::path(dlw_dir, "dlw_inventory") |>
    fs::dir_create(recurse = TRUE)
  dlw_metadata_dir  <- fs::path(dlw_dir, "dlw_metadata", rt) |>
    fs::dir_create(recurse = TRUE)

  # ------------------------------------------------------------------
  # PIP repository layout (surveys, metadata, inventories)
  # ------------------------------------------------------------------
  pip_dir                  <- fs::path(main_dir, "pip_repository") |>
    fs::dir_create(recurse = TRUE)
  pip_data_dir             <- fs::path(pip_dir, "pip_data", "surveys") |>
    fs::dir_create(recurse = TRUE)
  pip_master_inventory_dir <- fs::path(pip_dir, "pip_data", "master_inventory") |>
    fs::dir_create(recurse = TRUE)
  pip_metadata_dir         <- fs::path(pip_dir, "pip_data", "surveys_metadata", rt) |>
    fs::dir_create(recurse = TRUE)
  pip_inventory_dir        <- fs::path(pip_dir, "pip_inventory", rt) |>
    fs::dir_create(recurse = TRUE)

  # The stamp project root is the top-level main_dir; stamp is used to
  # register aliases and work with multiple repositories in one root.
  stamp_root <- main_dir

  # Initialize stamp if needed. This creates `.stamp` under `stamp_root`.
  if (!fs::dir_exists(fs::path(stamp_root, ".stamp"))) {
    stamp::st_init(root = stamp_root)
  }

  # Return named list of canonical repository paths
  folder_paths <- list(
    stamp_root    = stamp_root,
    aux_data      = aux_dir[1],
    aux_metadata  = aux_dir[2],
    dlw_data      = dlw_data_dir,
    dlw_inventory = dlw_inventory_dir,
    dlw_metadata  = dlw_metadata_dir,
    pip_data      = pip_data_dir,
    pip_metadata  = pip_metadata_dir,
    pip_inventory = pip_inventory_dir,
    pip_master_inventory = pip_master_inventory_dir
  )

  class(folder_paths) <- "pip_folder_paths"
  folder_paths
}


#' Retrieve and assign the active working release
#'
#' Utility for functions that need access to the currently configured
#' working release. The function fetches the `wrk_release` object from
#' the package `.pipenv` environment and assigns it into the caller's
#' frame under `name` (default: `wrk_release`). If no working release
#' is set, the function aborts with an informative message.
#'
#' @param name character: name to assign the working release object to in
#'   the calling environment. Defaults to `"wrk_release"`.
#' @param verbose logical: whether to print a short confirmation message
#'   showing the active release.
#'
#' @return Invisibly returns `NULL`. The primary effect is that a variable
#'   called `name` is assigned in the calling frame containing the working
#'   release list (with `release`, `identity`, and `ppp`).
#'
#' @examples
#' 
#' \dontrun{
#' hello <- function() {
#'   get_wrk_release()
#'   invisible(wrk_release)
#' }
#' setup_working_release()
#' print(hello())
#' }
#'
#' @export
get_wrk_release <- function(name = "wrk_release",
                            verbose  = getOption("pipfun.verbose")) {
  # Fetch the working release from the package environment
  wrk_release <- get_from_pipenv("wrk_release")
  if (is.null(wrk_release)) {
    cli::cli_abort(
      c(x = "Working release has not been set up",
        i = "You need to set a working release with {.code pipfun::setup_working_release()}"))
  } else {
    if (verbose) cli::cli_alert_info("Your working release is {.field {wrk_release$release}}")
  }

  # Assign into the caller's frame for immediate use by the calling function
  assign(name, wrk_release, envir = parent.frame())
}




#' Retrieve folder paths registered for the active PIP working release
#'
#' Fetches the `folder_paths` object stored in `.pipenv` and assigns it to
#' the caller's environment (useful for interactive development). Optionally
#' returns a single folder path when `folder` is supplied.
#'
#' @param folder character|null: optional specific folder name to return
#'   (e.g. `"aux_data"`). If `NULL` (default) the full named list is returned invisibly.
#' @param name character: variable name to assign the full folder list to in
#'   the calling environment. Defaults to `"pip_folders"`.
#' @param verbose logical: if `TRUE`, prints information about the retrieved paths.
#'
#' @return Invisibly returns the named list of folder paths. If `folder` is
#'   provided, returns the single path (invisibly) for that folder.
#'
#' @export
get_pip_folders <- function(name = NULL, verbose = TRUE) {

  if (!exists(".pipenv", envir = globalenv()) ||
      !exists("folder_paths", envir = .pipenv, inherits = FALSE)) {
    cli::cli_abort("PIP folder paths have not been set up")
  }

  folders <- get("folder_paths", envir = .pipenv)

  if (!is.null(name) && length(name) == 1 && name %in% names(folders)) {
    return(folders[[name]])
  }

  if (!is.null(name)) {
    assign(name, folders, envir = parent.frame())
  }

  invisible(folders)
}




#' Retrieve stamp aliases registered for PIP folders
#'
#' Returns the alias mapping that was created by `init_pip_aliases()` and
#' persisted into `.pipenv`. Aliases map short names (e.g. `"aux"`) to
#' actual folder paths registered with `stamp`.
#'
#' @param folder character|null: optional folder name (key of aliases) to return.
#'   If `NULL`, returns the full named character vector of aliases.
#' @param name character: variable name to assign the aliases to in the
#'   calling environment. Defaults to `"pip_aliases"`.
#' @param verbose logical: if `TRUE`, prints a summary of aliases retrieved.
#'
#' @return Invisibly returns a named character vector of aliases. If `folder`
#'   is provided, returns the single alias string (invisibly).
#'
#' @examples
#' 
#' \\dontrun{
#' setup_working_release()
#' get_pip_aliases()             # returns all aliases
#' get_pip_aliases("aux_data")  # returns alias for aux_data
#' }
#'
#' @export
get_pip_aliases <- function(name = NULL, verbose = TRUE) {

  if (!exists(".pipenv", envir = globalenv()) ||
      !exists("pip_aliases", envir = .pipenv, inherits = FALSE)) {
    cli::cli_abort("PIP aliases have not been set up")
  }

  aliases <- get("pip_aliases", envir = .pipenv)

  if (!is.null(name)) {
    return(aliases[[name]])
  }

  invisible(aliases)
}

#' Initialize and register stamp aliases for PIP folders
#'
#' Sets up short, human-friendly aliases for each of the repository
#' folders returned by `set_pip_folders()` using the `stamp` package.
#' When `include_release = TRUE`, release-specific aliases will include
#' the release string (so aliases like `pip_meta_20251211` are created).
#'
#' @param folder_paths named list: output of `set_pip_folders()`.
#' @param verbose logical: whether to print alias registration messages.
#' @param include_release logical: when `TRUE`, append `_<release>` to
#'   aliases for folders that are release-specific.
#' @param release character|null: release identifier used when
#'   `include_release = TRUE`. Required in that case.
#'
#' @return Invisibly returns a named character vector of the final alias names.
#'   The vector's names correspond to the keys of `folder_paths`.
#'
#' @keywords internal
init_pip_aliases <- function(folder_paths,
                             verbose = getOption("pipfun.verbose"),
                             include_release = FALSE,
                             release = NULL) {

  # Map from folder key -> short base alias
  alias_map <- c(
    aux_data      = "aux",
    aux_metadata  = "aux_meta",
    dlw_data      = "dlw",
    dlw_inventory = "dlw_inv",
    dlw_metadata  = "dlw_meta",
    pip_data      = "pip",
    pip_metadata  = "pip_meta",
    pip_inventory = "pip_inv",
    pip_master_inventory = "pip_master"
  )

  # Folders whose aliases should be release-specific when requested
  release_specific <- c(
    "aux_data",
    "aux_metadata",
    "dlw_metadata",
    "pip_metadata",
    "pip_inventory"
  )

  if (include_release && (is.null(release))) {
    cli::cli_abort("release must be provided when include_release = TRUE")
  }

  # Build the final alias names (append release suffix for specific folders)
  final_aliases <- vapply(names(alias_map), function(nm) {
    base <- alias_map[[nm]]
    if (include_release && (nm %in% release_specific)) {
      paste0(base, "_", release)
    } else {
      base
    }
  }, FUN.VALUE = character(1))

  # Register aliases with stamp for each folder; stamp will report
  # conflicts or errors as appropriate.
  for (nm in names(final_aliases)) {

    alias <- final_aliases[[nm]]
    root  <- fs::path_norm(folder_paths[[nm]])

    stamp::st_init(
      root  = root,
      alias = alias
    )

    if (verbose) {
      cli::cli_alert_success(
        "Registered alias {.field {alias}} -> {.path {root}}"
      )
    }
  }

  invisible(final_aliases)
}


