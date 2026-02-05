#' Loads PIP release into .pipenv
#'
#' Sets the working PIP release and initializes stamp aliases
#' for all PIP folders associated with that release.
#'
#' @inheritParams find_release
#' @inheritParams get_pip_releases
#' @inheritParams download_and_read_file
#' @inheritDotParams pip_create_globals -vintage -create_dir
#' @param ppp numeric: PPP year to use.
#' @param main_dir character: directory path where all PIP data is stored.
#'
#' @return Invisible list with working release information
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

  identity <- match.arg(identity)
  ppp <- ppp[1]

  if (!ppp %in% getOption("pipfun.ppps")) {
    cli::cli_abort(c(
      x = "Wrong PPP value",
      i = "PPP values must be one of {.or {getOption(\"pipfun.ppps\")}}"
    ))
  }

  # ------------------------------------------------------------------
  # Resolve release metadata
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
  # Create globals (no directory creation here)
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

  # ------------------------------------------------------------------
  # Working release descriptor
  # ------------------------------------------------------------------
  wr <- list(
    release  = pr[, release],
    identity = pr[, identity],
    ppp      = ppp
  )

  # ------------------------------------------------------------------
  # Create folders (pure filesystem step)
  # ------------------------------------------------------------------
  folder_paths <- set_pip_folders(
    main_dir = main_dir,
    release  = pr[, release],
    identity = pr[, identity]
  )

  # ------------------------------------------------------------------
  # Initialize stamp aliases (one alias per folder)
  # ------------------------------------------------------------------
  aliases <- init_pip_aliases(
    folder_paths,
    include_release = alias_include_release,
    release = pr[, release]
  )

  # ------------------------------------------------------------------
  # Persist state in .pipenv
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






#' Set PIP directory paths
#'
#' This function creates all necessary directories for a PIP release
#' and returns a named list of paths.
#'
#' @inheritParams setup_working_release
#' @returns Named list of directory paths
#' @export
set_pip_folders <- function(main_dir  = getOption("pipfun.main_dir"),
                            release  = NULL,
                            identity  = getOption("pipfun.identities")) {

  identity <- match.arg(identity)
  if (is.null(release)) {
    release <- get_latest_pip_release() |> _[, release]
  }

  rt <- glue("{release}_{identity}")

  # Aux data
  aux_internal <- c("aux_data", "aux_metadata")
  aux_dir <- fs::path(main_dir, "aux_repository", aux_internal, rt) |>
    fs::dir_create()

  # DLW data
  dlw_dir           <- fs::path(main_dir, "dlw_repository") |>
    fs::dir_create(recurse = TRUE)
  dlw_data_dir      <- fs::path(dlw_dir, "dlw_data") |>
    fs::dir_create(recurse = TRUE)
  dlw_inventory_dir <- fs::path(dlw_dir, "dlw_inventory") |>
    fs::dir_create(recurse = TRUE)
  dlw_metadata_dir  <- fs::path(dlw_dir, "dlw_metadata", rt) |>
    fs::dir_create(recurse = TRUE)

  # PIP data
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

  # Determine stamp project root (the overall PIP data directory)
  stamp_root <- main_dir

  # Initialize stamp if needed
  if (!fs::dir_exists(fs::path(stamp_root, ".stamp"))) {
    stamp::st_init(root = stamp_root)
  }

  # Return named list of paths
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


#' get working release in PIP functions
#'
#' You can place this function at the beginning of any of your PIP function to
#' work with the working release
#'
#' @param name character: Name of the working release object. default is
#'   "wrk_release" and you should leave it like that
#'
#' @return assign `name` object to `parent.frame()` which is the function it is
#'   being called from
#' @export
#'
#' @examples
#' \dontrun{
#' hello <- function() {
#' get_wrk_release()
#' invisible(wrk_release)
#' }
#' setup_working_release()
#' print(hello())
#' }
get_wrk_release <- function(name = "wrk_release",
                            verbose  = getOption("pipfun.verbose")) {
  wrk_release <- get_from_pipenv("wrk_release")
  if (is.null(wrk_release)) {
    cli::cli_abort(
      c(x = "Working release has not been set up",
        i = "You need to set a working release with {.code pipfun::setup_working_release()}"))
  } else {
    if (verbose) cli::cli_alert_info("Your working release is {.field {wrk_release$release}}")
  }

  assign(name, wrk_release, envir = parent.frame())
}




#' Get PIP folder paths from .pipenv
#'
#' This function retrieves the folder paths that were set up for a PIP release.
#'
#' @param folder character: optional, name of a specific folder to retrieve.
#'   If NULL (default), returns all folder paths.
#' @param name character: name of the object to assign to the calling environment.
#'   Default is `"pip_folders"`.
#' @param verbose logical: whether to print info about the folders retrieved. Default is FALSE
#'
#' @return A named list of folder paths or a single folder path if `folder` is specified.
#' @export
get_pip_folders <- function(folder = NULL,
                            name = "pip_folders",
                            verbose = FALSE) {

  pip_folders <- get_from_pipenv("folder_paths")

  if (is.null(pip_folders)) {
    cli::cli_abort(
      c(x = "PIP folder paths have not been set up",
        i = "You need to set a working release with {.code pipfun::setup_working_release()}"))
  }

  if (verbose) {
    cli::cli_alert_info("Retrieved PIP folder paths")
    print(pip_folders)
  }

  # Assign to parent.frame for developer convenience
  assign(name,
         pip_folders,
         envir = parent.frame())

  if (is.null(folder)) return(invisible(pip_folders))

  if (!(folder %in% names(pip_folders))) {
    cli::cli_abort("{.field {folder}} is not available in {.field pip_folders}")
  }

  invisible(pip_folders[[folder]])
}




#' Get PIP aliases from .pipenv
#'
#' Retrieve the alias mapping that was registered during setup_working_release().
#'
#' @param folder character: optional, name of a specific folder alias to retrieve.
#'   If NULL (default), returns all aliases.
#' @param name character: name of the object to assign to the calling environment.
#'   Default is `"pip_aliases"`.
#' @param verbose logical: whether to print info about the aliases retrieved.
#'   Default is FALSE.
#'
#' @return Named character vector (invisible) of aliases or a single alias string
#'   if `folder` is specified.
#' @export
#'
#' @examples
#' \dontrun{
#' setup_working_release()
#' get_pip_aliases()             # returns all aliases
#' get_pip_aliases("aux_data")   # returns alias for aux_data
#' }
get_pip_aliases <- function(folder = NULL,
                            name = "pip_aliases",
                            verbose = FALSE) {

  pip_aliases <- get_from_pipenv("pip_aliases")

  if (is.null(pip_aliases)) {
    cli::cli_abort(
      c(x = "PIP aliases have not been set up",
        i = "Run {.code pipfun::setup_working_release()} to register aliases")
    )
  }

  if (verbose) {
    cli::cli_alert_info("Retrieved PIP aliases")
    print(pip_aliases)
  }

  if (is.null(folder)) return(invisible(pip_aliases))

  if (!(folder %in% names(pip_aliases))) {
    cli::cli_abort("{.field {folder}} is not available in {.field pip_aliases}")
  }

  invisible(pip_aliases[[folder]])
}


#' Initialize stamp aliases for PIP folders
#'
#' @param folder_paths Named list from set_pip_folders()
#' @param include_release logical: append release to release-specific aliases
#' @param release character: release string (e.g. \"20251211\"). Required when include_release = TRUE
#' @return Invisible named character vector of aliases
#' @export
init_pip_aliases <- function(folder_paths,
                             verbose = getOption("pipfun.verbose"),
                             include_release = FALSE,
                             release = NULL) {

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

  # Which folders are release-specific (those that include rt in set_pip_folders)
  release_specific <- c(
    "aux_data",
    "aux_metadata",
    "dlw_metadata",
    "pip_metadata",
    "pip_inventory"
  )

  if (include_release && (is.null(release) || !nzchar(release))) {
    cli::cli_abort("release must be provided")
  }

  # Build final alias names
  final_aliases <- vapply(names(alias_map), function(nm) {
    base <- alias_map[[nm]]
    if (include_release && (nm %in% release_specific)) {
      paste0(base, "_", release)
    } else {
      base
    }
  }, FUN.VALUE = character(1))

  # Register aliases with stamp; let stamp handle conflicts/errors
  for (nm in names(final_aliases)) {

    alias <- final_aliases[[nm]]
    root  <- fs::path_norm(folder_paths[[nm]])

    stamp::st_init(
      root  = root,
      alias = alias
    )

    if (verbose) {
      cli::cli_alert_success(
        "Registered alias {.field {alias}} → {.path {root}}"
      )
    }
  }

  invisible(final_aliases)
}


