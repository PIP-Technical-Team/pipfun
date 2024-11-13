#' Create new release for PIP update
#'
#' @inheritParams create_new_brach
#' @inheritDotParams get_pip_releases
#' @param ppp numeric: vector of PPP years.
#'
#' @return invisible TRUE if everything went fine
#' @export
#'
#' @examples
#' \dontrun{
#' new_pip_release()
#' }
new_pip_release <-
  function(release     = format(Sys.Date(), "%Y%m%d"),
           identity    = c("PROD", "INT", "TEST"),
           verbose     = getOption("pipfun.verbose"),
           root_dir    = Sys.getenv("PIP_ROOT_DIR"),
           working_dir = fs::path(root_dir,
                                  getOption("pipfun.working_dir")),
           ppps        = getOption("pipfun.ppps"),
           ...) {

  # defenses ----------
  identity <- match.arg(identity)
  call_args <- all_args()
  check_pip_release_inputs(call_args)


  # add new release to pool --------
  ## get current releases ---------
  pr <- get_pip_releases(force = TRUE, ...)
  # pr <- get_pip_releases(force = TRUE)
  mt <- attr(pr, "metadata") # get metadata from GH


  # Create new release ----
  nt <- data.table(release  = release,
                   identity = identity)

  dt <- rbindlist(list(pr, nt),
                  fill = TRUE,
                  use.names = TRUE) |>
    unique()

  # get PPP metadata
  ppp <- get_latest_ppp_versions(ppps = ppps)
  ppp[, n := .I]


  # create folder for welfare data ----

  df <-
    dt[
    # expand to match ppp
    rep(1:.N, nrow(ppp))
    # merge
     ][, n := rowid(release, identity)
       ][ppp, on = "n"]

  df[,
     `:=`(
         ppp_rv = paste0("0", ppp_rv),
         ppp_av = paste0("0", ppp_av)
       )][,
     aux_ver := paste(release, identity, sep = "_")
     ][,
       pc_ver := paste(release, ppp_year,
                         ppp_rv, ppp_av,
                         identity, sep = "_")]


  aux_versions <- df[,unique(aux_ver)]
  pc_versions  <- df[,unique(pc_ver)]


  aux_dir <- create_aux_dir(working_dir  = working_dir,
                            aux_versions = aux_versions)

  pc_dir <- create_pc_dir(working_dir  = working_dir,
                          pc_versions  = pc_versions)


  # Create repo--------
  # NOTE FOR THE FUTURE: I don't remember what I mean by
  # "create repo" I guess is it related to create a branch for the new release
  # in a particular repo, but I'm not sure

  # Update release info ----------

  release_info <-
    save_to_gh(df       = dt,
             owner    = mt$owner,
             repo     = mt$repo,
             branch   = mt$branch,
             metadata = mt)

  # Returning list ---------
  # add here the name of the objects that are wanted to be included in the
  # returning list
  ret_obj_names  <- c("aux_dir", "pc_dir", "release_info")


  lreturn <- vector("list", length = length(ret_obj_names))
  for (i in seq_along(ret_obj_names)) {
    lreturn[[i]] <- get(ret_obj_names[i])
  }

  names(lreturn) <- ret_obj_names
  return(lreturn)

}


#' Create auxiliary directories for new release
#'
#' @param aux_versions character: name of auxiliary folders. they must come in
#'   the form "%Y%m%d_`identify`", where `identify` stands for [c("PROD", "INT",
#'   "TEST")]
#' @inheritParams new_pip_release
#'
#' @rdname create_dir
#' @keywords internal
create_aux_dir <- function(aux_versions,
                           working_dir = fs::path(Sys.getenv("PIP_ROOT_DIR"),
                                                  getOption("pipfun.working_dir"))
                           ) {

  wdir    <- fs::path(working_dir, "aux_data")
  newdirs <- create_dir(wdir, dirs = aux_versions)

  return(newdirs)

}

#' Create poverty calculator directories for new release
#'
#' @param pc_versions character: name of auxiliary folders. they must come in
#'   the form "%Y%m%d_YYYY_MM_AA_`identify`", where `YYYY` stands for the PPP
#'   year, `MM` stands for the master version of the PPPs,  `AA` refers to the
#'   adaptation  version of the pppp`, and where `identify` stands for
#'   [c("PROD", "INT", "TEST")]
#' @inheritParams new_pip_release
#'
#' @rdname create_dir
#' @keywords internal
create_pc_dir <-
  function(pc_versions,
           working_dir = fs::path(Sys.getenv("PIP_ROOT_DIR"),
                                  getOption("pipfun.working_dir"))
                          ) {

  wdir    <- fs::path(working_dir, "pc_data")
  newdirs <- create_dir(wdir, dirs = pc_versions)

  return(newdirs)

}


#' Create directories in folder
#'
#' This function is an informative wrapper around [fs::dir_create]
#'
#' @param wdir chracter: working directory path
#' @param dirs chracter: directories to be created inside or removed from
#'   `wdir`
#'
#' @return logical vector. the names of the elements correspond to the directory
#'   paths
#' @export
create_dir <- function(wdir, dirs,
                       verbose = getOption("pipfun.verbose")) {
  dir_ex <- fs::path(wdir, dirs) |>
    fs::dir_exists()

  if (all(dir_ex == TRUE)) {
    if (verbose)
      cli::cli_alert_info("All versions already exist. No folder will be created")
    return(invisible(dir_ex))
  }

  dirs <- dir_ex[dir_ex == FALSE]
  dirs_ex <- names(dirs) |>
    fs::dir_create(recurse = TRUE) |>
    fs::dir_exists()

  return(dirs_ex)
}



#' Remove a PIP release from folders and Github
#'
#' CAUTION: Use this functions with care.
#' @rdname new_pip_release
#' @export
remove_pip_release <-
  function(release,
           identity    = c("PROD", "INT", "TEST"),
           verbose     = getOption("pipfun.verbose"),
           working_dir = NULL,
           ppps        = getOption("pipfun.ppps"),
           ...) {
  # defenses ----------
  identity <- match.arg(identity)
  call_args <- all_args()
  check_pip_release_inputs(call_args)

  if (is.null(working_dir)) {
    root_dir    = Sys.getenv("PIP_ROOT_DIR")
    official_dir = fs::path(root_dir,
                           getOption("pipfun.working_dir"))

    selection <- menu(choices =
                        c(official_dir,
                          "select your own dir"),
         title = "Select from where you want to delete the data")
    if (selection == 1) {
      working_dir <- official_dir
    } else if (selection == 2) {
      cli::cli_abort("implement browse or something like that... (DEVELPMENT) ")
    }else {
      cli::cli_abort("option not allowed")
    }

  } else {
    end_of_dir <- fs::path_file(working_dir)
    if (end_of_dir != getOption("pipfun.working_dir")) {
      working_dir <- fs::path(working_dir, getOption("pipfun.working_dir"))
    }
  }

  # add new release to pool --------
  ## get current releases ---------
  pr <- get_pip_releases(force = TRUE, ...)
  # pr <- get_pip_releases(force = TRUE)
  mt <- attr(pr, "metadata") # get metadata from GH


  # find release to be removed ----
  filtered_pr  <-
    find_release(pr      = pr,
                 release = release,
                 identity = identity)

  # get PPP metadata
  ppp <- get_latest_ppp_versions(ppps = ppps)
  ppp[, n := .I]

  # update release data
  dt <-  pr[!filtered_pr, on = c("release", "identity")]

  # remove folder for welfare data ----
  ## get versions id -----------
  # This part must be converted into a function because it is repeated in
  # new_pip_releases() as well.
  df <-
    filtered_pr[
      # expand to match ppp
      rep(1:.N, nrow(ppp))
      # merge
    ][, n := rowid(release, identity)
    ][ppp, on = "n"]

  df[,
     `:=`(
       ppp_rv = paste0("0", ppp_rv),
       ppp_av = paste0("0", ppp_av)
     )][,
        aux_ver := paste(release, identity, sep = "_")
     ][,
       pc_ver := paste(release, ppp_year,
                       ppp_rv, ppp_av,
                       identity, sep = "_")]


  aux_versions <- df[,unique(aux_ver)]
  pc_versions  <- df[,unique(pc_ver)]

  ## actual removal of dirs -----------
  aux_dir <- remove_aux_dir(working_dir  = working_dir,
                            aux_versions = aux_versions)

  pc_dir <- remove_pc_dir(working_dir  = working_dir,
                          pc_versions  = pc_versions)


  # Update release info ----------


  release_info <-
    save_to_gh(df       = dt,
               owner    = mt$owner,
               repo     = mt$repo,
               branch   = mt$branch,
               metadata = mt)

  # Returning list ---------
  # add here the name of the objects that are wanted to be included in the
  # returning list
  ret_obj_names  <- c("aux_dir", "pc_dir", "release_info")


  lreturn <- vector("list", length = length(ret_obj_names))
  for (i in seq_along(ret_obj_names)) {
    lreturn[[i]] <- get(ret_obj_names[i])
  }

  names(lreturn) <- ret_obj_names
  return(lreturn)

}

remove_aux_dir <-
  function(aux_versions, working_dir) {

  wdir        <- fs::path(working_dir, "aux_data")
  remove_dir(wdir, dirs = aux_versions)

}

remove_pc_dir <-
  function(pc_versions,
           working_dir = fs::path(Sys.getenv("PIP_ROOT_DIR"),
                                  getOption("pipfun.working_dir"))
  ) {

    wdir        <- fs::path(working_dir, "pc_data")
    remove_dir(wdir, dirs = pc_versions)
}


#' @rdname create_dir
#' @export
remove_dir <- function(wdir, dirs,
                       verbose = getOption("pipfun.verbose")) {
  dir_ex <- fs::path(wdir, dirs) |>
    fs::dir_exists()
  dir_no_ex <- dir_ex == FALSE

  if (any(dir_no_ex)) {
    if (verbose) {
      dir_names <- names(dir_no_ex)
      cli::cli_alert_info("Folder{?s} {.file {dir_names}} {?does/do} not exist")
    }
    return(invisible(dir_names))
  }


  deleted_dir <-
    names(dir_ex) |>
    fs::dir_delete() |>
    {\(.) !fs::dir_exists(.)}()

  return(deleted_dir)
}






new_aux_release <- function(measure     = NULL,
                            release = format(Sys.Date(), "%Y%m%d"),
                            ref_branch  = "DEV",
                            new_branch  = paste(release,
                                                identity[1],
                                                sep = "_"),
                            verbose     = getOption("pipfun.verbose")
                            ) {

}


#' Get PIP releases
#'
#' All the releases available in PIP in any of the servers.
#'
#' @inheritParams get_file_info_from_gh
#' @param force logical: whether to load releases from Github even if they
#'   already available in env .pipenv
#' @param verbose logical: whether to display additional information
#'
#' @return data.table with releases table
#' @export
#'
#' @examples
#' get_pip_releases()
get_pip_releases <- function(owner     = getOption("pipfun.ghowner"),
                             repo      = "pip_info",
                             file_path = "releases.csv",
                             branch    = "releases",
                             verbose   = getOption("pipfun.verbose"),
                             force     = FALSE) {


  # Check if releases available in .pipenv
  if (force == FALSE) {
    if (rlang::env_has(.pipenv, "releases")) {
      if (verbose) {
        cli::cli_alert("{.field releases} is already available in env
                        {.code .pipenv}. Use option {.code force} to
                        load them again from gh",
                        wrap = TRUE)
      }
      return(rlang::env_get(.pipenv, "releases"))
    }
  }

  pr <- get_file_from_gh(owner = owner,
                         repo = repo,
                         branch =  branch,
                         file_path = file_path)

  rlang::env_poke(.pipenv, "releases", pr)
  pr

}



#' Get latest PIP release
#'
#' latest PIP release per identity
#'
#' @param identity character: one of "PROD", "INT", or "TEST"
#' @inheritDotParams get_pip_releases
#'
#' @return data.table with most recent release
#' @export
#'
#' @examples
#' get_latest_pip_release()
get_latest_pip_release <- function(identity = c("PROD", "INT", "TEST"),
                                   ...) {

  iden <- match.arg(identity)
  df   <- get_pip_releases(...)

  # Filter by identity and get max
  df <- df[identity == iden
           ][,
             .SD[which.max(release)]]
  # return
  df
}


#' check arguments of release functions
#'
#' @param call_args arguments from release function in form of list. they should
#'   be created using [all_args]
#'
#' @return invisible TRUE if everything goes well
#' @keywords internal
check_pip_release_inputs <- function(call_args) {
  list2env(call_args, envir = environment())

  if (exists("release", envir = environment(), inherits = FALSE)){
    if (!grepl("[0-9]{8}", release))
      cli::cli_abort("{.arg release} must be a numeric chracter,
                     representing a date in the form {.field \"%Y%m%d\"}.
                     You provided {.strong {release}}")
  }

  if (exists("working_dir")){
    if (!fs::dir_exists(working_dir))
      cli::cli_alert_danger("Directory {.file {working_dir}} does not exist. Please check")
  }



}


# release = format(Sys.Date(), "%Y%m%d"),
# identity    = c("PROD", "INT", "TEST"),
# verbose     = getOption("pipfun.verbose"),
# root_dir    = Sys.getenv("PIP_ROOT_DIR"),
# working_dir = fs::path(root_dir,
#                        getOption("pipfun.working_dir"))




# find release  ----
#' Find release in releases table
#'
#' @inheritParams get_latest_pip_release
#' @inheritParams new_pip_release
#' @param pr PIP Releases table from [get_pip_releases]
#'
#' @return invisible data frame with filtered release
#'
#' @keywords internal
find_release <- function(pr = NULL, release, identity) {
  release2del  <- release
  identity2del <- identity

  if (is.null(pr))
    pr <- get_pip_releases()

  filtered_pr  <- pr[release == release2del & identity == identity2del]
  nr <- nrow(filtered_pr)

  if (nr == 0) {
    cli::cli_abort("Release {.field {release2del}_{identity2del}} does not exist")
  } else if (nr > 1) {
    cli::cli_abort("Release {.field {release2del}_{identity2del}} does
                     uniquely identify the data.
                     Check with
                     {.run pipfun::get_pip_releases(force = TRUE)}")
  }
  invisible(filtered_pr)
}

