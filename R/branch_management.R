#' Create new branch in repo
#'
#' By default it will create a new branch in aux PIP repo, but it can create a
#' new branch in repo
#'
#' @inheritParams load_from_gh
#' @param release character: date in the form "%Y%m%d"
#' @param identity character: for PIP repos it must be one of `c("PROD", "INT",
#'   "TEST")`. For other repos, just leave de default, which is 'PROD'.
#' @param ref_branch Character: reference branch from which the new branch will be
#'   created.
#' @param new_branch character: name of new branch. Default is
#'   [paste0(release, "_", identity[1])]
#'
#' @return TRUE if [new_branch] already exists or if it was created
#' @export
#'
#' @examples
#' \dontrun{
#' # success
#'   create_new_brach("regions",
#'   release = "20240903")
#'
#'   create_new_brach("regions",
#'   new_branch = "test")
#'
#' # Fail
#'   create_new_brach("fjfjf",
#'   new_branch = "test") |>
#'   try()
#' }
create_new_brach <- function(measure     = NULL,
                             owner       = getOption("pipfun.ghowner"),
                             repo        = ifelse(is.null(measure), NA,
                                                  paste0("aux_", measure)) ,
                             release     = format(Sys.Date(), "%Y%m%d"),
                             identity    = c("PROD", "INT", "TEST"),
                             ref_branch  = "DEV",
                             new_branch  = paste0(release, "_", identity[1]),
                             verbose     = getOption("pipfun.verbose")) {

  identity <- match.arg(identity)

  # defenses ----------
  stopifnot(exprs = {
    grepl("-?\\d{8}", release)
    is.character(repo)
    is.character(owner)
    is.character(ref_branch)
    is.character(new_branch)
    length(ref_branch) == 1
    length(new_branch) == length(ref_branch)
  })

  # Get branches available -------
  branch_available <-
    get_gh(owner = owner,
           repo = repo,
           what = "branches")

  # confirm new branch is not available ------

  if (new_branch %in% branch_available) {
    cli::cli_alert("branch {.field {new_branch}} already exists in repo {.field {owner}/{repo}}")
    return(invisible(TRUE))
  }


  if (!(ref_branch %in% branch_available)) {
    cli::cli_abort("reference branch {.field {ref_branch}} does not exists in repo {.field {owner}/{repo}}")
  }



  # create branch -----------
  sha <- gh::gh("GET /repos/:owner/:repo/git/ref/heads/:ref",
                owner = owner,
                repo = repo,
                ref = ref_branch)$object$sha

  success_creation <-
    tryCatch(
      expr = {
        gh::gh("POST /repos/:owner/:repo/git/refs",
               owner = owner,
               repo = repo,
               ref = sprintf("refs/heads/%s", new_branch),
               sha = sha)
        if (verbose) cli::cli_alert_success("branch {.field {new_branch}} successfully created")
        TRUE
      }, # end of expr section

      error = function(e) {
        print(e$message)
        FALSE
      }, # end of error section

      warning = function(w) {
        print(w$message)
        delete_branch(branch_to_delete = new_branch,
                      owner   = owner,
                      repo    = repo,
                      ask     = FALSE,
                      verbose = FALSE)
        FALSE
      }
    ) # End of trycatch

  if (success_creation) {
    return(invisible(TRUE))
  } else {
    cli::cli_abort("Failed creating branch {new_branch}. Check connection and try again")
  }


  # TIME <- format(Sys.time(), "%Y%m%d%H%M%S")
  # DATE <- format(Sys.Date(), "%Y%m%d")

}


#' delete branch in Github repo
#'
#' @param branch_to_delete character: branch to delete
#' @inheritParams create_new_brach
#' @param ask logical: whether to ask the user to confirm. Default is [interactive()]
#'
#' @return logical, whether or not branch was deleted
#' @export
#'
#' @examples
#' \dontrun{
#' create_new_brach(
#'   measure = "regions",
#'   release = "20240903")
#'
#' delete_branch(branch_to_delete = "20240903_PROD",
#'               measure = "regions",
#'               ask = FALSE)
#'
#' create_new_brach("regions",
#' new_branch = "test")
#'
#' delete_branch(branch_to_delete = "test",
#'               measure = "regions",
#'               ask = FALSE)
#' }
delete_branch <- function(branch_to_delete,
                          measure     = NULL,
                          owner       = getOption("pipfun.ghowner"),
                          repo        = ifelse(is.null(measure), NA,
                                               paste0("aux_", measure)),
                          ask         = interactive(),
                          verbose     = getOption("pipfun.verbose")) {

  # defenses ----------
  stopifnot(exprs = {
    is.character(repo)
    is.character(owner)
    is.character(branch_to_delete)
    length(branch_to_delete) == 1
  })
  # Confirm branch exist -------
  branch_exists <-
    confirm_branch_exists(branch  = branch_to_delete,
                          measure = measure,
                          owner   = owner,
                          repo    = repo)

  if (!branch_exists) {
    cli::cli_abort("branch {.field {branch_to_delete}} does not exists. Nothing to delete")
  }

  # confirm new branch is not available ------
  if (ask) {
    cli::cli_alert_danger("Are you sure you want to delete branch
    {.field {branch_to_delete}} from repo
    {.field {owner}/{repo}}?\n
    Enter an item from the menu, or 0 to exit",
                          wrap = TRUE)
    answer <- switch(menu(c("No.", "Yes.")) + 1,
                     FALSE, FALSE, TRUE)
  } else {
    answer <- TRUE
  }

  if (answer) {
    deleted <-
      tryCatch({
        deleted <-
          gh::gh("DELETE /repos/:owner/:repo/git/refs/:ref",
                 owner = owner,
                 repo = repo,
                 ref = sprintf("heads/%s", branch_to_delete))
        if (verbose)
          cli::cli_inform("Branch {.field {branch_to_delete}} successfully deleted.")
        TRUE
      }, error = function(e) {
        if (verbose)
          cli::cli_alert_danger("Failed to delete branch:\n {e$message}")
        FALSE
      })

  } else {
    if (verbose)
      cli::cli_alert("Branch {.field {branch_to_delete}} NOT deleted")
    return(invisible(FALSE))
  }
  return(invisible(deleted))
}



#' confirm branch exists in repo
#'
#' @param owner owner of repo
#' @param repo character: name of repository
#' @param branch branch to confirm
#' @param measure name of auxiliary repo
#'
#' @return logical
#' @export
#'
#' @examples
#' \dontrun{
#' # Exists
#' confirm_branch_exists("DEV", "regions")
#'
#' # Does not exist
#' confirm_branch_exists("ijijiji", "regions")
#' }
confirm_branch_exists <- function(branch,
                                  measure     = NULL,
                                  owner       = getOption("pipfun.ghowner"),
                                  repo        = ifelse(is.null(measure), NA,
                                                       paste0("aux_", measure))
) {

  # defenses ----------
  stopifnot(exprs = {
    is.character(repo)
    is.character(owner)
    is.character(branch)
    length(branch) == 1
  })


  tryCatch({
    # Attempt to fetch the specific branch
    branch <- gh("GET /repos/:owner/:repo/branches/:branch",
                 owner = owner,
                 repo = repo,
                 branch = branch)
    TRUE
  }, error = function(e) {
    FALSE
  })


}

#' Compare the SHA of the latest commit of two branches
#' @inheritParams confirm_branch_exists
#' @param branch1 character: name of one branch
#' @param branch2 character: name of the other branch
#' @return logical. TRUE if same SHA, FALSE otherwise
#'
compare_branches_sha <- function(owner  = getOption("pipfun.ghowner"),
                                 measure = NULL,
                                 repo        = ifelse(is.null(measure), NA,
                                                      paste0("aux_", measure)),
                                 branch1 = "main",
                                 branch2 = "DEV") {

  # Confirm branches exist
  # confirm_branch_exists(branch = branch1,
  #                       owner = owner,
  #                       repo = repo)

  # Retrieve branch info for both branches
  branch_info_1 <- get_branch_info_from_gh(owner = owner,
                                           repo = repo,
                                           branch = branch1)
  branch_info_2 <- get_branch_info_from_gh(owner = owner,
                                           repo = repo,
                                           branch = branch2)

  # Extract the SHA of the latest commit from each branch
  sha_1 <- branch_info_1$commit$sha
  sha_2 <- branch_info_2$commit$sha

  updated <- FALSE

  # Compare the SHAs
  if (sha_1 == sha_2) {
    updated <- TRUE
    cli::cli_alert_success(
      "The {.strong {cli::col_blue('SHAs')}} of the latest commits on both branches are {.strong {cli::col_blue('the same')}}."
    )
  } else {
    cli::cli_alert_warning("The {.strong {cli::col_blue('SHAs')}} of the latest commits on the branches are {.strong {cli::col_blue('different')}}.")
  }

  return(updated)
}

# Compare content of two branches
#'  nheritParams compare_branches_sha
#'

compare_branch_content <- function(owner = getOption("pipfun.ghowner"),
                                   repo,
                                   branch1 = "main",
                                   branch2 = "dev") {
  same_content <- FALSE

  # Get commit info for both branches
  commit1 <- get_commit_info_from_gh(owner = owner,
                                     repo = repo,
                                     branch = branch1)
  commit2 <- get_commit_info_from_gh(owner = owner,
                                     repo = repo,
                                     branch = branch2)

  # Extract the tree SHAs
  tree_sha1 <- commit1$commit$tree$sha
  tree_sha2 <- commit2$commit$tree$sha


  # Compare the tree SHAs
  if (tree_sha1 == tree_sha2) {
    same_content <- TRUE
    cli::cli_alert_success("The branches {branch1} and {branch2} have the same content at their latest commits.")
  } else {
    cli::cli_alert_warning("The branches {branch1} and {branch2} have different content at their latest commits.")
    cli::cli_text("Branch 1 ({branch1}) Tree SHA: {tree_sha1}")
    cli::cli_text("Branch 2 ({branch2}) Tree SHA: {tree_sha2}")
  }

  return(same_content)
}

