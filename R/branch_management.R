#' Create new branch in repo
#'
#' By default it will create a new branch in aux PIP repo, but it can create a
#' new branch in repo
#'
#' @inheritParams load_from_gh
#' @param ref_branch Character: reference branch from which the new branch will be
#'   created.
#' @param new_branch character: name of new branch. Default is
#'   `paste0(release, "_", identity)`
#' @param verbose A logical: whether to print detailed messages
#'   about the process. The default is `TRUE`
#'
#' @return TRUE if [new_branch] already exists or if it was created
#' @export
#'
#' @examples
#' \dontrun{
#' # success
#'   create_new_branch("regions",
#'   release = "20240903")
#'
#'   create_new_branch("regions",
#'   new_branch = "test")
#'
#' # Fail
#'   create_new_branch("fjfjf",
#'   new_branch = "test") |>
#'   try()
#' }
create_new_branch <- function(measure     = NULL,
                              owner       = getOption("pipfun.ghowner"),
                              repo        = ifelse(is.null(measure), NA,
                                                    paste0("aux_", measure)) ,
                              #release     = format(Sys.Date(), "%Y%m%d"),
                              #identity    = getOption("pipfun.identities"),
                              ref_branch  = "DEV",
                              new_branch  = NULL,
                              verbose     = getOption("pipfun.verbose")) {


  # Get working release
  pipfun::get_wrk_release()

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  release_branch <- paste0(release, "_", identity)

  if (is.null(new_branch)) {
    new_branch <- paste0(release, "_", identity)
  }

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
#' @inheritParams create_new_branch
#' @param ask logical: whether to ask the user to confirm. Default is [interactive()]
#' @param verbose A logical: whether to print detailed messages
#'   about the process. The default is `TRUE`
#'
#' @return logical, whether or not branch was deleted
#' @export
#'
#' @examples
#' \dontrun{
#' create_new_branch(
#'   measure = "regions",
#'   release = "20240903")
#'
#' delete_branch(branch_to_delete = "20240903_PROD",
#'               measure = "regions",
#'               ask = FALSE)
#'
#' create_new_branch("regions",
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

  #Confirm branch exist -------
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
  stopifnot(
    is.character(repo),
    is.character(owner),
    is.character(branch),
    length(branch) == 1
  )

  result <- tryCatch({
    gh::gh("GET /repos/:owner/:repo/branches/:branch",
           owner = owner,
           repo = repo,
           branch = branch)
    TRUE  # Branch exists
  }, error = function(e) {
    if (grepl("404", e$message)) {
      FALSE  # Branch does not exist
    } else {
      stop(e)  # Re-raise other errors
    }
  })

  return(result)

}

#' Compare the SHA of the latest commit of two branches
#' @inheritParams confirm_branch_exists
#' @param branch1 character: name of one branch
#' @param branch2 character: name of the other branch
#' @return list of 3 elements: sha1, sha2 and updated (logical, TRUE if sha codes are equal)
#' @export
#'
compare_branches_sha <- function(owner  = getOption("pipfun.ghowner"),
                                 measure = NULL,
                                 repo        = ifelse(is.null(measure), NA,
                                                      paste0("aux_", measure)),
                                 branch1 = "main",
                                 branch2 = "DEV") {

  # Confirm branches exist
  confirm_branch_exists(branch = branch1,
                        owner = owner,
                        repo = repo)

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

  return(list(sha_1   = sha_1,
              sha_2   = sha_2,
              updated = updated))
}

#' Compare content of two branches
#' @inheritParams compare_branches_sha
#' @return list of 3 elements: tree sha of branch 1, tree sha of branch 2 and "same content" (TRUE if branches have same content, FALSE otherwise)
#' @export
#' @examples
#' \dontrun{
#' # Different content
#' compare_branch_content(repo   = "aux_ppp",
#'                       branch1 = "DEV",
#'                       branch2 = "DEV_v2")
#'
#' }
compare_branch_content <- function(owner = getOption("pipfun.ghowner"),
                                   repo,
                                   branch1 = "main",
                                   branch2 = "dev",
                                   verbose = TRUE) {
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
    if (verbose) cli::cli_alert_success("The branches {.strong {cli::col_blue(branch1)}} and {.strong {cli::col_blue(branch2)}} have the same content at their latest commits.")
  } else {
    if (verbose) cli::cli_alert_warning("The branches {.strong {cli::col_blue(branch1)}} and {.strong {cli::col_blue(branch2)}} have different content at their latest commits.")
  }

  return(list(
    tree_sha_1 = tree_sha1,
    tree_sha_2 = tree_sha2,
    same_content = same_content
  ))
}

#' Get branches from a GitHub repository
#' @inheritParams compare_branches_sha
#' @return vector with names of branches in specified repository
#' @export
get_repo_branches <- function(owner = getOption("pipfun.ghowner"),
                              repo) {
  # Use the GitHub API to get the branches of the repository
  branches_info <- gh::gh(
    "GET /repos/:owner/:repo/branches",
    owner = owner,
    repo = repo,
    .limit = Inf
  )

  # Extract and return branch names
  branch_names <- sapply(branches_info,
                         function(branch) branch$name)
  # Check if any branch matches the release pattern (8 digits)
  release_pattern <- "^\\d{8}"
  release_branches <- grep(release_pattern,
                           branch_names,
                           value = TRUE)

  # Return all branch names and indicate if release branches are found
  ret <- list(
    all_branches       = branch_names,
    release_branches   = release_branches,
    has_release_branch = length(release_branches) > 0
  )

  return(ret)
}

# -----------------------------
# Update branches #########
# -----------------------------
# Option 1:  let branch 2 point to the same commit as branch 1 ------ #

#' Update Branches of a GitHub Repository
#'
#' This function compares the commit history and content between two branches of a GitHub repository.
#' If the content of the branches is different, it updates `branch2` to match the latest commit of `branch1`.
#'
#' @param owner The GitHub username or organization name. Defaults to the option `"pipfun.ghowner"` if not specified.
#' @param repo The name of the GitHub repository.
#' @param branch1 The source branch whose latest commit is used to update `branch2`.
#' @param branch2 The target branch that will be updated to match the latest commit of `branch1`.
#' @param force logical. If `FALSE`, ask permission to user before merging. Default is TRUE
#'
#' @return Returns `TRUE` if the update was successful or if the branches were already up-to-date, `FALSE` if an error occurred during the update.
#' @export
update_branches <- function(owner = getOption("pipfun.ghowner"),
                            repo,
                            branch1,
                            branch2,
                            force = TRUE
) {

  # Update branch 2 based on branch 1 latest commit

  # Check sha of latest commit
  branches_sha <- compare_branches_sha(repo = repo,
                                       owner = owner,
                                       branch1 = branch1,
                                       branch2 = branch2)

  # Check tree sha of latest commit
  branches_content <- compare_branch_content(repo = repo,
                                             owner = owner,
                                             branch1 = branch1,
                                             branch2 = branch2)

  # Do nothing if branches already have same content
  if (branches_content$same_content) {
    cli::cli_alert_warning("Branches are already up-to-date.")
    return(TRUE)
  }

  if (force == FALSE) {

    Ask <- utils::askYesNo(msg     = "Do you want to proceed with the update? Type your answer",
                    default = TRUE,
                    prompts = c("Yes", "No", "Cancel"))

    if (Ask == FALSE | is.na(Ask)) {
      cli::cli_abort(message = "Update interrupted.
                                No action taken on branches")}

  }
  # If different content, update branch 2 -say, release branch- based on branch 1
  result <- tryCatch({
    gh::gh(
      "PATCH /repos/:owner/:repo/git/refs/heads/:branch",
      owner = owner,
      repo = repo,
      branch = branch2,
      sha = branches_sha$sha_1,
      force = TRUE
    )
    message("Branch ", branch2, " successfully updated to match ", branch1)
    #cli::cli_alert_success("Branch '{branch2}' successfully updated to match '{branch1}'.")
    TRUE  # Update successful
  }, error = function(e) {
    message("Error updating the branch: ", e$message)
    #cli::cli_alert_warning("Error updating the branch: {e$message}")
    FALSE  # Update failed
  })

  return(result)
}

# Option 2: merge branch 1 into branch 2

#' Merge a Source Branch into a Target Branch
#'
#' This function merges the content of a source branch into a target branch
#' within a specified GitHub repo. It uses the GitHub API to ensure
#' that the target branch is updated with the latest changes from the source
#' branch while preserving commit history.
#'
#' @param owner Character. The GitHub username that owns the repository
#'   Defaults to the value of the `pipfun.ghowner` option
#' @param repo Character. The name of the repository.
#' @param source_branch Character. The name of the branch to merge from
#' @param target_branch Character. The name of the branch to merge into
#' @param force logical. If `FALSE`, ask permission to user before merging. Default is TRUE
#'
#' @return Logical. Returns `TRUE` if the merge was successful or the branches
#'   already had the same content. Returns `FALSE` if the merge failed.
#'
#' @export
#'
#' @details The function first checks whether the branches already have the
#'   same content by comparing their latest commit tree SHAs. If the branches
#'   are identical, no action is taken. Otherwise, the function performs a
#'   merge operation using GitHub's API. A descriptive commit message is added
#'   to document the merge.
#'
#'
#' @examples
#' \dontrun{
#'   owner <- getOption("pipfun.ghowner")            # GitHub username
#'   repo <- "aux_test"         # Repository name
#'   source_branch <- "DEV" # Branch to merge from
#'   target_branch <- "20240512"     # Branch to merge into
#'
#'   merge_branch_into(owner, repo, source_branch, target_branch)
#' }
merge_branch_into <- function(owner = getOption("pipfun.ghowner"),
                              repo,
                              source_branch,
                              target_branch,
                              force = TRUE) {

  # Check tree SHA of latest commits
  branches_content <- compare_branch_content(
    repo = repo,
    owner = owner,
    branch1 = source_branch,
    branch2 = target_branch
  )

  # Do nothing if branches already have the same content
  if (branches_content$same_content) {
    cli::cli_alert_warning("Branches are already up-to-date.")
    return(TRUE)
  }

  if (force == FALSE) {

    Ask <- utils::askYesNo(msg     = "Do you want to proceed with merging? Type your answer",
                    default = TRUE,
                    prompts = c("Yes", "No", "Cancel"))

    if (Ask == FALSE | is.na(Ask)) {
      cli::cli_abort(message = "Merge interrupted.
                                No action taken on branches")}



  }
  # Create a merge of source_branch into target_branch
  result <- tryCatch({
    merge_result <- gh::gh(
      "POST /repos/:owner/:repo/merges",
      owner = owner,
      repo = repo,
      base = target_branch,
      head = source_branch,
      commit_message = paste("Merge branch", source_branch, "into", target_branch)
    )
    cli::cli_alert_success("Branch '{target_branch}' successfully updated to include changes from '{source_branch}'.")
    TRUE
  }, error = function(e) {
    cli::cli_alert_warning("Error merging branches: {e$message}")
    FALSE
  })

  return(result)
}

#' Create or Update release branch of a GH repo
#'
#' This function checks if a GitHub repository has a release branch. If a release branch exists, it updates it with the latest `DEV` branch.
#' If no release branch exists, it creates a new one from `DEV`
#'
#' @param owner Character. The GitHub owner or organization name. Defaults to `getOption("pipfun.ghowner")`
#' @param repo Character. The name of the repository.
#' @param ref_branch Character. The branch from which the release branch should be created or updated. Defaults to `"DEV"`
#' @param release TBC
#' @param identity Character. The identity used for naming the new branch if created. One of `getOption("pipfun.identities")`
#'
#' @return Invisible `TRUE` if the process succeeds, otherwise an error message is displayed
#' @export
#'
#' @examples
#' \dontrun{
#' sync_release_branch(owner = "PIP-Technical-Team", repo = "aux_gdp")
#' }
sync_release_branch <- function(owner       = getOption("pipfun.ghowner"),
                                repo,
                                ref_branch  = "DEV",
                                #release     = NULL,
                                #identity    = c("TEST", "PROD", "INT"),
                                target_branch) {

  #identity       <- match.arg(identity)
  #release_branch <- paste0(release, "_", identity)

  # Get repository branches
  branches_info <- get_repo_branches(owner = owner,
                                     repo  = repo)

  if (release_branch %in% branches_info$release_branches) {

    # If a release branch exists, update it with most recent version of DEV
    cli::cli_alert_info("Updating existing target branch: {.field {target_branch}}")

    update_branches(owner   = owner,
                    repo    = repo,
                    branch1 = ref_branch,
                    branch2 = target_branch)
  } else {

    # If release branch does not exist, create it
    cli::cli_alert_info("Target branch not found. Creating a new one.")
    create_new_branch(owner      = owner,
                      repo       = repo,
                      ref_branch = ref_branch,
                      release    = release,
                      identity   = identity)
  }
}

