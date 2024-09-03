create_new_release <- function() {

}


create_new_brach <- function(measure,
                             owner       = getOption("pipfun.ghowner"),
                             repo        = paste0("aux_", measure),
                             new_release = format(Sys.Date(), "%Y%m%d"),
                             identity    = c("PROD", "INT", "TEST"),
                             ref_branch  = "DEV",
                             new_branch  = paste0(new_release, "_", identity[1]),
                             verbose     = getOption("pipfun.verbose")) {

  identity <- match.arg(identity)

  # Get branches available
  branch_available <-
    get_gh(owner = owner,
           repo = repo,
           what = "branches")

  # confirm new branch is not available

  if (new_branch %in% branch_available) {
    cli::cli_alert("branch {.field {new_branch}} already exists in repo {.field {owner}/{repo}}")
    return(invisible(TRUE))
  }


  if (!(ref_branch %in% branch_available)) {
    cli::cli_abort("reference branch {.field {ref_branch}} does not exists in repo {.field {owner}/{repo}}")
  }



  # create branch if it does not exist
  sha <- gh::gh("GET /repos/:owner/:repo/git/ref/heads/:ref",
                owner = owner,
                repo = repo,
                ref = ref_branch)$object$sha

  info_new_branch <-
    gh::gh("POST /repos/:owner/:repo/git/refs",
         owner = owner,
         repo = repo,
         ref = sprintf("refs/heads/%s", new_branch),
         sha = sha)

  new_branch_available <-
    get_gh(owner = owner,
           repo = repo,
           what = "branches")

  if (new_branch %in% new_branch_available) {
    if (verbose) cli::cli_alert_success("branch {new_branch} successfully created")
    return(invisible(TRUE))
  } else {
    cli::cli_abort("Filed creating branch {new_branch}. Check connection and try again")
  }


  # TIME <- format(Sys.time(), "%Y%m%d%H%M%S")
  # DATE <- format(Sys.Date(), "%Y%m%d")

}


delete_branch <- function(branch_to_delete,
                          owner       = getOption("pipfun.ghowner"),
                          repo        = paste0("aux_", measure)) {
}
