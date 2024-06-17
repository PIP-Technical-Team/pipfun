#' Load Data from github. This function is inspired in the {gh} package.
#'
#' @param measure character: measure to be loaded
#' @param owner character: Github repo owner. Default is
#'   `getOption("pipfun.ghowner")`
#' @param repo character: name of the repo
#' @param branch character: either "DEV" or "PROD". Refers to the branch that
#'   will be used to update either the development server or production.
#' @param tag character: specific release to be used in the update.
#' @param filename character: Name of file name without the ".csv" extension.
#'   Default is `measure`
#' @param ext character: Extension of `filename`. Default "csv"
#' @param ... parameters to be passed to the loading functions depending of the
#'   extension used
#'
#' @return data.table
#' @export
load_from_gh <- function(measure,
                         owner     = getOption("pipfun.ghowner"),
                         repo      = paste0("aux_", measure),
                         branch    = "DEV",
                         tag       = branch,
                         filename  = measure,
                         ext       = "csv",
                         ...) {
  #   ____________________________________________________________________________
  #   on.exit                                                                 ####
  on.exit({

    if (exists("temp_file")) {
      if (fs::file_exists(temp_file)) {
        unlink(temp_file)
      }
    }
    # close(path)

  })

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  check_github_token()
  stopifnot(exprs = {
    length(branch) == 1
  })
  #   ____________________________________________________________________________
  #   Early returns                                                           ####
  if (FALSE) {
    return()
  }

  #   ____________________________________________________________________________
  #   Initial params                                                ####

  creds     <- gitcreds::gitcreds_get()
  my_token  <- creds$password
  path      <- glue("https://raw.githubusercontent.com/{owner}/{repo}/{tag}/{filename}.{ext}")


  path <-
    glue("https://github.com/{owner}/{repo}/raw/{tag}/{filename}.{ext}")
  # path <- file(path)

  tryCatch(
    expr = {
      # load depending of the extension
      df <-  suppressMessages(  # suppress any loading message

        if (ext == "csv") {

          readr::read_csv(path, ...)
          # data.table::fread(path, ...)

        } else if (ext  %in% c("xls", "xlsx")) {

          temp_file <- tempfile(fileext = ext)
          req <- httr::GET(path,
                           # write result to disk
                           httr::write_disk(path = temp_file))


          readxl::read_excel(path = temp_file, ...)

        } else if (ext == "dta") {

          haven::read_dta(path, ...)

        } else if (ext == "qs") {

          qs::qread(path, ...)

        } else if (ext == "fst") {

          fst::read_fst(path, ...)

        } else if (ext == "yaml") {

          yaml::read_yaml(path, ...)

        }

      )

      if (is.data.frame(df)) {
        setDT(df)
      }
    },
    # end of expr section

    error = function(e) {
      if (tag == branch) {

        ##  ............................................................................
        ##  Error in branches                                                       ####

        branches <- get_gh(owner, repo, what = "branches")

        if (!(branch  %in% branches)) {
          msg     <- c(
            "{.field branch} specified ({branch}) does not exist in repo
          {.file {owner}/{repo}}",
          "i" = "Select one among {.field {branches}}"
          )
          cli::cli_abort(msg, class = "pipaux_error")

        } else {
          msg     <- c("Problem loading {.file {filename}.{ext}} Correctly:
                     {e$message}")
          cli::cli_abort(msg, class = "pipaux_error",
                         wrap = TRUE)

        }

      } else {

        ##  ............................................................................
        ##  Error in tags                                                           ####

        tags     <- get_gh(owner, repo, what = "tags")

        if (!(tag  %in% tags)) {
          msg     <- c(
            "{.field tag} specified ({tag}) does not exist in repo
          {.file {owner}/{repo}}",
          "i" = "Select one among {.field {tags}}"
          )
          cli::cli_abort(msg, class = "pipaux_error")

        } else {
          msg     <- c("Could not load {.file {filename}.{ext}} from Github repo:
                     {e$message}")
          cli::cli_abort(msg, class = "pipaux_error")

        }
      }

    } # end of finally section

  ) # End of trycatch

  #   ____________________________________________________________________________
  #   Return                                                                  ####
  return(df)

}





#' Get tags from specific Github repo
#'
#' @param owner character: Github username that owns the repo
#' @param repo character: Github repository name
#' @param what character: either "tags" or "branches"
#'
#' @return character vector with tags
#' @keywords internal
get_gh <- function(owner,
                   repo,
                   what = c("tags", "branches")) {

  # Defenses -----------
  what <- match.arg(what)

  # Computations -------

  rs <-
    gh::gh("/repos/{owner}/{repo}/{what}",
           owner = owner,
           repo = repo,
           what = what,
           .limit = Inf)  |>
    purrr::map_chr("name")

  if (what == "tags") {
    rs <- sort(rs, decreasing = TRUE)
  }

  # Return -------------
  rs
}



# This function is a modified version from  https://gitcreds.r-lib.org/
gitcreds_msg <- function(wh) {
  msgs <- c(
    no_git = paste0(
      "No git installation found. You need to install git and set up ",
      "your GitHub Personal Access token using {.fn gitcreds::gitcreds_set}."),
    no_creds = paste0(
      "No git credentials found. Please set up your GitHub Personal Access ",
      "token using {.fn gitcreds::gitcreds_set}.",
      "Or, follow the instruction here: {.url https://happygitwithr.com/https-pat#tldr}")
  )
  cli::format_inline(msgs[wh])
}



#' make sure your GITHUB credentials are properly setup
#'
#' @return invisible TRUE if credentials are perfectly set
#' @export
#'
#' @examples
#' \dontrun{
#' check_github_token()
#' }
get_github_creds <- function() {
  # Check that either GITHUB_PAT is set or credentials have been stored using gitcreds
  # If not, abort with a message

  creds <- tryCatch(
    invisible(gitcreds::gitcreds_get()),
    gitcreds_nogit_error = function(e) cli::cli_abort("{gitcreds_msg(\"no_git\")}"),
    gitcreds_no_credentials = function(e) cli::cli_abort("{gitcreds_msg(\"no_creds\")}")
  )
  invisible(creds)

  # if (Sys.getenv("GITHUB_PAT") == "")
  #   cli::cli_abort("Enviroment variable `GITHUB_PAT` is empty. Please set it up using Sys.setenv(GITHUB_PAT = 'code')")
}


#' check if the Repo is private or not
#'
#' @inheritParams load_from_gh
#'
#' @return logical
#' @export
#'
#' @examples
#' is_private_repo("cpi")
#' is_private_repo("nan")
is_private_repo <- function(measure   = NULL,
                            owner     = getOption("pipfun.ghowner"),
                            repo      = paste0("aux_", measure)) {
  # Construct API URL
  url   <- glue("https://api.github.com/repos/{owner}/{repo}")
  creds <- get_github_creds()

  # Make the API request
  response <- httr2::request(url) |>
    httr2::req_auth_basic(username = creds$username, password = creds$password) |>
    httr2::req_perform()
  # Check status code
  if (response$status_code == 200) {
    content <- httr2::resp_body_json(response)
    return(content$private) # private is TRUE/FALSE in the response
  } else {
    cli::cli_abort("Error fetching repository information. Please check the repository name and owner.")
  }
}
