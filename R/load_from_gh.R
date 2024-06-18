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

  # prepare temp file
  # check if ext starts with .
  temp_file <- tempfile(fileext = ifelse(grepl("^\\.", ext), ext, paste0(".", ext)))

  #   ____________________________________________________________________________
  #   on.exit                                                                 ####
  on.exit({
    if (fs::file_exists(temp_file)) unlink(temp_file)
  })

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  stopifnot(exprs = {
    length(branch) == 1
  })

  #   ____________________________________________________________________________
  #   get data                                                ####

  root <- "https://raw.githubusercontent.com"
  path  <- glue("{root}/{owner}/{repo}/{tag}/{filename}.{ext}")

  # download the file
  download_from_gh(path, temp_file)

  # load temporal file from disk
  tryCatch(
    expr = {
      # load depending of the extension
      df <-  load_from_disk(temp_file, ext, ...) |>
        # suppress any loading message
        suppressMessages()

      if (is.data.frame(df)) {
        setDT(df)
      }
    },
    # end of expr section

    error = function(e) {
      cli::cli_abort(c("Error loading temp file from disk",
                       x = "{e$message}"))

    } # end of error section

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
                   what = c("tags", "branches", "releases", "contents")) {

  # Defenses -----------
  what  <- match.arg(what)
  creds <- get_github_creds()

  # Computations -------

  rs <-
    gh::gh("/repos/{owner}/{repo}/{what}",
           owner = owner,
           repo = repo,
           what = what,
           .limit = Inf,
           .token = creds$password)  |>
    vapply(\(x) x[["name"]], character(1))

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


download_from_gh <- function(path, temp_file) {

  creds = get_github_creds()

  # load temporal file from disk
  tryCatch(
    expr = {
      # using httr2 to download the file
      # Create a request object with authentication
      httr2::request(path) |>
        httr2::req_auth_basic(username = creds$username,
                              password = creds$password) |>
        httr2::req_perform() |>
        httr2::resp_body_raw() |>
        writeBin(temp_file)

    },
    # end of expr section

    error = function(e) {
      # extract owner and repo name from path of the form
      # root <- "https://raw.githubusercontent.com"
      # path  <- glue("{root}/{owner}/{repo}/{tag}/{filename}.{ext}")
      path_parts <- gsub("https://raw.githubusercontent.com/", "", path) |>
        strsplit("/") |>
        unlist()
      owner    <- path_parts[1]
      repo     <- path_parts[2]
      branch   <- path_parts[3]
      branches <- get_gh(owner, repo, what = "branches")
      tags     <- get_gh(owner, repo, what = "tags")

      if (!(branch %in% c(branches, tags))) {
        cli::cli_abort(c("{.field {branch}} is not a branch neither a tag
                       available in repo {.file {owner}/{repo}}.
                       \nAvailability:",
                       i = "tags: {.field {tags}}",
                       i = "branches: {.field {branches}}"))
      } else {
        cli::cli_abort(c("Error downloading file from github",
                         x = "{e$message}"))
      }

    } # end of error section

  ) # End of trycatch
  invisible(temp_file)

}


load_from_disk <- function(temp_file, ext, ...) {
  if (ext == "csv")  return(readr::read_csv(temp_file, ...))

  if (ext  %in% c("xls", "xlsx"))
    return(readxl::read_excel(temp_file, ...))

  if (ext == "dta") return(haven::read_dta(temp_file, ...))

  if (ext == "qs") return(qs::qread(temp_file, ...))

  if (ext == "fst") return(fst::read_fst(temp_file, ...))

  if (ext == "yaml") return(yaml::read_yaml(temp_file, ...))

  cli::cli_abort("Extension {.field {ext}} not supported")
}


