#' Load Data from github. This function is inspired from the gh package.
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
                         ext       = NULL,
                         ...) {


  #   ____________________________________________
  #   on.exit                                      ####
  on.exit({
    if (fs::file_exists(temp_file)) unlink(temp_file)
  })

  #   ______________________________________________________
  #   Defenses                                            ####
  stopifnot(exprs = {
    length(branch) == 1
  })

  # check ext of filename -------
  fext <- fs::path_ext(filename) |>
    tolower()

  if (is.null(ext) && fext == "") {
    cli::cli_abort("You need provide either a {.arg filename} with extension
                   or an {.arg ext} in the arguments")
  } else if (!is.null(ext) && fext != "" && fext != ext) {
    cli::cli_warn("The extension of the file ({.field {fext}}) is different
                  from the one in the {.arg ext} argument ({.field {ext}}).
                  {.field {fext} will be used")

  } else if (!is.null(ext) && fext == "") {
    filename <- fs::path(filename, ext = ext)
  }



  # prepare temp file ----------
  # check if ext starts with .
  temp_file <- fs::file_temp(ext = fs::path_ext(filename) |>
                               tolower())


  #   _____________________________________________________
  #   get data                                            ####

  root <- "https://raw.githubusercontent.com"
  path  <- glue("{root}/{owner}/{repo}/{tag}/{filename}")

  # download the file
  download_from_gh(path, temp_file)

  # load temporal file from disk
  tryCatch(
    expr = {
      # load depending of the extension
      df <-  load_from_disk(temp_file,...) |>
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

  #   __________________________________________________
  #   Return                                ####
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
#' \dontrun{
#' is_private_repo("cpi")
#' is_private_repo("nan")
#' }
is_private_repo <- function(measure   = NULL,
                            owner     = getOption("pipfun.ghowner"),
                            repo      = paste0("aux_", measure)) {
  # Construct API URL
  url   <- glue("https://api.github.com/repos/{owner}/{repo}")
  creds <- get_github_creds()

  # Make the API request
  response <- url |>
    httr2::request() |>
    httr2::req_auth_basic(username = creds$username,
                          password = creds$password) |>
    httr2::req_perform()
  # Check status code
  if (response$status_code == 200) {
    content <- httr2::resp_body_json(response)
    return(content$private) # private is TRUE/FALSE in the response
  } else {
    cli::cli_abort("Error fetching repository information. Please check the repository name and owner.")
  }
}






