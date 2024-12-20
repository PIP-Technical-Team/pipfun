#' Save to GitHub
#'
#' @param df A dataframe object
#' @param metadata a list with all the information of a file, usually from
#'   [get_pip_releases]
#' @inheritParams load_from_gh
#' @return invisible NULL
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(a = 1:10, b = letters[1:10])
#' save_to_gh(df, repo = "pip_info",
#'            filename = "to_delete.csv",
#'            branch = "testing")
#' }
save_to_gh <- function(df,
                       repo,
                       owner     = getOption("pipfun.ghowner"),
                       branch    = "DEV",
                       filename  = repo,
                       ext       = "csv",
                       metadata  = NULL,
                       message   = paste("Updating data via R script on",
                                         Sys.time()),
                       verbose   = TRUE,
                       ...) {

  if (!requireNamespace("gh", quietly = TRUE)) {
    stop("Package 'gh' is required. Please install it using install.packages('gh').")
  }
  if (!requireNamespace("cli", quietly = TRUE)) {
    install.packages("cli")
    library(cli)
  }

  creds <- get_github_creds()  # Use the passed function to get GitHub credentials


  # Try to get existing SHA of the file (if it exists)
  if (is.null(metadata)) {
    # Construct the file path
    file_path <- check_filename_ext(filename, ext)

    metadata <- tryCatch({
      gh::gh(
        "GET /repos/{owner}/{repo}/contents/{file_path}",
        owner     = owner,
        repo      = repo,
        file_path = file_path,
        .params   = list(ref = branch),
        .token    = creds$password
      )
    }, error = function(e) {
      if (grepl("404", e$message)) {
        NULL  # File does not exist; will create a new file
      } else {
        cli::cli_abort(e)
      }
    })
  } else {
    file_path <- metadata$path

  }

  if(is.null(metadata)){ # To pass the tests for metadata NULL
    return(NULL)
  }

  # Convert data frame to base64-encoded content based on the file extension
  content <- convert_df_to_base64(df, ext)

  # Prepare parameters for the GitHub API request
  params <- list(
    branch  = branch,
    message = message,
    content = content
  )

  # Include 'sha' parameter if the file already exists (for updating)
  if (!is.null(metadata)) {
    params$sha <- metadata$sha
  }

  # Upload the file to GitHub
  output <- gh::gh(
    "PUT /repos/{owner}/{repo}/contents/{path}",
    owner   = owner,
    repo    = repo,
    path    = file_path,
    .params = params,
    .token  = creds$password
  )

  if (verbose) {
    cli::cli_alert_success("File {.file {file_path}} saved successfully to
    branch {.field {branch}}  of {owner}/{repo} in GitHub!")
  }

  mt <- output |>
    append(list(init = metadata))
#
#   mt <- mt |>
#     append(list(url_inf = info_from_url(output$content$url)))

  if(!is.null(mt$init$sha)){
    mt$data_change <- mt$content$sha != mt$init$sha

    if (verbose) {
      if (mt$data_change) {
        cli::cli_alert("Data has been updated")
      } else {
        cli::cli_alert("Data did not change")
      }
    }
  }

  return(invisible(mt))
}



check_filename_ext <- function(filename, ext = NULL) {
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
  filename
}

