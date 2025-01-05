#' Save to GitHub
#'
#' This function uploads or updates a file in a GitHub repository. If the file
#' does not already exist, a new file will be created. If the
#' file already exists, it will be updated with the new data.
#'
#' @param df A dataframe containing the data to be uploaded or used to
#'   update an existing file. The dataframe will be converted into a base64-encoded
#'   string before being uploaded
#' @param repo A character string specifying the name of the GitHub repo
#'   where the file will be uploaded or updated
#' @param owner A character string specifying the GitHub username or organization
#'   that owns the repository. Defaults to `pipfun.ghowner` option
#' @param branch A character string specifying the branch of the repository where
#'   the file should be uploaded or updated. The default is `DEV` branch
#' @param filename A character string specifying the name of the file to be created
#'   or updated in the GitHub repository. If not provided, it defaults to repo name
#' @param ext A character string representing the file extension (e.g., `.csv`, `.json`)
#'   If `NULL`, it will be inferred from the data frame type or can be left unspecified.
#' @param metadata A list containing metadata for an existing file in the repository. Usually from [get_pip_releases]
#'   It should contain `sha` (the SHA hash of the file) and `path` (the file
#'   path in the repository). If `NULL`, the function will check whether the file exists
#'   and retrieve the metadata
#' @param verbose A logical: whether to print detailed messages
#'   about the process. The default is `TRUE`
#' @param message A character string specifying the commit message for the GitHub upload
#'   or update. The default is a message with the current timestamp
#'
#' @return
#' Returns `invisible(NULL)`. The function primarily performs an upload or update
#' operation and does not return any value other than invisibly indicating the completion
#' of the task.
#'
#' @examples
#' \dontrun{
#'   # Create a new file on GitHub
#'   df <- data.frame(a = 1:5, b = letters[1:5])
#'   save_to_gh(df = df, repo = "aux_test", filename = "data.csv", ext = "csv")
#'
#'   # Update an existing file on GitHub
#'   df <- data.frame(a = 6:10, b = letters[6:10])
#'   save_to_gh(df = df, repo = "aux_test", filename = "data.csv", ext = "csv")
#' }
#' @export
#'
save_to_gh <- function(df,
                      repo,
                      owner    = getOption("pipfun.ghowner"),
                      branch   = "DEV",
                      filename = repo,
                      ext      = NULL,
                      metadata = NULL,
                      verbose  = TRUE,
                      message  = paste("Updating data via R script on", Sys.time())) {

  # Ensure the required packages are installed
  if (!requireNamespace("gh", quietly = TRUE)) {
    stop("Package 'gh' is required. Please install it using install.packages('gh').")
  }

  if (!requireNamespace("cli", quietly = TRUE)) {
    install.packages("cli")
    library(cli)
  }

  # Get GitHub credentials
  creds <- get_github_creds()

  # Convert the data frame to base64-encoded content based on the file extension
  content <- convert_df_to_base64(df, ext)

  # Prepare params for GitHub request
  params <- list(
    branch  = branch,
    message = message,
    content = content
  )

  # Check if metadata is provided and is valid
  if (!is.null(metadata) && (!"sha" %in% names(metadata) || !"path" %in% names(metadata))) {
    cli::cli_abort("Invalid metadata provided. It must contain 'sha' and 'path'.")
  }

  # Version control: check if the file already exists in the repo
  if (is.null(metadata)) {
    # Construct the file path
    file_path <- check_filename_ext(filename, ext)

    # Attempt to retrieve metadata (file info) from GitHub
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
  }

  if (!is.null(metadata)) {
    # If metadata exists, get the file path and SHA
    file_path <- metadata$path
    params$sha <- metadata$sha  # Include SHA for updating an existing file
  } else {
    # If no metadata, this is a new file, so set the file path for creation
    file_path <- check_filename_ext(filename, ext)
    params$sha <- NULL
  }

  # Upload the file to GitHub
  output <- gh::gh(
    "PUT /repos/{owner}/{repo}/contents/{path}",
    owner   = owner,
    repo    = repo,
    path    = file_path,
    message = message,  # Commit message
    content = content,
    .params = params,  # Base64-encoded file content
    sha     = params$sha,  # Include SHA directly in the body of the request if updating
    .token  = creds$password
  )

  # Update metadata: store initial metadata and URL info
  mt <- output |>
    append(list(init = metadata)) |>   # 'init' will be NULL if file didn't exist before PUT request
    append(info_from_url(output$content$url))

  # Track if data has changed
  if (!is.null(mt$init$sha)) {
    # If SHA exists in 'init', compare the current and previous SHAs
    mt$data_change <- mt$content$sha != mt$init$sha
  } else {
    # If the file was newly created (no initial SHA), set data_change to TRUE
    mt$data_change <- TRUE
  }

  # If verbose, print success and data change status
  if (verbose) {
    cli::cli_alert_success(
      "File {.file {filename}.{ext}} saved successfully to branch {.field {branch}} of {owner}/{repo} in GitHub!"
    )
  }

  if (verbose) {
    cli::cli_alert(
      if (mt$data_change) "Data has been updated" else "Data did not change"
    )
  }

  return(invisible(mt))
}



# Helper function to convert data frame to base64-encoded content based on file extension
convert_df_to_base64 <- function(df, ext = "csv") {
  if (is.null(ext))
    ext <- "csv"

  ext <- tolower(ext)


  if (ext == "csv") {

    content <- readr::format_csv(df)
    encoded <- base64enc::base64encode(charToRaw(content))
    return(encoded)

  } else if (ext == "json") {
    content <- jsonlite::toJSON(df, pretty = TRUE, auto_unbox = TRUE)
    encoded <- base64enc::base64encode(charToRaw(content))
    return(encoded)

  } else if (ext == "rds") {
    raw_content <- serialize(df, NULL)
    encoded <- base64enc::base64encode(raw_content)
    return(encoded)

  } else if (ext == "qs") {
    raw_content <- qs::qserialize(df)
    encoded <- base64enc::base64encode(raw_content)
    return(encoded)

  } else if (ext == "fst") {
    temp_file <- tempfile(fileext = ".fst")
    on.exit(unlink(temp_file), add = TRUE)
    fst::write_fst(df, temp_file)
    raw_content <- readBin(temp_file, what = "raw", n = file.info(temp_file)$size)
    encoded <- base64enc::base64encode(raw_content)
    return(encoded)

  } else if (ext == "dta") {
    temp_file <- tempfile(fileext = ".dta")
    on.exit(unlink(temp_file), add = TRUE)
    haven::write_dta(df, temp_file)
    raw_content <- readBin(temp_file, what = "raw", n = file.info(temp_file)$size)
    encoded <- base64enc::base64encode(raw_content)
    return(encoded)

  } else {
    cli::cli_abort("Unsupported file extension: {.ext {ext}}")
  }
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


