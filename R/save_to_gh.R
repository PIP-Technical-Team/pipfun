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
# save_to_gh <- function(df,
#                        repo,
#                        owner     = getOption("pipfun.ghowner"),
#                        branch    = "DEV",
#                        filename  = repo,
#                        ext       = NULL,
#                        metadata  = NULL,
#                        message   = paste("Updating data via R script on",
#                                          Sys.time()),
#                        verbose   = TRUE,
#                        ...) {
#
#   if (!requireNamespace("gh", quietly = TRUE)) {
#     stop("Package 'gh' is required. Please install it using install.packages('gh').")
#   }
#   if (!requireNamespace("cli", quietly = TRUE)) {
#     install.packages("cli")
#     library(cli)
#   }
#
#   creds <- get_github_creds()  # Use the passed function to get GitHub credentials
#
#
#   # Try to get existing SHA of the file (if it exists)
#   if (is.null(metadata)) {
#     # Construct the file path
#     file_path <- check_filename_ext(filename, ext)
#
#     metadata <- tryCatch({
#       gh::gh(
#         "GET /repos/{owner}/{repo}/contents/{file_path}",
#         owner     = owner,
#         repo      = repo,
#         file_path = file_path,
#         .params   = list(ref = branch),
#         .token    = creds$password
#       )
#     }, error = function(e) {
#       if (grepl("404", e$message)) {
#         NULL  # File does not exist; will create a new file
#       } else {
#         cli::cli_abort(e)
#       }
#     })
#   } else {
#     file_path <- metadata$path
#
#   }
#
#   # Convert data frame to base64-encoded content based on the file extension
#   content <- convert_df_to_base64(df, ext)
#
#   # Prepare parameters for the GitHub API request
#   params <- list(
#     branch  = branch,
#     message = message,
#     content = content
#   )
#
#   # Include 'sha' parameter if the file already exists (for updating)
#   if (!is.null(metadata)) {
#     params$sha <- metadata$sha
#   }
#
#   # Upload the file to GitHub
#   output <- gh::gh(
#     "PUT /repos/{owner}/{repo}/contents/{path}",
#     owner   = owner,
#     repo    = repo,
#     path    = file_path,
#     .params = params,
#     .token  = creds$password
#   )
#
#   if (verbose) {
#     cli::cli_alert_success("File {.file {filename}.{ext}} saved successfully to
#     branch {.field {branch}}  of {owner}/{repo} in GitHub!")
#   }
#
#   mt <- output |>
#     append(list(init = metadata)) |>
#     append(info_from_url(output$content$url))
#
#   mt$data_change <- mt$content$sha != mt$init$sha
#
#   if (verbose) {
#     if (mt$data_change) {
#       cli::cli_alert("Data has been updated")
#     } else {
#       cli::cli_alert("Data did not change")
#     }
#   }
#
#   return(invisible(mt))
# }


# RT New version of save to github

save_file_to_gh <- function(df,
                            repo,
                            owner = getOption("pipfun.ghowner"),
                            branch = "DEV",
                            filename = repo,
                            ext = NULL,
                            metadata = NULL,
                            verbose = TRUE,
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
  } else {
    # If metadata is provided, get the file path and SHA
    file_path <- metadata$path
    params$sha <- metadata$sha  # Include SHA for updating an existing file
  }

  # Upload the file to GitHub
  output <- gh::gh(
    "PUT /repos/{owner}/{repo}/contents/{path}",
    owner   = owner,
    repo    = repo,
    path    = file_path,
    message = message,  # Commit message
    content = content,  # Base64-encoded file content
    sha     = params$sha,  # Include SHA directly in the body of the request
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


