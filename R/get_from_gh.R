# The objective of all the functions below is to provide a suite of interactive
# tools to work with github files seamlessly. Ideally, these functions will
# fully supersede the functions in load_from_gh.R


#' Get file from Github
#'
#' @inheritParams get_file_info_from_gh
#' @return a file in a data.table class
#' @export
#'
#' @examples
#' get_file_from_gh(owner     = getOption("pipfun.ghowner"),
#'                  repo      = "pip_info",
#'                  file_path = "releases.csv",
#'                  branch    = "releases")
get_file_from_gh <- function(owner= getOption("pipfun.ghowner"),
                             repo,
                             branch = "main",
                             file_path) {


  # Fetch the content metadata using gh, with authentication
  metadata <- get_file_info_from_gh(owner     = owner,
                                    repo      = repo,
                                    branch    = branch,
                                    file_path = file_path)

  # Determine the file type
  file_type <- fs::path_ext(file_path)

  # Switch to handle different file types
  data <-
    switch(
      file_type,
      rds = {
        # Decode base64 content and read RDS
        # rawContent <- base64enc::base64decode(metadata$content)
        # readRDS(rawConnection(rawContent))

        metadata$content |>
          base64enc::base64decode() |>
          rawConnection() |>
          readRDS()
      },
      json = {
        # Parse JSON from base64-encoded content
        # json_content <-
        # rawToChar(base64enc::base64decode(metadata$content))
        # jsonlite::fromJSON(json_content)

        metadata$content |>
          base64enc::base64decode() |>
          rawToChar() |>
          jsonlite::fromJSON()
      },
      csv = {
        # Parse csv from base64-encoded content

        metadata$content |>
          base64enc::base64decode() |>
          rawToChar() |>
          fread()
      },
      download_and_read_file(metadata$download_url)

    ) |>
    setDT()

  data

}

#' Download and read file
#'
#' Helper function to handle file downloads and reading
#'
#' @param url character: url of file. usually it comes
#'   [get_file_info_from_gh()$download_url]
#' @param type character: file format
#'
#' @return data in data.table format
#' @keywords internal
download_and_read_file <- function(path) {
  type      <- fs::path_ext(path)
  temp_file <- tempfile(fileext = paste0(".", type))
  on.exit(unlink(temp_file))
  temp_file <- download_from_gh(path, temp_file)

  load_from_disk(temp_file) |>
    setDT()
}

#' Download file from Github
#'
#' @param path character: URL of file
#' @param temp_file [tempfile()] where new file will be saved
#'
#' @return file of extension in [path]
#' @keywords internal
#'
#' @examples
download_from_gh <- function(path, temp_file) {

  creds = get_github_creds()

  # load temporal file from disk
  tryCatch(
    expr = {
      # using httr2 to download the file
      # Create a request object with authentication
      path |>
        httr2::request() |>
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
      file_path   <- paste(path_parts[4:length(path_parts)], collapse =  "/")
      branches <- get_gh(owner, repo, what = "branches")
      tags     <- get_gh(owner, repo, what = "tags")

      if (!(branch %in% c(branches, tags))) {
        cli::cli_abort(c("{.field {branch}} is not a branch neither a tag
                       available in repo {.file {owner}/{repo}}.
                       \nAvailability:",
                         i = "tags: {.field {tags}}",
                         i = "branches: {.field {branches}}"))
      } else {
        cli::cli_abort(c(x = "Error downloading file from github",
                         i = "check file {.file {file_path}} exists",
                         x = "{e$message}"))
      }

    } # end of error section

  ) # End of trycatch
  invisible(temp_file)
}



#' Load data from tempfile
#'
#' this is usually executed right after [download_from_gh()]
#'
#' @param temp_file temporal file
#' @param ... additional parameter to pass to corresponding reading function
#'
#' @return data from disk
#' @keywords internal
#'
#' @examples
load_from_disk <- function(temp_file, ...) {
  ext <- fs::path_ext(temp_file) |>
    tolower()
  data <- switch(ext,
                 csv  = readr::read_csv(temp_file,
                                        show_col_types = FALSE,
                                        ...),
                 xls  = readxl::read_excel(temp_file, ...),
                 xlsx = readxl::read_excel(temp_file, ...),
                 dta  = haven::read_dta(temp_file, ...),
                 qs   = qs::qread(temp_file, ...),
                 fst  = fst::read_fst(temp_file, ...),
                 yaml = yaml::read_yaml(temp_file, ...),
                 rds  = readr::read_rds(temp_file, ...),
                 json = jsonlite::fromJSON(temp_file, ...),
                 cli::cli_abort("Extension {.field {ext}} not supported")
  )

  return(data)
}


#' Get info of a file in a Github repo
#'
#'
#' @param owner character: owner of repo
#' @param repo character: repository name
#' @param file_path character: file path
#' @param branch character: branch where the file is
#'
#' @return Complete response from GET method of Github API
#' @export
#'
#' @examples
#' get_file_info_from_gh(owner     = getOption("pipfun.ghowner"),
#'                       repo      = "pip_info",
#'                       file_path = "releases.csv",
#'                       branch    = "releases")
get_file_info_from_gh <- function(owner= getOption("pipfun.ghowner"),
                                  repo,
                                  branch = "main",
                                  file_path) {



  creds = get_github_creds()

  # alternative
  # gh::gh("GET /repos/:owner/:repo/contents/:path",
  #        owner = owner, repo = repo, path = path, ref = ref,
  #        .token = Sys.getenv("GITHUB_PAT"))

  gh::gh(
    "GET /repos/{owner}/{repo}/contents/{file_path}",
    owner     = owner,
    repo      = repo,
    file_path = file_path,
    .params   = list(ref = branch),
    .token = creds$password
  )

}


