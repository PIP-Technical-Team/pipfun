# The objective of all the functions below is to provide a suite of interactive
# tools to work with github files or folders seamlessly. Ideally, these functions will
# fully supersede the functions in load_from_gh.R


#' Get file from Github
#'
#' @inheritParams get_file_info_from_gh
#' @inheritParams download_and_read_file
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
                             file_path,
                             creds = NULL) {


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
      download_and_read_file(metadata$download_url, creds = creds)

    ) |>
    setDT()

  setattr(data, "metadata", metadata)
  data
}

#' Download and read file
#'
#' Helper function to handle file downloads and reading
#'
#' @param url character: url of file. usually it comes
#'   `get_file_info_from_gh()$download_url`
#' @param creds  list. Basically, it is `get_github_creds()`
#'
#' @return data in data.table format
#' @keywords internal
download_and_read_file <- function(url, creds = NULL) {
  type      <- fs::path_ext(url)
  temp_file <- tempfile(fileext = paste0(".", type))
  on.exit(unlink(temp_file))
  temp_file <- download_from_gh(url = url,
                                temp_file = temp_file,
                                creds = creds)

  load_from_disk(temp_file) |>
    setDT()
}

#' Download file from Github
#'
#' @inheritParams download_and_read_file
#' @param temp_file [tempfile()] where new file will be saved
#'
#' @return file of extension in [path]
#' @keywords internal
download_from_gh <- function(url,
                             temp_file,
                             creds = NULL) {

  if (is.null(creds)) {
    creds = get_github_creds()
  }

  # load temporal file from disk
  tryCatch(
    expr = {
      # using httr2 to download the file
      # Create a request object with authentication
      url |>
        httr2::request() |>
        httr2::req_auth_basic(username = creds$username,
                              password = creds$password) |>
        httr2::req_perform() |>
        httr2::resp_body_raw() |>
        writeBin(temp_file)

    },
    # end of expr section

    error = function(e) {
      # extract owner and repo name from url of the form
      # root <- "https://raw.githubusercontent.com"
      # url  <- glue("{root}/{owner}/{repo}/{tag}/{filename}.{ext}")
      path_parts <- gsub("https://raw.githubusercontent.com/", "", url) |>
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
load_from_disk <- function(temp_file, ...) {
  ext <- fs::path_ext(temp_file) |>
    tolower()
  data <- switch(ext,
                 csv  = readr::read_csv(temp_file,
                                        show_col_types = FALSE,
                                        col_names = TRUE),
                                        #...),
                 xls  = readxl::read_excel(temp_file, ...),
                 xlsx = readxl::read_excel(temp_file, ...),
                 #dta  = haven::read_dta(temp_file, ...),
                 dta  = haven::read_dta(temp_file, encoding = "UTF-8", ...),
                 qs   = qs::qread(temp_file, ...),
                 fst  = fst::read_fst(temp_file, ...),
                 yaml = yaml::read_yaml(temp_file, ...),
                 rds  = readr::read_rds(temp_file, ...),
                 json = jsonlite::fromJSON(temp_file, ...),
                 cli::cli_abort("Extension {.field {ext}} not supported")
  )

  return(data)
}

#' Get all files from folder in Github
#'
#' @inheritParams get_file_info_from_gh
#' @param folder_path character: folder path
#' @param output_path character: folder path
#'
#' @return file of extension in [path]
#' @export
#'
#' @examples
#' \dontrun{
#' load_all_from_gh(owner     = getOption("pipfun.ghowner"),
#'                  repo      = "pipfaker",
#'                  folder_path = "data/20240627_2017_01_02_PROD/_aux",
#'                  branch    = "aux_estimations",
#'                  output_path = getwd())
#'                  }
load_all_from_gh <- function(owner= getOption("pipfun.ghowner"),
                             repo,
                             branch = "main",
                             folder_path,
                             output_path){

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Fetch the content metadata using gh, with authentication

  metadata      <- get_file_info_from_gh(owner     = owner,
                                         repo      = repo,
                                         branch    = branch,
                                         file_path = folder_path)

  # Create lists of paths

  urls         <- lapply(metadata,
                         function(df) df$url)

  output_paths <- lapply(as.list(names(urls)),
                         function(base) path <- fs::path(output_path, base))

  # Download all files

  temp_files   <- mapply(download_from_gh, urls, output_paths)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(invisible(temp_files))

}


#' Get info of a file or files within a folder in a Github repo
#'
#'
#' @param owner character: owner of repo
#' @param repo character: repository name
#' @param file_path character: file or folder path
#' @param branch character: branch where the file or folder is
#'
#' @return Complete response from GET method of Github API
#' @export
#'
#' @examples
#' get_file_info_from_gh(owner     = getOption("pipfun.ghowner"),
#'                       repo      = "pip_info",
#'                       file_path = "releases.csv",
#'                       branch    = "releases")
#'
#' get_file_info_from_gh(owner     = getOption("pipfun.ghowner"),
#'                       repo      = "pipfaker",
#'                       file_path = "data/20240627_2017_01_02_PROD/_aux",
#'                       branch    = "aux_estimations")
get_file_info_from_gh <- function(owner= getOption("pipfun.ghowner"),
                                  repo,
                                  branch = "main",
                                  file_path) {



  creds = get_github_creds()
  mt <- gh::gh(
    "GET /repos/{owner}/{repo}/contents/{file_path}",
    owner     = owner,
    repo      = repo,
    file_path = file_path,
    .params   = list(ref = branch),
    .token = creds$password
  )

  # Fix names for folders

  if(is.null(names(mt))){
    names(mt) <- lapply(mt, function(x) names(x) <- x$name)

    return(mt)
  }

  # For files

  append(mt, info_from_url(mt$url))


}


info_from_url <- function(url) {
  split_url <- url |>
    strsplit("/", fixed = TRUE) |>
    unlist()

  repos_pos <- which(split_url == "repos")

  owner  <-  split_url[repos_pos + 1]
  repo   <-  split_url[repos_pos + 2]

  branch_pattern <- "(.*ref=)(.*)"
  branch <-  gsub(branch_pattern, "\\2", split_url[repos_pos + 4])


  list(owner = owner,
       repo  = repo,
       branch = branch)
}

#' Get info of latest commit of a GitHub repo
#' @param owner character: owner of repo
#' @param repo character: repository name
#' @param branch character: branch name (default is "main")
#' @return A list containing detailed information about the latest commit on the specified branch.
#' @keywords internal
get_commit_info_from_gh <- function(owner = getOption("pipfun.ghowner"),
                                    repo,
                                    branch = "main") {
  # Get GitHub credentials
  creds <- gitcreds::gitcreds_get()

  # Fetch the latest commit of the branch
  commit_info <- gh::gh(
    "GET /repos/{owner}/{repo}/branches/{branch}",
    owner  = owner,
    repo   = repo,
    branch = branch,
    .token = creds$password
  )

  # Return the commit details
  return(commit_info$commit)
}

#' Get info of a branch in a GitHub repo
#'
#' @param owner character: owner of repo
#' @param repo character: repository name
#' @param branch character: branch name (default is "main")
#' @param gh_func function: function used to call the GitHub API (default is `gh::gh`)
#' @param creds_func function: function used to retrieve GitHub credentials (default is `get_github_creds`)
#' @param url_func function: function used to extract additional information from the protection URL (default is `info_from_url`)
#'
#'
#' @return Complete response from GET method of GitHub API
#' @export
#'
#' @examples
#' get_branch_info_from_gh(owner     = getOption("pipfun.ghowner"),
#'                         repo      = "pip_info",
#'                         branch    = "releases")
get_branch_info_from_gh <- function(owner = getOption("pipfun.ghowner"),
                                    repo,
                                    branch = "main",
                                    gh_func = gh::gh,
                                    creds_func = get_github_creds,
                                    url_func = info_from_url) {
  # Get GitHub credentials
  creds <- creds_func()

  # Fetch branch metadata using GitHub API
  mt <- gh_func(
    "GET /repos/{owner}/{repo}/branches/{branch}",
    owner  = owner,
    repo   = repo,
    branch = branch,
    .token = creds$password
  )

  # Append additional information extracted from the URL
  append(mt,
         url_func(mt$protection_url))
}

