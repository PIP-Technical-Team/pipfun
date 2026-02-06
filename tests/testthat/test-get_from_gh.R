skip_if_not_installed("gitcreds")
skip_if_not_installed("gh")
skip_if_not_installed("data.table")
skip_if_not_installed("janitor")
skip_if_not_installed("httr2")

library(data.table)
library(janitor)

# Obtain credentials and ensure a usable token is present
creds <- get_github_creds()
token_pattern <- "^(gh[ps]_[a-zA-Z0-9]{36}|github_pat_[a-zA-Z0-9]{22}_[a-zA-Z0-9]{59})$"
is_token <- isTRUE(grepl(token_pattern, creds$password)) ||
            identical(creds$password, Sys.getenv("GITHUB_PAT")) ||
            nzchar(Sys.getenv("GITHUB_PAT"))

skip_if_not(is_token, "Valid GitHub token not available; skipping integration tests")

owner  <- getOption("pipfun.ghowner")
repo   <- "pip_info"
branch <- "testing"
path   <- "data"

# List contents of folder on GitHub (may error if path differs)
files <- gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
                owner = owner, repo = repo, path = path, ref = branch,
                .token = creds$password)

# collect download URLs and file names; skip if none found
file_urls <- vapply(files, function(x) x$download_url, character(1))
file_names <- vapply(files, function(x) x$name, character(1))

iris_idx <- grep("iris", tolower(file_names))
if (length(iris_idx) == 0) {
  skip("No iris files found in target GitHub path; skipping")
}
file_urls <- file_urls[iris_idx]
names(file_urls) <- tolower(fs::path_ext(file_urls))

# prepare expected small iris data for comparison (10 rows)
iris10 <- iris[1:10, ] |> as.data.table() |> janitor::clean_names()
# ensure factor columns converted to character for safe comparison
fct_cols <- names(Filter(is.factor, iris10))
if (length(fct_cols) > 0) {
  iris10[, (fct_cols) := lapply(.SD, as.character), .SDcols = fct_cols]
}

# Helper to test download_from_gh -> file exists & readable
test_download <- function(furl) {
  ext <- fs::path_ext(furl)
  tfile <- tempfile(fileext = paste0(".", ext))
  on.exit(unlink(tfile), add = TRUE)

  download_from_gh(furl, tfile, creds = creds)

  info_df <- fs::file_info(tfile)
  expect_true(info_df$size > 0, info = "Downloaded file should be non-empty")
  expect_true(fs::file_access(tfile), info = "Downloaded file should be accessible")
}

test_that("download_from_gh downloads files for each extension", {
  purrr::walk(file_urls, \(x) test_download(x))
})

test_that("download_from_gh reports error for missing branch or file", {
  bad_file <- sub("/data/", "/flu/", file_urls[1])
  ext <- fs::path_ext(bad_file)
  tfile <- tempfile(fileext = paste0(".", ext))
  on.exit(unlink(tfile), add = TRUE)

  expect_error(download_from_gh(bad_file, tfile, creds = creds))
  
  bad_branch_file <- file_urls[1] |> sub(paste0("/", branch, "/"), "/nonexistent_branch/", fixed = TRUE)
  ext2 <- fs::path_ext(bad_branch_file)
  tfile2 <- tempfile(fileext = paste0(".", ext2))
  on.exit(unlink(tfile2), add = TRUE)

  expect_error(download_from_gh(bad_branch_file, tfile2, creds = creds))
})

# Test reading files from disk via load_from_disk after download
test_read <- function(furl) {
  ext <- fs::path_ext(furl)
  tfile <- tempfile(fileext = paste0(".", ext))
  on.exit(unlink(tfile), add = TRUE)

  download_from_gh(furl, tfile, creds = creds)
  df <- load_from_disk(tfile) |> as.data.table()

  # normalize and compare to expected iris10 where appropriate
  df <- janitor::clean_names(df)
  fct_cols_df <- names(Filter(is.factor, df))
  if (length(fct_cols_df) > 0) {
    df[, (fct_cols_df) := lapply(.SD, as.character), .SDcols = fct_cols_df]
  }

  expect_equal(iris10, df, ignore_attr = TRUE)
}

test_that("files are read correctly from disk after download", {
  purrr::walk(file_urls, \(x) test_read(x))
})

test_that("get_file_info_from_gh returns metadata for a specific file", {
  expect_silent(
    get_file_info_from_gh(owner = owner, repo = repo, branch = branch, file_path = "data/iris.csv")
  )
})


