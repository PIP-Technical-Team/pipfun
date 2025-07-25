skip_if_not_installed("gitcreds")

creds <- get_github_creds()
token_pattern <- "^(gh[ps]_[a-zA-Z0-9]{36}|github_pat_[a-zA-Z0-9]{22}_[a-zA-Z0-9]{59})$"
is_token <- grepl(token_pattern, creds$password)

if (!is_token) {
  is_token <- grepl(Sys.getenv("GITHUB_PAT"), creds$password)
}

skip_if_not(is_token, "Github token not valid")
library(data.table)
library(collapse)

# init conditions -----------

## GEt -------
owner      <- getOption("pipfun.ghowner")
repo       <- "pip_info"
branch     <- "testing"
path       <- "data"  # Folder ith files


files <- gh::gh("GET /repos/:owner/:repo/contents/:path",
                owner = owner,
                repo = repo,
                path = path,
                ref = branch,
                .token = creds$password)

# Extract the names of all the files in the folder
file_names <- vapply(files, function(x) x$name, character(1))

## just get iris ------------
file_urls  <- vapply(files, function(x) x$download_url, character(1))

which_iris <- grep("iris", file_urls)
file_urls <- file_urls[which_iris]
names(file_urls) <- fs::path_ext(file_urls) |>
  tolower()


## original iris data -------------
iris10 <- iris[1:10,] |>
  setDT() |>
  janitor::clean_names()

fct_vars <- iris10 |>
  fact_vars("names")

iris10[, (fct_vars) := lapply(.SD, as.character),
       .SDcols = fct_vars]




# download_from_gh() ---------------
test_download <- function(furl) {
  ext <- fs::path_ext(furl)
  tfile <- tempfile(fileext = paste0(".",ext))

  try(download_from_gh(furl, tfile), silent = TRUE)
  info_df <- fs::file_info(tfile) |>
    setDT()


  # test characteristics of file -------
  expect_true(info_df$size > 0)
  expect_true(info_df$permissions == "rw-")
  expect_true(fs::file_access(tfile))
  unlink(tfile)
}

test_that("download each extension correctly", {

  purrr::walk(file_urls, \(x) test_download(x))

})

test_that("branch or tag not available is returned as error", {
  bad_file <- gsub("data", "flu", file_urls[1])
  ext <- fs::path_ext(bad_file)
  tfile <- tempfile(fileext = paste0(".",ext))

  download_from_gh(bad_file, tfile) |>
    expect_error()

  bad_branch <- gsub("testing", "flu", file_urls[1])
  ext <- fs::path_ext(bad_branch)
  tfile <- tempfile(fileext = paste0(".",ext))

  download_from_gh(bad_branch, tfile) |>
    expect_error()


})


# load_from_disk() ---------------

test_read <- function(furl) {
  ext <- fs::path_ext(furl)
  tfile <- tempfile(fileext = paste0(".",ext))

  download_from_gh(furl, tfile)
  df <- load_from_disk(tfile) |>
    as.data.table()

  # test characteristics of file -------
  expect_equal(iris10, df,
               ignore_attr = TRUE,
               info = paste("failed in", ext))
  unlink(tfile)
}


test_that("files are read correctly from disk", {
  purrr::walk(file_urls, \(x) test_read(x))
})

#  get_file_info_from_gh() ---------
test_that("get_file_info_from_gh extract right info", {
  info <- get_file_info_from_gh(owner,
                                     repo,
                                     branch = branch,
                                     "data/iris.csv")|>
    expect_no_error()
})


# get branch info from gh
test_that("get branch info from gh works as expected", {

  get_branch_info_from_gh(repo = "aux_test",
                          branch = "main") |>
    expect_no_error()

  branch_info <- get_branch_info_from_gh(repo = "aux_test",
                                         branch = "main")

  class(branch_info) |>
    expect_equal("list")

  get_branch_info_from_gh(repo = "cnjwe",
                          branch = "main") |>
    expect_error()

  # Check correct info

  fake_creds <- function() list(token = "fake_token")
  fake_gh <- function(..., owner, repo, branch, .token) {
    list(
      name = branch,
      commit = list(sha = "testsha123"),
      protection_url = "https://api.github.com/repos/owner/repo/branches/main/protection"
    )
  }
  fake_url <- function(url) list(protection_level = "high")

  res <- get_branch_info_from_gh(
    owner = "owner",
    repo = "repo",
    branch = "main",
    gh_func = fake_gh,
    creds_func = fake_creds,
    url_func = fake_url
  )

  expect_equal(res$name,
               "main")
  expect_equal(res$commit$sha,
               "testsha123")
  expect_equal(res$protection_level,
               "high")


})
