library(withr)
owner <- getOption("pipfun.ghowner")
repo <- "aux_test"
creds <- get_github_creds()
identity <- getOption("pipfun.identities")[1]
root_dir <- tempdir()

# Keep only known safe branches
base_date <- format(Sys.Date(), "%Y%m%d")
to_keep <- c("DEV", "DEV_v2", "main", "PROD", "test_main", base_date)

# Create and setup new release for testing
release <- base_date

new_pip_release(
  release = release,
  identity = identity,
  root_dir = root_dir
)

setup_working_release(
  release = release,
  identity = identity,
  root_dir = root_dir
)

existing_branches <- gh::gh(
  "GET /repos/{owner}/{repo}/branches",
  owner = owner,
  repo = repo,
  .limit = Inf
)

branch_names <- sapply(
  existing_branches,
  function(branch) branch$name
)

# Clean up unknown branches from previous runs
branches_to_delete <- setdiff(
  branch_names,
  to_keep
)

invisible(lapply(
  branches_to_delete,
  function(b) {
    delete_branch(
      repo = repo,
      branch_to_delete = b,
      ask = FALSE
    )
  }
))

# Set up test branches
create_test_branch <- function(name, from = "main", env = parent.frame()) {
  create_new_branch(
    repo = repo,
    new_branch = name,
    ref_branch = from
  )
  withr::defer(
    delete_branch(
      repo = repo,
      branch_to_delete = name,
      ask = FALSE
    ),
    envir = env
  )
}

create_test_branch(paste0(base_date, "_TEST"))
create_test_branch(paste0(base_date, "_v2"))
create_test_branch(paste0(base_date, "_force_true"))
create_test_branch(paste0(base_date, "_force_cancel"))
create_test_branch(paste0(base_date, "_force_false"))

# ______________________________ #
# Tests ####
# ______________________________ #

test_that("returns empty results when no branches", {

  mockery::stub(get_repo_branches, "gh::gh", list())

  res <- get_repo_branches(owner = "foo", repo = "bar")
  expect_equal(res$all_branches, list())
  expect_equal(res$release_branches, character(0))
  expect_false(res$has_release_branch)
})

test_that("handles branches without release pattern", {
  mockery::stub(get_repo_branches, "gh::gh", \(...) list(list(name = "main"), list(name = "dev")))

  res <- get_repo_branches(owner = "foo", repo = "bar")
  expect_equal(res$all_branches, c("main", "dev"))
  expect_equal(res$release_branches, character(0))
  expect_false(res$has_release_branch)
})

test_that("detects release pattern branches", {
  mockery::stub(get_repo_branches, "gh::gh", \(...) list(list(name = "20250101"), list(name = "feature-x")))
  res <- get_repo_branches(owner = "foo", repo = "bar")

  expect_equal(res$all_branches, c("20250101", "feature-x"))
  expect_equal(res$release_branches, "20250101")
  expect_true(res$has_release_branch)
})

test_that("handles multiple release branches", {
  mockery::stub(get_repo_branches, "gh::gh", \(...) list(
    list(name = "20230101"),
    list(name = "20241231"),
    list(name = "main")
  ))

  res <- get_repo_branches(owner = "foo", repo = "bar")
  expect_equal(res$release_branches, c("20230101", "20241231"))
  expect_true(res$has_release_branch)
})


test_that("compare branches sha works as expected", {
  expect_no_error(
    compare_branches_sha(
      owner = owner,
      measure = "test",
      branch1 = "DEV",
      branch2 = "main"
    )
  )

  expect_error(
    compare_branches_sha(
      owner = owner,
      repo = "ahguenc",
      branch1 = "main",
      branch2 = "DEV"
    )
  )

  res <- compare_branches_sha(
    owner = owner,
    measure = "test",
    branch1 = "DEV",
    branch2 = "main"
  )

  expect_equal(
    class(res),
    "list"
  )

  sha_1 <- gh::gh(
    "GET /repos/{owner}/{repo}/branches/{branch}",
    owner = owner,
    repo = repo,
    branch = "DEV",
    .token = creds$password
  )$commit$sha

  sha_2 <- gh::gh(
    "GET /repos/{owner}/{repo}/branches/{branch}",
    owner = owner,
    repo = repo,
    branch = "main",
    .token = creds$password
  )$commit$sha

  expect_equal(
    res$updated,
    sha_1 == sha_2
  )

  out <- compare_branches_sha(
    owner = owner,
    repo = repo,
    branch1 = "main",
    branch2 = "test_main"
  )

  sha_3 <- gh::gh(
    "GET /repos/{owner}/{repo}/branches/{branch}",
    owner = owner,
    repo = repo,
    branch = "test_main",
    .token = creds$password
  )$commit$sha

  expect_equal(
    out$updated,
    sha_1 != sha_3
  )

  expect_equal(
    out$updated,
    TRUE
  )

  expect_error(
    compare_branches_sha(
      owner = owner,
      repo = repo,
      branch1 = "invalid_name",
      branch2 = "DEV"
    )$updated
  )
})

test_that("compare branches content works as expected", {
  branch1 <- paste0("test_same1_", base_date)
  branch2 <- paste0("test_same2_", base_date)

  create_test_branch(branch1)
  create_test_branch(branch2)
  # Adding sleep condition to give some time for branches to reflect
  Sys.sleep(3)
  res_same <- compare_branch_content(
    repo = repo,
    branch1 = branch1,
    branch2 = branch2
  )

  expect_true(
    res_same$same_content
  )

  diff_branch <- paste0("test_diff_", base_date)

  create_test_branch(diff_branch, from = "DEV")
  # Adding sleep condition to give some time for branches to reflect
  Sys.sleep(3)

  res_diff <- compare_branch_content(
    repo = repo,
    branch1 = branch1,
    branch2 = diff_branch
  )

  expect_false(
    res_diff$same_content
  )

  expect_error(
    compare_branch_content(
      repo = "test"
    )
  )
})

# Confirm branch exists ##

test_that("returns TRUE when branch exists", {
  mockery::stub(
    confirm_branch_exists,
    'gh::gh',
    function(...) list(name = "main")
  )
  res <- confirm_branch_exists(branch = "main", measure = "test")
  expect_true(res)
})

test_that("returns FALSE when branch does not exist (404)", {
  mockery::stub(
    confirm_branch_exists,
    'gh::gh',
    function(...) stop("404 Not Found")
  )
  res <- confirm_branch_exists(branch = "nonexistent", measure = "test")
  expect_false(res)
})

test_that("propagates non-404 errors", {
  mockery::stub(
    confirm_branch_exists,
    'gh::gh',
    function(...) stop("500 Internal Server Error")
  )
  expect_error(
    confirm_branch_exists(branch = "main", measure = "test"),
    "500"
  )
})

test_that("defensive checks work for wrong argument types", {
  expect_error(confirm_branch_exists(branch = 123, measure = "test"), "is.character")
  expect_error(confirm_branch_exists(branch = c("a", "b"), measure = "test"), "length")
  expect_error(confirm_branch_exists(branch = "main", measure = "test", owner = 1), "is.character")
  expect_error(confirm_branch_exists(branch = "main", measure = "test", repo = 2), "is.character")
})

test_that("returns TRUE when branches already have same content", {
  # mock compare_branches_sha
  mockery::stub(update_branches, "compare_branches_sha", function(...) list(sha_1 = "abc123"))
  # mock compare_branch_content
  mockery::stub(update_branches, "compare_branch_content", function(...) list(same_content = TRUE))
  # mock gh::gh (should not be called)
  gh_mock <- mockery::mock()
  mockery::stub(update_branches, "gh::gh", gh_mock)

  res <- update_branches(owner = "me", repo = "repo", branch1 = "main", branch2 = "dev")
  expect_true(res)
  expect_equal(mock_args(gh_mock), list()) # never called
})

test_that("updates branch when content differs and force=TRUE", {
  mockery::stub(update_branches, "compare_branches_sha", function(...) list(sha_1 = "abc123"))
  mockery::stub(update_branches, "compare_branch_content", function(...) list(same_content = FALSE))
  gh_mock <- mock(TRUE)
  mockery::stub(update_branches, "gh::gh", gh_mock)

  res <- update_branches(owner = "me", repo = "repo", branch1 = "main", branch2 = "dev", force = TRUE)
  expect_true(res)
  expect_called(gh_mock, 1)
})

test_that("aborts when force=FALSE and user answers No", {
  mockery::stub(update_branches, "compare_branches_sha", function(...) list(sha_1 = "abc123"))
  mockery::stub(update_branches, "compare_branch_content", function(...) list(same_content = FALSE))
  mockery::stub(update_branches, "utils::askYesNo", function(...) FALSE)

  expect_error(
    update_branches(owner = "me", repo = "repo", branch1 = "main", branch2 = "dev", force = FALSE),
    "Update interrupted"
  )
})

test_that("returns FALSE if gh::gh() fails", {
  mockery::stub(update_branches, "compare_branches_sha", function(...) list(sha_1 = "abc123"))
  mockery::stub(update_branches, "compare_branch_content", function(...) list(same_content = FALSE))
  mockery::stub(update_branches, "gh::gh", function(...) stop("Some API error"))

  res <- update_branches(owner = "me", repo = "repo", branch1 = "main", branch2 = "dev", force = TRUE)
  expect_false(res)
})

test_that("returns TRUE when branches already have same content", {
  stub(merge_branch_into, "compare_branch_content", function(...) list(same_content = TRUE))
  gh_mock <- mock()
  stub(merge_branch_into, "gh::gh", gh_mock)

  res <- merge_branch_into(owner = "me", repo = "repo", source_branch = "main", target_branch = "dev")
  expect_true(res)
  expect_equal(mock_args(gh_mock), list()) # gh::gh never called
})

test_that("merges when content differs and force=TRUE", {
  stub(merge_branch_into, "compare_branch_content", function(...) list(same_content = FALSE))
  gh_mock <- mock(TRUE)
  stub(merge_branch_into, "gh::gh", gh_mock)

  res <- merge_branch_into(owner = "me", repo = "repo", source_branch = "main", target_branch = "dev", force = TRUE)
  expect_true(res)
  expect_called(gh_mock, 1)
})

test_that("aborts when force=FALSE and user answers No", {
  stub(merge_branch_into, "compare_branch_content", function(...) list(same_content = FALSE))
  stub(merge_branch_into, "utils::askYesNo", function(...) FALSE)

  expect_error(
    merge_branch_into(owner = "me", repo = "repo", source_branch = "main", target_branch = "dev", force = FALSE),
    "Merge interrupted"
  )
})

test_that("returns FALSE if gh::gh() fails", {
  stub(merge_branch_into, "compare_branch_content", function(...) list(same_content = FALSE))
  stub(merge_branch_into, "gh::gh", function(...) stop("Some API error"))

  res <- merge_branch_into(owner = "me", repo = "repo", source_branch = "main", target_branch = "dev", force = TRUE)
  expect_false(res)
})

test_that("delete_branch works", {

  branch_name <- "to_delete"

  # Create the branch and defer cleanup only if the test exits early
  create_new_branch(
    new_branch = branch_name,
    repo       = repo,
    ref_branch = "DEV"  # or "main", depending on desired origin
  )
  Sys.sleep(3)
  # Delete the branch
  delete_branch(
    branch_to_delete = branch_name,
    repo             = repo,
    owner            = owner,
    ask              = FALSE
  )
  Sys.sleep(5)
  # Confirm it was deleted
  branches <- gh::gh(
    "GET /repos/{owner}/{repo}/branches",
    owner = owner,
    repo  = repo
  )

  branch_names <- sapply(
    branches,
    function(branch) branch$name
  )

  expect_false(
    branch_name %in% branch_names
  )
})

