# ----------------------------------------- #
# Preliminary operations ####
# ----------------------------------------- #
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

test_that("get repo branches works as expected", {
  new_branch <- paste0(base_date, "_release")

  create_test_branch(new_branch)

  branches_api <- gh::gh(
    "GET /repos/{owner}/{repo}/branches",
    owner = owner,
    repo = repo,
    .limit = Inf
  )

  branch_names <- sapply(
    branches_api,
    function(branch) branch$name
  )

  branches_test <- get_repo_branches(
    owner = owner,
    repo = repo
  )

  expect_equal(
    branch_names,
    branches_test$all_branches
  )

  expect_equal(
    branches_test$has_release_branch,
    TRUE
  )

  expect_contains(
    branches_test$release_branches,
    new_branch
  )
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

test_that("confirm branch exists works as expected", {

  # Create a temporary branch
  temp_branch <- paste0("test_branch_exists_", base_date)
  create_test_branch(temp_branch)

  # Get all branch names
  branches_info <- gh::gh(
    "GET /repos/{owner}/{repo}/branches",
    owner = owner,
    repo = repo,
    .limit = Inf
  )

  branch_names <- sapply(
    branches_info,
    function(branch) branch$name
  )

  expect_true(
    temp_branch %in% branch_names
  )

  expect_equal(
    confirm_branch_exists(
      repo = repo,
      branch = temp_branch
    ),
    TRUE
  )

  expect_equal(
    confirm_branch_exists(
      repo = repo,
      branch = "non_existent_branch"
    ),
    FALSE
  )

  expect_error(
    confirm_branch_exists(
      repo = repo,
      branch = 2
    )
  )
})


test_that("update branches work as expected", {
  # Branches already in sync
  expect_equal(
    update_branches(
      repo = repo,
      branch1 = "main",
      branch2 = "test_main"
    ),
    TRUE
  )

  # Set up a fresh branch from "main" and one from "DEV_v2"
  branch_from_main <- paste0("test_update_main_", base_date)
  branch_from_dev <- paste0("test_update_dev_", base_date)

  create_test_branch(branch_from_main, from = "main")
  create_test_branch(branch_from_dev, from = "DEV_v2")

  # Apply update from main into dev-based branch
  update_branches(
    repo = repo,
    branch1 = branch_from_main,
    branch2 = branch_from_dev
  )

  # Check content match
  result <- compare_branch_content(
    repo = repo,
    branch1 = branch_from_main,
    branch2 = branch_from_dev
  )

  expect_equal(
    result$same_content,
    TRUE
  )
})



test_that("merge_branch_into works correctly", {

  # 1. Merge when branches have the same content
  expect_no_error(
    merge_branch_into(
      repo = repo,
      source_branch = "main",
      target_branch = paste0(base_date, "_TEST")
    )
  )

  # 2. Merge when branches have different content
  expect_no_error(
    merge_branch_into(
      repo = repo,
      source_branch = "DEV",
      target_branch = paste0(base_date, "_v2")
    )
  )

  # 3. Error if source branch doesn't exist
  expect_error(
    merge_branch_into(
      repo = repo,
      source_branch = "invalid_source",
      target_branch = paste0(base_date, "_v2")
    )
  )

  # 4. Merge with force = TRUE
  expect_no_error(
    merge_branch_into(
      repo = repo,
      source_branch = "DEV",
      target_branch = paste0(base_date, "_force_true"),
      force = TRUE
    )
  )

  expect_no_error(
    merge_branch_into(
      repo = repo,
      source_branch = paste0(base_date, "_force_true"),
      target_branch = "DEV",
      force = TRUE
    )
  )

  # 5. Merge with force = FALSE and confirmation "Yes"
  assign("askYesNo", function(...) TRUE, envir = .GlobalEnv)

  expect_no_error(
    merge_branch_into(
      repo = repo,
      source_branch = paste0(base_date, "_force_false"),
      target_branch = "DEV",
      force = FALSE
    )
  )

  # Cleanup: remove monkey-patched askYesNo
  rm(askYesNo, envir = .GlobalEnv)
})



test_that("delete_branch works", {

  branch_name <- "to_delete"

  # Create the branch and defer cleanup only if the test exits early
  create_new_branch(
    new_branch = branch_name,
    repo       = repo,
    ref_branch = "DEV"  # or "main", depending on desired origin
  )

  # Delete the branch
  delete_branch(
    branch_to_delete = branch_name,
    repo             = repo,
    owner            = owner,
    ask              = FALSE
  )

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

