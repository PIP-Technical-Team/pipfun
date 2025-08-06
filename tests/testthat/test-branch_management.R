# ----------------------------------------- #
# Preliminary operations ####
# ----------------------------------------- #
library(withr)
owner <- getOption("pipfun.ghowner")
repo <- "aux_test"
creds <- get_github_creds()
identity <- getOption("pipfun.identities")[1]
root_dir <- tempdir()

# Polling helpers for GitHub API propagation
wait_for_branch <- function(branch, repo, owner, timeout = 10, interval = 1) {
  for (i in seq_len(timeout)) {
    branches_info <- gh::gh(
      "GET /repos/:owner/:repo/branches",
      owner = owner,
      repo = repo,
      .limit = Inf
    )
    branch_names <- sapply(branches_info, function(b) b$name)
    if (branch %in% branch_names) return(TRUE)
    Sys.sleep(interval)
  }
  FALSE
}

wait_for_content_match <- function(repo, branch1, branch2, owner, timeout = 10, interval = 1) {
  for (i in seq_len(timeout)) {
    result <- compare_branch_content(
      repo = repo,
      branch1 = branch1,
      branch2 = branch2,
      owner = owner
    )
    if (isTRUE(result$same_content)) return(TRUE)
    Sys.sleep(interval)
  }
  FALSE
}

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
   "GET /repos/:owner/:repo/branches",
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

# Helper: create a branch and wait for propagation
create_test_branch <- function(name, from = "main", env = parent.frame()) {
  # Log available branches and ref_branch for debugging
  branch_available <- tryCatch({
    gh::gh(
      "GET /repos/:owner/:repo/branches",
      owner = owner,
      repo = repo,
      .limit = Inf
    )
  }, error = function(e) NULL)
  branch_names <- if (!is.null(branch_available)) sapply(branch_available, function(b) b$name) else NA
  message(sprintf("Attempting to create branch '%s' from ref_branch '%s'. Available branches: %s", name, from, paste(branch_names, collapse = ", ")))

  res <- tryCatch({
    create_new_branch(
      repo = repo,
      new_branch = name,
      ref_branch = from
    )
    TRUE
  }, error = function(e) {
    message(sprintf("Failed to create branch %s: %s", name, e$message))
    FALSE
  })
  if (!res) {
    stop(sprintf("Branch creation failed for %s (ref_branch: %s). Available branches: %s", name, from, paste(branch_names, collapse = ", ")))
  }
  Sys.sleep(2) # Wait for GitHub API propagation
  withr::defer(
    delete_branch(
      repo = repo,
      branch_to_delete = name,
      ask = FALSE
    ),
    envir = env
  )
  invisible(res)
}

create_test_branch(paste0(base_date, "_TEST"))
create_test_branch(paste0(base_date, "_v2"))
create_test_branch(paste0(base_date, "_force_true"))
create_test_branch(paste0(base_date, "_force_cancel"))
create_test_branch(paste0(base_date, "_force_false"))

# ______________________________ #
# Essential Tests ####
# ______________________________ #

test_that("branch creation and detection works", {
  new_branch <- paste0(base_date, "_release")
  res <- create_test_branch(new_branch)
  # Fail fast if branch creation failed
  if (!res) stop(sprintf("Test branch creation failed for %s", new_branch))
  expect_true(wait_for_branch(new_branch, repo, owner, timeout = 10, interval = 1))

  branches_test <- get_repo_branches(
    owner = owner,
    repo = repo
  )
  expect_true(new_branch %in% branches_test$all_branches)
  expect_true(branches_test$has_release_branch)
  expect_contains(branches_test$release_branches, new_branch)
})

test_that("branch content comparison works", {
  branch1 <- paste0("test_same1_", base_date)
  branch2 <- paste0("test_same2_", base_date)
  res1 <- create_test_branch(branch1)
  if (!res1) stop(sprintf("Test branch creation failed for %s", branch1))
  res2 <- create_test_branch(branch2)
  if (!res2) stop(sprintf("Test branch creation failed for %s", branch2))
  expect_true(wait_for_branch(branch1, repo, owner))
  expect_true(wait_for_branch(branch2, repo, owner))

  res_same <- compare_branch_content(
    repo = repo,
    branch1 = branch1,
    branch2 = branch2
  )
  expect_true(res_same$same_content)

  diff_branch <- paste0("test_diff_", base_date)
  res3 <- create_test_branch(diff_branch, from = "DEV")
  if (!res3) stop(sprintf("Test branch creation failed for %s", diff_branch))
  expect_true(wait_for_branch(diff_branch, repo, owner))

  res_diff <- compare_branch_content(
    repo = repo,
    branch1 = branch1,
    branch2 = diff_branch
  )
  expect_false(res_diff$same_content)
})

test_that("branch deletion works", {
  branch_name <- paste0("to_delete_", base_date)
  res <- create_test_branch(branch_name)
  if (!res) stop(sprintf("Test branch creation failed for %s", branch_name))
  expect_true(wait_for_branch(branch_name, repo, owner))

  delete_branch(
    branch_to_delete = branch_name,
    repo             = repo,
    owner            = owner,
    ask              = FALSE
  )

  branches <- gh::gh(
     "GET /repos/:owner/:repo/branches",
    owner = owner,
    repo = repo
  )
  branch_names <- sapply(
    branches,
    function(branch) branch$name
  )
  expect_false(branch_name %in% branch_names)
})

