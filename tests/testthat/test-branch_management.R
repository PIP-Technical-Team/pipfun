
# ----------------------------------------- #
# Preliminary operations ####
# ----------------------------------------- #
library(withr)
owner <- getOption("pipfun.ghowner")
repo <- "aux_test"
creds <- get_github_creds()
identity <- getOption("pipfun.identities")[1]
root_dir <- tempdir()

# Create known branches once
to_keep <- c("DEV", "DEV_v2", "main", "PROD", "test_main", "20241121")

existing_branches <- gh::gh("GET /repos/{owner}/{repo}/branches",
                            owner = owner,
                            repo = repo,
                            .limit = Inf)

branch_names <- sapply(existing_branches,
                       function(branch) branch$name)

# Clean up unknown branches from previous runs
to_delete <- setdiff(branch_names,
                     to_keep)

invisible(lapply(to_delete,
                 function(b) {
  delete_branch(repo = repo,
                branch_to_delete = b, ask = FALSE)
}))

# Create and setup new release for testing
release  = base_date = format(Sys.Date(), "%Y%m%d")

new_pip_release(release = release,
                identity = identity,
                root_dir = root_dir)

setup_working_release(release = release,
                      identity = identity,
                      root_dir = root_dir)

# Create some branches for testing

create_new_branch(repo = repo,
                  new_branch = paste0(base_date, "_TEST"),
                  ref_branch = "main")
create_new_branch(repo = repo,
                  new_branch = paste0(base_date, "_v2"),
                  ref_branch = "main")
create_new_branch(repo = repo,
                  new_branch = paste0(base_date, "_force_true"),
                  ref_branch = "main")
create_new_branch(repo = repo,
                  new_branch = paste0(base_date, "_force_cancel"),
                  ref_branch = "main")
create_new_branch(repo = repo,
                  new_branch = paste0(base_date, "_force_false"),
                  ref_branch = "main")
create_new_branch(repo = repo,
                  new_branch = "to_delete",
                  ref_branch = "DEV")

# ______________________________ #
# Tests ####
# ______________________________ #

test_that("get repo branches works as expected", {

  # create a release branch ###
  new_branch <- paste0(base_date,
                       "_release")

  create_new_branch(owner = owner,
                    repo = repo,
                    new_branch = new_branch,
                    ref_branch = "main")

  withr::defer(delete_branch(repo = repo,
                             branch_to_delete = new_branch,
                             ask = FALSE))

  branches_api <- gh::gh("GET /repos/{owner}/{repo}/branches",
                         owner = owner,
                         repo = repo,
                         .limit = Inf)

  branch_names <- sapply(branches_api,
                         function(branch) branch$name)

  branches_test <- get_repo_branches(owner = owner,
                                     repo = repo)

  expect_equal(branch_names,
               branches_test$all_branches)

  # check it finds release branch

  expect_equal(get_repo_branches(owner = owner,
                                 repo = repo)$has_release_branch, TRUE)

  expect_contains(get_repo_branches(owner = owner,
                                    repo = repo)$release_branches,
                  new_branch)
})

# Test compare branches sha
test_that("compare branches sha works as expected", {

  # Arguments
  compare_branches_sha(owner = owner,
                       measure = "test",
                       branch1 = "DEV",
                       branch2 = "main") |>
    expect_no_error()

  # error if incorrect repo
  compare_branches_sha(owner = owner,
                       repo = "ahguenc",
                       branch1 = "main",
                       branch2 = "DEV") |>
    expect_error()



  # Class of output
  res <- compare_branches_sha(owner = owner,
                              measure = "test",
                              branch1 = "DEV",
                              branch2 = "main")
  class(res) |>
    expect_equal("list")

  # FALSE
  out <- compare_branches_sha(owner = owner,
                              repo = repo,
                              branch1 = "main",
                              branch2 = "DEV")

  # different shas
  sha_1 <- gh::gh(
    "GET /repos/{owner}/{repo}/branches/{branch}",
    owner  = owner,
    repo   = repo,
    branch = "main",
    .token = creds$password
  )$commit$sha

  sha_2 <- gh::gh(
    "GET /repos/{owner}/{repo}/branches/{branch}",
    owner  = owner,
    repo   = repo,
    branch = "DEV",
    .token = creds$password
  )$commit$sha

  same_sha <- sha_1 == sha_2

  out$updated |>
    expect_equal(FALSE)

  out$updated |>
    expect_equal(same_sha)


  # TRUE
  out <- compare_branches_sha(owner = owner,
                              repo = repo,
                              branch1 = "main",
                              branch2 = "test_main")

  sha_1 <- gh::gh(
    "GET /repos/{owner}/{repo}/branches/{branch}",
    owner  = owner,
    repo   = repo,
    branch = "main",
    .token = creds$password
  )$commit$sha

  sha_2 <- gh::gh(
    "GET /repos/{owner}/{repo}/branches/{branch}",
    owner  = owner,
    repo   = repo,
    branch = "test_main",
    .token = creds$password
  )$commit$sha

  same_sha <- sha_1 == sha_2

  out$updated |>
    expect_equal(same_sha)

  out$updated |>
    expect_equal(TRUE)


  # ERROR
  compare_branches_sha(owner   = owner,
                      repo    = repo,
                      branch1 = "ahhdshd",
                      branch2 = "DEV")$updated |>
    expect_error()

})

# Test compare branches content

test_that("compare branches content works as expected", {

  # Same content
  res <- compare_branch_content(repo = repo,
                                branch1 = "main",
                                branch2 = "PROD")

  res$tree_sha_1 |>
    expect_equal(res$tree_sha_2)


  res$same_content |>
    expect_equal(TRUE)

  res <- compare_branch_content(repo = repo,
                                branch1 = "main",
                                branch2 = "test_main"
  )

  commit_1 <- gh::gh(
    "GET /repos/{owner}/{repo}/branches/{branch}",
    owner  = owner,
    repo   = repo,
    branch = "main",
    .token = creds$password
  )$commit

  tree_sha_1 <- commit_1$commit$tree$sha

  commit_2 <- gh::gh(
    "GET /repos/{owner}/{repo}/branches/{branch}",
    owner  = owner,
    repo   = repo,
    branch = "test_main",
    .token = creds$password
  )$commit

  tree_sha_2 <- commit_2$commit$tree$sha

  same_tree_sha <- tree_sha_1 == tree_sha_2

  res$tree_sha_1 |>
    expect_equal(res$tree_sha_2)

  res$same_content |>
    expect_equal(same_tree_sha)


  res$same_content |>
    expect_equal(TRUE)

  # Output class
  class(res) |>
    expect_equal("list")

  class(res$tree_sha_1) |>
    expect_equal(class(res$tree_sha_2))

  class(res$tree_sha_2) |>
    expect_equal("character")

  # Different content
  res <- compare_branch_content(repo = "aux_test",
                                branch1 = "main",
                                branch2 = "DEV_v2")

  commit_1 <- gh::gh(
    "GET /repos/{owner}/{repo}/branches/{branch}",
    owner  = owner,
    repo   = repo,
    branch = "main",
    .token = creds$password
  )$commit

  tree_sha_1 <- commit_1$commit$tree$sha

  commit_2 <- gh::gh(
    "GET /repos/{owner}/{repo}/branches/{branch}",
    owner  = owner,
    repo   = repo,
    branch = "DEV_v2",
    .token = creds$password
  )$commit

  tree_sha_2 <- commit_2$commit$tree$sha

  same_tree_sha <- tree_sha_1 == tree_sha_2

  (res$tree_sha_1 == res$tree_sha_2) |>
    expect_equal(FALSE)

  res$same_content |>
    expect_equal(FALSE)

  res$same_content |>
    expect_equal(same_tree_sha)

  # Error
  compare_branch_content(repo = "test") |>
    expect_error()

})

# Test confirm branch exists
test_that("confirm branch exists work as expected", {

  # Fetch all branches with automatic pagination
  branches_info <- gh::gh(
    "GET /repos/:owner/:repo/branches",
    owner = owner,
    repo = repo,
    .limit = Inf  # Automatically fetch all pages
  )

  # Extract branch names
  branch_names <- sapply(branches_info, function(branch) branch$name)
  print(branch_names)


  br_exists <- ("main" %in% branch_names)

  confirm_branch_exists(repo = "aux_test",
                          branch = "main") |>
      expect_equal(br_exists)

  # Error -incorrect input

  confirm_branch_exists(repo   = hfgv,
                        branch = "DEV") |>
    expect_error()

  confirm_branch_exists(repo   = "aux_test",
                        branch = 2) |>
    expect_error()


})


# Test update branches
test_that("update branches work as expected", {

  # Already updated
  update_branches(repo = "aux_test",
                  branch1 = "main",
                  branch2 = "test_main"
                  ) |>
    expect_equal(TRUE)

  # Update and check they have same content
  update_branches(repo = "aux_test",
                  branch1 = "DEV_v2",
                  branch2 = "20241121"
                  )

  # check same content
  compare_branch_content(repo = "aux_test",
                         branch1 = "DEV_v2",
                         branch2 = "20241121"
                         )$same_content |>
    expect_equal(TRUE)

})


# Test merge branches
test_that("merge branch into works correctly", {

  # When branches have same content
  merge_branch_into(repo = "aux_test",
                    source_branch = "main",
                    target_branch = paste0(format(Sys.Date(), "%Y%m%d"), "_TEST"))|>
    expect_no_error()


  # When branches have different content
  # successful merge
  merge_branch_into(repo = "aux_test",
                    source_branch = "DEV",
                    target_branch = paste0(format(Sys.Date(), "%Y%m%d"), "_v2"))|>
    expect_no_error()

  # compare_branch_content(repo = "aux_test",
  #                        branch1 = "DEV",
  #                        branch2 = paste0(format(Sys.Date(), "%Y%m%d"), "_v2"))$same_content |>
  #   expect_equal(TRUE)

  # error when branches do not exist
  merge_branch_into(repo = "aux_test",
                    source_branch = "hvhtfj",
                    target_branch = paste0(format(Sys.Date(), "%Y%m%d"), "_v2"))|>
    expect_error()

  # Testing the force option

  # Case: force = TRUE (default)
  merge_branch_into(repo = "aux_test",
                    source_branch = "DEV",
                    target_branch = paste0(format(Sys.Date(), "%Y%m%d"), "_force_true"),
                    force = TRUE) |>
    expect_no_error()

  # Confirm the merge was successful
  merge_branch_into(repo = "aux_test",
                    target_branch = "DEV",
                    source_branch = paste0(format(Sys.Date(), "%Y%m%d"), "_force_true"),
                    force = TRUE)

  # Case: force = FALSE with user confirmation (simulating "Yes")
  assign("askYesNo", function(...) TRUE, envir = .GlobalEnv)

  merge_branch_into(repo = "aux_test",
                    target_branch = "DEV",
                    source_branch = paste0(format(Sys.Date(), "%Y%m%d"), "_force_false"),
                    force = FALSE) |>
    expect_no_error()

  # Confirm the merge was successful
  merge_branch_into(repo = "aux_test",
                    target_branch = "DEV",
                    source_branch = paste0(format(Sys.Date(), "%Y%m%d"), "_force_false"),
                    force = FALSE)


  # Remove the custom `askYesNo` function after the test
  rm(askYesNo, envir = .GlobalEnv)

  # Case: force = FALSE with user canceling (simulating "No")
  assign("askYesNo", function(...) FALSE, envir = .GlobalEnv)


    # merge_branch_into(repo = "aux_test",
    #                   target_branch = "DEV",
    #                   source_branch = paste0(format(Sys.Date(), "%Y%m%d"), "_force_cancel"),
    #                   force = FALSE) |>
    #   expect_error()
    #

  # Clean up the environment by removing the custom `askYesNo`
  rm(askYesNo, envir = .GlobalEnv)


})



# Test delete branches function
test_that("delete branch works", {

  # delete branch
  delete_branch(branch_to_delete = "to_delete",
                repo = repo,
                owner = owner,
                ask = FALSE)

  # confirm it was deleted
  branches <- gh::gh("GET /repos/{owner}/{repo}/branches",
                     owner = owner,
                     repo = repo)

  branch_names <- sapply(branches,
                         function(branch) branch$name)

  ("to_delete" %in% branch_names) |>
    expect_equal(FALSE)

})

# ----------------------------------------- #
# Cleaning ####
# ----------------------------------------- #

# Delete branches used for testing
delete_branch(repo = "aux_test",
              branch_to_delete = paste0(format(Sys.Date(), "%Y%m%d"), "_force_true"))

delete_branch(repo = "aux_test",
              branch_to_delete = paste0(format(Sys.Date(), "%Y%m%d"), "_force_false"))

delete_branch(repo = "aux_test",
              branch_to_delete = paste0(format(Sys.Date(), "%Y%m%d"), "_force_cancel"))

