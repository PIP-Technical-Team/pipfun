# Save vars

owner <- getOption("pipfun.ghowner")
measure <- "test"
repo <- paste0("aux_", measure)

# create test branch
create_new_branch(measure    = "test",
                  ref_branch = "main",
                  new_branch = "test_main")

# Test compare branches sha
test_that("compare branches sha works as expected", {

  # Arguments
  compare_branches_sha(owner = owner,
                       measure = "test",
                       branch1 = "DEV",
                       branch2 = "main") |>
    expect_no_error()

  # error if both measure and repo name are provided
  # compare_branches_sha(owner = owner,
  #                      measure = "test",
  #                      repo = repo,
  #                      branch1 = "DEV",
  #                      branch2 = "main") |>
  #   expect_error()

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

  out$updated |>
    expect_equal(FALSE)


  # TRUE
  out <- compare_branches_sha(owner = owner,
                              repo = repo,
                              branch1 = "main",
                              branch2 = "test_main")
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

  res$tree_sha_1 |>
    expect_equal(res$tree_sha_2)


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

  (res$tree_sha_1 == res$tree_sha_2) |>
    expect_equal(FALSE)

  res$same_content |>
    expect_equal(FALSE)

  # Error
  compare_branch_content(repo = "test") |>
    expect_error()

})

# Test confirm branch exists
test_that("confirm branch exists work as expected", {

  confirm_branch_exists(repo = "aux_test",
                        branch = "DEV") |>
    expect_equal(TRUE)

  confirm_branch_exists(repo = "aux_test",
                        branch = "dchju") |>
    expect_equal(FALSE)

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

# Create some branches in aux_test repo for for testing purposes
create_new_branch(repo = "aux_test",
                  new_branch = paste0(format(Sys.Date(), "%Y%m%d"), "_TEST"),
                  ref_branch = "main",
                  identity = "TEST"
)

create_new_branch(repo = "aux_test",
                  new_branch = paste0(format(Sys.Date(), "%Y%m%d"), "_v2"),
                  ref_branch = "main",
                  identity = "TEST"
)

# Test merge branches
test_that("merge branch into works correctly", {

  # When branches have same content
  merge_branch_into(repo = "aux_test",
                    source_branch = "main",
                    target_branch = paste0(format(Sys.Date(), "%Y%m%d"), "_TEST"))|>
    expect_no_error()

  compare_branch_content(repo = "aux_test",
                         branch1 = "main",
                         branch2 = paste0(format(Sys.Date(), "%Y%m%d"), "_TEST"))$same_content |>
    expect_equal(TRUE)



  # When branches have different content
  # successful merge
  merge_branch_into(repo = "aux_test",
                    source_branch = "DEV",
                    target_branch = paste0(format(Sys.Date(), "%Y%m%d"), "_v2"))|>
    expect_no_error()

  compare_branch_content(repo = "aux_test",
                         branch1 = "DEV",
                         branch2 = paste0(format(Sys.Date(), "%Y%m%d"), "_v2"))$same_content |>
    expect_equal(TRUE)

})



# delete test branch
# TODO


