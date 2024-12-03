# Save vars

owner <- getOption("pipfun.ghowner")
measure <- "test"
repo <- paste0("aux_", measure)

# create test branch
create_new_branch(measure    = "test",
                  ref_branch = "main",
                  new_branch = "test_main")


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

# delete test branch
# TODO
