# Save vars

owner <- getOption("pipfun.ghowner")
measure <- "test"
repo <- paste0("aux_", measure)



test_that("compare branches sha works as expected", {

  # FALSE
  out <- compare_branches_sha(owner = owner,
                              repo = repo,
                              branch1 = "main",
                              branch2 = "DEV")

  out$updated |>
    expect_equal(FALSE)


  # TRUE



  # ERROR


})
