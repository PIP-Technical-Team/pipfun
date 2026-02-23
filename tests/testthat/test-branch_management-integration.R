library(testthat)

skip_on_cran()
skip_if_offline()
skip_if_not(Sys.getenv("RUN_GH_TESTS") == "true")

test_that("integration: branch lifecycle works", {

  owner <- getOption("pipfun.ghowner")
  repo <- "aux_test"
  identity <- getOption("pipfun.identities")[1]
  root_dir <- tempdir()

  base_date <- format(Sys.Date(), "%Y%m%d")
  release <- base_date
  branch_name <- paste0(base_date, "_integration_test")

  # Create release
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

  # Create branch
  create_new_branch(
    repo = repo,
    new_branch = branch_name,
    ref_branch = "main"
  )

  expect_true(
    confirm_branch_exists(branch_name, measure = "test")
  )

  # Delete branch
  delete_branch(
    branch_to_delete = branch_name,
    repo = repo,
    owner = owner,
    ask = FALSE
  )

  expect_false(
    confirm_branch_exists(branch_name, measure = "test")
  )
})