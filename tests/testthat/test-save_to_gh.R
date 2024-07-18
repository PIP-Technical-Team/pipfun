test_that("save_to_gh works correctly", {
  Sys.setenv(GITHUB_PAT = 'code')
  testthat::local_mocked_bindings(gh = function(...) NULL)
  expect_message(save_to_gh(iris, "test"), "File test.csv saved to DEV branch of aux_test in GitHub successfully")
  expect_null(save_to_gh(iris, "test"))
})
