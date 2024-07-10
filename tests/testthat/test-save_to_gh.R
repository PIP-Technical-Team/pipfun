test_that("save_to_gh works correctly", {
  Sys.setenv(GITHUB_PAT = 'code')
  mockery::stub(save_to_gh, "gh::gh", NULL)
  expect_message(save_to_gh(iris, "test"), "File test.csv saved to DEV branch of aux_test in GitHub successfully")
})
