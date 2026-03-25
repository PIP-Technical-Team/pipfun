library(testthat)
library(mockery)

# ─────────────────────────────────────
# get_repo_branches
# ─────────────────────────────────────

test_that("returns empty results when no branches", {

  stub(get_repo_branches, "gh::gh", list())

  res <- get_repo_branches(owner = "foo", repo = "bar")

  expect_equal(res$all_branches, list())
  expect_equal(res$release_branches, character(0))
  expect_false(res$has_release_branch)
})

test_that("handles branches without release pattern", {

  stub(get_repo_branches, "gh::gh",
       \(...) list(list(name = "main"), list(name = "dev")))

  res <- get_repo_branches(owner = "foo", repo = "bar")

  expect_equal(res$all_branches, c("main", "dev"))
  expect_equal(res$release_branches, character(0))
  expect_false(res$has_release_branch)
})

test_that("detects release pattern branches", {

  stub(get_repo_branches, "gh::gh",
       \(...) list(list(name = "20250101"),
                  list(name = "feature-x")))

  res <- get_repo_branches(owner = "foo", repo = "bar")

  expect_equal(res$release_branches, "20250101")
  expect_true(res$has_release_branch)
})

test_that("handles multiple release branches", {

  stub(get_repo_branches, "gh::gh",
       \(...) list(
         list(name = "20230101"),
         list(name = "20241231"),
         list(name = "main")
       ))

  res <- get_repo_branches(owner = "foo", repo = "bar")

  expect_equal(res$release_branches, c("20230101", "20241231"))
  expect_true(res$has_release_branch)
})

# ─────────────────────────────────────
# confirm_branch_exists
# ─────────────────────────────────────

test_that("returns TRUE when branch exists", {

  stub(confirm_branch_exists, "gh::gh",
       function(...) list(name = "main"))

  expect_true(confirm_branch_exists("main", measure = "test"))
})

test_that("returns FALSE on 404", {

  stub(confirm_branch_exists, "gh::gh",
       function(...) stop("404 Not Found"))

  expect_false(confirm_branch_exists("nope", measure = "test"))
})

test_that("propagates non-404 errors", {

  stub(confirm_branch_exists, "gh::gh",
       function(...) stop("500 Internal Server Error"))

  expect_error(
    confirm_branch_exists("main", measure = "test"),
    "500"
  )
})

test_that("defensive argument checks work", {

  expect_error(confirm_branch_exists(branch = 123, measure = "test"))
  expect_error(confirm_branch_exists(branch = c("a","b"), measure = "test"))
  expect_error(confirm_branch_exists(branch = "main", measure = "test", owner = 1))
  expect_error(confirm_branch_exists(branch = "main", measure = "test", repo = 2))
})

# ─────────────────────────────────────
# update_branches
# ─────────────────────────────────────

test_that("returns TRUE when content already identical", {

  stub(update_branches, "compare_branches_sha",
       function(...) list(sha_1 = "abc"))

  stub(update_branches, "compare_branch_content",
       function(...) list(same_content = TRUE))

  gh_mock <- mock()
  stub(update_branches, "gh::gh", gh_mock)

  res <- update_branches("me", "repo", "main", "dev")

  expect_true(res)
  expect_equal(mock_args(gh_mock), list())
})

test_that("updates when content differs and force=TRUE", {

  stub(update_branches, "compare_branches_sha",
       function(...) list(sha_1 = "abc"))

  stub(update_branches, "compare_branch_content",
       function(...) list(same_content = FALSE))

  gh_mock <- mock(TRUE)
  stub(update_branches, "gh::gh", gh_mock)

  res <- update_branches("me", "repo", "main", "dev", force = TRUE)

  expect_true(res)
  expect_called(gh_mock, 1)
})

test_that("aborts when force=FALSE and user declines", {

  stub(update_branches, "compare_branches_sha",
       function(...) list(sha_1 = "abc"))

  stub(update_branches, "compare_branch_content",
       function(...) list(same_content = FALSE))

  stub(update_branches, "utils::askYesNo",
       function(...) FALSE)

  expect_error(
    update_branches("me", "repo", "main", "dev", force = FALSE),
    "Update interrupted"
  )
})

test_that("returns FALSE if gh call fails", {

  stub(update_branches, "compare_branches_sha",
       function(...) list(sha_1 = "abc"))

  stub(update_branches, "compare_branch_content",
       function(...) list(same_content = FALSE))

  stub(update_branches, "gh::gh",
       function(...) stop("API error"))

  res <- update_branches("me", "repo", "main", "dev", force = TRUE)

  expect_false(res)
})

# ─────────────────────────────────────
# merge_branch_into
# ─────────────────────────────────────

test_that("merge returns TRUE when already identical", {

  stub(merge_branch_into, "compare_branch_content",
       function(...) list(same_content = TRUE))

  gh_mock <- mock()
  stub(merge_branch_into, "gh::gh", gh_mock)

  res <- merge_branch_into("me", "repo", "main", "dev")

  expect_true(res)
  expect_equal(mock_args(gh_mock), list())
})

test_that("merge executes when force=TRUE", {

  stub(merge_branch_into, "compare_branch_content",
       function(...) list(same_content = FALSE))

  gh_mock <- mock(TRUE)
  stub(merge_branch_into, "gh::gh", gh_mock)

  res <- merge_branch_into("me", "repo", "main", "dev", force = TRUE)

  expect_true(res)
  expect_called(gh_mock, 1)
})

test_that("merge aborts when force=FALSE and user declines", {

  stub(merge_branch_into, "compare_branch_content",
       function(...) list(same_content = FALSE))

  stub(merge_branch_into, "utils::askYesNo",
       function(...) FALSE)

  expect_error(
    merge_branch_into("me", "repo", "main", "dev", force = FALSE),
    "Merge interrupted"
  )
})

test_that("merge returns FALSE if gh fails", {

  stub(merge_branch_into, "compare_branch_content",
       function(...) list(same_content = FALSE))

  stub(merge_branch_into, "gh::gh",
       function(...) stop("API error"))

  res <- merge_branch_into("me", "repo", "main", "dev", force = TRUE)

  expect_false(res)
})