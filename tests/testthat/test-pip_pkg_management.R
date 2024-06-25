# Test for is_package_loaded
test_that("is_package_loaded returns TRUE for loaded packages", {
  expect_true(is_package_loaded("base")) # base should always be loaded
})

test_that("is_package_loaded returns FALSE for not loaded packages", {
  expect_false(is_package_loaded("thisPackageShouldNotExistHopefully"))
})

# Test for is_package_attached
test_that("is_package_attached returns TRUE for attached packages", {
  # Assuming fst is attached for this test
  pkg <- "fst"
  expect_false(is_package_attached(pkg))

  library(pkg, character.only = TRUE)
  expect_true(is_package_attached(pkg))
  detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE)
})

test_that("is_package_attached returns FALSE for not attached packages", {
  expect_false(is_package_attached("thisPackageShouldNotExistHopefully"))
})


test_that("check_pkg_active aborts for inactive packages", {
  expect_error(check_pkg_active("thisPackageShouldNotExistHopefully"))
})

# Test for is_package_attached
test_that("Attached and loaded as expected", {
  # data.table is fully imported by pipfun
  pkg <- "data.table"
  expect_true(is_package_loaded(pkg))

  # yet it is not attached
  expect_false(is_package_attached(pkg))

  # now we attach it
  library(pkg, character.only = TRUE)
  expect_true(is_package_attached(pkg))

  # And detach it again

  unloadNamespace(paste0("package:", pkg))

  # it is not attached anymore
  expect_false(is_package_attached(pkg))

  # but it is loaded
  expect_true(is_package_loaded(pkg))
})
