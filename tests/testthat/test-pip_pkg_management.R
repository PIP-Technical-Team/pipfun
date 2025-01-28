# Test for is_package_loaded
test_that("is_package_loaded returns TRUE for loaded packages", {
  expect_true(is_package_loaded("base")) # base should always be loaded
})

test_that("is_package_loaded returns FALSE for not loaded packages", {
  expect_false(is_package_loaded("thisPackageShouldNotExistHopefully"))
})

test_that("is_package_attached returns TRUE for attached packages", {
  # Assuming fst is attached for this test
  pkg <- "fst"

  # Ensure the package is detached before starting
  if (pkg %in% (.packages())) {
    detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE)
  }
  expect_false(is_package_attached(pkg)) # Package should not be attached

  # Load the package
  library(pkg, character.only = TRUE)
  Sys.sleep(0.1) # Ensure the library is attached
  expect_true(is_package_attached(pkg)) # Package should now be attached

  # Detach the package
  detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE)
  Sys.sleep(0.1) # Ensure the package is detached
  expect_false(is_package_attached(pkg)) # Package should now be detached
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

  # Check that the package is loaded
  expect_true(is_package_loaded(pkg))

  # Now we attach it
  library(pkg, character.only = TRUE)
  expect_true(is_package_attached(pkg))

  # Package is loaded and attached
  expect_true(is_package_loaded(pkg))
  expect_true(is_package_attached(pkg))

  # Skip unloading since it's imported by pipfun and can't be unloaded
})

