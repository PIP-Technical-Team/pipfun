# Test for get_pipenv function
test_that("get_pipenv returns the entire .pipenv environment", {
  # Ensure .pipenv is an environment
  expect_true(is.environment(get_pipenv()))

  # Ensure .pipenv is the same environment as the one we created
  expect_identical(get_pipenv(), .pipenv)
})

# Test for set_in_pipenv function
test_that("set_in_pipenv assigns a value in .pipenv", {
  # Set a value in .pipenv
  set_in_pipenv("test_key", 123)

  # Check if the value is correctly assigned
  expect_equal(.pipenv$test_key, 123)

  # Set another value in .pipenv
  set_in_pipenv("another_key", "test_value")

  # Check if the value is correctly assigned
  expect_equal(.pipenv$another_key, "test_value")
})

# Test for get_from_pipenv function
test_that("get_from_pipenv retrieves a value from .pipenv", {
  # Set a value in .pipenv for testing
  set_in_pipenv("test_key", 123)

  # Retrieve the value and check if it is correct
  expect_equal(get_from_pipenv("test_key"), 123)

  # Retrieve a non-existing key and check if it returns NULL
  expect_null(get_from_pipenv("non_existing_key"))
})

# Test for combined usage of set_in_pipenv and get_from_pipenv
test_that("set_in_pipenv and get_from_pipenv work together", {
  # Set a value in .pipenv
  set_in_pipenv("combined_key", 456)

  # Retrieve the value and check if it is correct
  expect_equal(get_from_pipenv("combined_key"), 456)
})

# Test for overwriting a value in .pipenv
test_that("set_in_pipenv overwrites an existing value in .pipenv", {
  # Set a value in .pipenv
  set_in_pipenv("overwrite_key", "initial_value")

  # Overwrite the value in .pipenv
  set_in_pipenv("overwrite_key", "new_value")

  # Retrieve the value and check if it is the new value
  expect_equal(get_from_pipenv("overwrite_key"), "new_value")
})
