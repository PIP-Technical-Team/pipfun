
withr::local_options(list(pipfun.log_ini.ow = TRUE))

# log_init ------------
test_that("log_init creates an empty log", {
  log_init("testlog", overwrite = TRUE)
  log <- rlang::env_get(.piplogenv, "testlog")
  expect_s3_class(log, "piplog")
  expect_equal(nrow(log), 0)
})

test_that("log_init fails if log already exists", {
  log_init("testlog", overwrite = TRUE)
  log_init("testlog") |>
    expect_error()
})


# log_add ------------
test_that("log_add appends a new entry", {
  log_init("testlog", overwrite = TRUE)
  log_add(event = "info", message = "Test message", name = "testlog")
  log <- rlang::env_get(.piplogenv, "testlog")
  expect_equal(nrow(log), 1)
  expect_equal(log$message[[1]], "Test message")
  expect_equal(log$event[[1]], "info")
})

# helpers ------------
test_that("log_error adds an error entry", {
  log_init("testlog", overwrite = TRUE)
  log_error("An error occurred", name = "testlog")
  log <- rlang::env_get(.piplogenv, "testlog")
  expect_true("error" %in% log$event)
})

test_that("log_warn and log_info behave correctly", {
  log_init("testlog", overwrite = TRUE)
  log_warn("A warning", name = "testlog")
  log_info("Some info", name = "testlog")
  log <- rlang::env_get(.piplogenv, "testlog")
  expect_true(all(c("warning", "info") %in% log$event))
})

test_that("print.piplog produces output without error", {
  skip() # we need to test when print.piplog is finished.
  msg <- "Printing test"
  log_init("testlog", overwrite = TRUE)
  log_info(msg, name = "testlog")
  log <- rlang::env_get(.piplogenv, "testlog")
  expect_output(print(log), msg, fixed = FALSE)
})


# Save and load --------

test_that("log_save() and log_load() work as expected", {
  skip_on_ci()  # Skip on GitHub Actions or CI environments
  skip_if_not_installed("qs")
  skip_if_not_installed("fs")

  name <- "persist_test"
  path <- fs::file_temp(ext = "qs")

  # Create and populate log
  log_init(name, overwrite = TRUE)
  log_info("Saving this log", name = name)

  # Save to file
  expect_true(log_save(name = name, path = path))
  expect_true(fs::file_exists(path))

  # Clear from memory
  log_reset(name)
  expect_false(name %in% log_names())

  # Load back
  log_load(path = path, name = name)
  expect_true(name %in% log_names())

  # Check contents
  log <- rlang::env_get(.piplogenv, name)
  expect_s3_class(log, "piplog")
  expect_equal(nrow(log), 1)
  expect_match(log$message[1], "Saving this log")

  # Clean up
  fs::file_delete(path)
  log_reset(name)
})

# log_filter and log_summary -----------------------------------------------

test_that("log_filter() returns filtered entries", {
  log_init("testlog", overwrite = TRUE)

  log_info("Message 1", name = "testlog")
  log_warn("Message 2", name = "testlog")
  log_error("Message 3", name = "testlog")

  errors <- log_filter(name = "testlog", event = "error")
  expect_s3_class(errors, "piplog")
  expect_equal(nrow(errors), 1)
  expect_equal(errors$event, "error")

  warnings <- log_filter(name = "testlog", event = "warning")
  expect_equal(nrow(warnings), 1)
  expect_equal(warnings$event, "warning")
})

test_that("log_summary() returns event counts", {
  log_init("testlog", overwrite = TRUE)

  log_info("info again", name = "testlog")
  log_error("error again", name = "testlog")

  summary <- log_summary("testlog")
  expect_s3_class(summary, "data.table")
  expect_true(all(c("event", "count") %in% names(summary)))
  expect_true("info" %in% summary$event)
  expect_true("error" %in% summary$event)
})

# log_has_errors ----------------------------------------------------------

test_that("log_has_errors() returns correct logical or filtered log", {
  log_init("testlog", overwrite = TRUE)
  expect_false(log_has_errors("testlog"))

  log_error("something bad happened", name = "testlog")
  expect_true(log_has_errors("testlog"))

  log <- log_has_errors("testlog", show = TRUE)
  expect_s3_class(log, "piplog")
  expect_equal(log$event, "error")
})
