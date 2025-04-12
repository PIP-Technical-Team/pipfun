
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

test_that("log_add captures arguments when args is NULL", {
  log_init("test_capture", overwrite = TRUE)

  # Simulate a caller function
  simulate_caller <- function(a = 1, b = "text") {
    log_add("info", "capturing args", name = "test_capture", .env = environment())
  }

  simulate_caller()

  log <- log_get("test_capture")
  last <- log[.N]

  expect_type(last$args[[1]], "list")
  expect_named(last$args[[1]], c("a", "b"))
  expect_equal(last$args[[1]]$a, 1)
  expect_equal(last$args[[1]]$b, "text")
})


test_that("log_add uses provided args if not NULL", {
  log_init("test_explicit", overwrite = TRUE)

  log_add("info", "manual args", name = "test_explicit", args = list(foo = 123))

  log <- log_get("test_explicit")
  last <- log[.N]

  expect_equal(last$args[[1]]$foo, 123)
})


test_that("log_add captures output if provided", {
  log_init("test_output", overwrite = TRUE)

  result <- sum(1:5)
  log_add("info", "with result", name = "test_output", output = result)

  log <- log_get("test_output")
  last <- log[.N]

  expect_equal(last$output[[1]], result)
})


test_that("log_add fallback to sys.call when .trace is NULL", {
  log_init("test_trace", overwrite = TRUE)

  log_add("info", "trace test", name = "test_trace")

  log <- log_get("test_trace")
  last <- log[.N]

  expect_true(inherits(last$trace[[1]], "call"))
})


# helpers ------------
test_that("log_error captures message and calling arguments", {
  log_init("testlog", overwrite = TRUE)

  simulate_error <- function(a = 42, b = "oops") {
    log_error("This is an error", name = "testlog")
  }

  simulate_error()
  log <- log_get("testlog")

  expect_equal(nrow(log), 1)
  expect_equal(log$event[[1]], "error")
  expect_match(log$message[[1]], "This is an error")
  expect_equal(log$args[[1]]$a, 42)
  expect_equal(log$args[[1]]$b, "oops")
})

test_that("log_warn captures arguments correctly", {
  log_init("testlog", overwrite = TRUE)

  simulate_warn <- function(x = TRUE) {
    log_warn("This is a warning", name = "testlog")
  }

  simulate_warn()
  log <- log_get("testlog")

  expect_equal(nrow(log), 1)
  expect_equal(log$event[[1]], "warning")
  expect_match(log$message[[1]], "This is a warning")
  expect_equal(log$args[[1]]$x, TRUE)
})

test_that("log_info captures output and arguments", {
  log_init("testlog", overwrite = TRUE)

  simulate_info <- function(vec = 1:3) {
    result <- sum(vec)
    log_info("Logging info with result", name = "testlog", output = result)
  }

  simulate_info()
  log <- log_get("testlog")

  expect_equal(nrow(log), 1)
  expect_equal(log$event[[1]], "info")
  expect_equal(log$args[[1]]$vec, 1:3)
  expect_equal(log$output[[1]], 6)
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
