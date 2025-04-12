
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
    log_add("info", "capturing args",
            name = "test_capture",
            .env = parent.frame())
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


### capturing logmeta -----------
test_that("log_add() captures logmeta passed to log_info()", {
  log_init("dotstest", overwrite = TRUE)

  log_info("Testing dots", name = "dotstest", logmeta = list(a = 1:3, b = "hello"))
  log <- log_get("dotstest")
  args <- log$args[[1]]

  expect_true("a" %in% names(args))
  expect_equal(args$b, "hello")
})

test_that("log_add() captures logmeta passed directly", {
  log_init("directdots", overwrite = TRUE)

  log_add("info", "Direct dots test", name = "directdots",
          logmeta = list(x = 99, y = "yay"))

  log <- log_get("directdots")
  args <- log$args[[1]]

  expect_equal(args$x, 99)
  expect_equal(args$y, "yay")
})

test_that("log_add() respects args override", {
  log_init("explicitargs", overwrite = TRUE)

  log_add("info", "Explicit args test", name = "explicitargs",
          args = list(z = "onlythis"))

  log <- log_get("explicitargs")
  args <- log$args[[1]]

  expect_equal(names(args), "z")
  expect_equal(args$z, "onlythis")
})

test_that("log_add() merges all captured arguments and logmeta correctly", {
  log_init("mergeargs", overwrite = TRUE)

  test_fun <- function(a = 1, b = 2, ...) {
    log_info("Merged", name = "mergeargs", logmeta = list(...))
  }

  test_fun(x = "hello", y = TRUE)

  log <- log_get("mergeargs")
  args <- log$args[[1]]

  expect_true(all(c("a", "b", "x", "y") %in% names(args)))
  expect_equal(args$x, "hello")
  expect_true(args$y)
})




# helpers ------------
test_that("log_info() captures arguments from parent function", {
  log_init("loginfo_test", overwrite = TRUE)

  my_function <- function(a = 1, b = 2, ...) {
    log_info("Info from my_function", name = "loginfo_test")
  }

  my_function(x = 42)

  log <- log_get("loginfo_test")
  args <- log$args[[1]]

  expect_true("a" %in% names(args))
  expect_true("b" %in% names(args))
  expect_true("x" %in% names(args))
  expect_equal(args$x, 42)
})

test_that("log_warn() captures arguments and logs warning", {
  log_init("logwarn_test", overwrite = TRUE)

  my_warn <- function(y = 10, ...) {
    log_warn("Warning issued", name = "logwarn_test")
  }

  my_warn(extra = TRUE)
  log <- log_get("logwarn_test")
  args <- log$args[[1]]

  expect_equal(log$event[[1]], "warning")
  expect_true("y" %in% names(args))
  expect_true("extra" %in% names(args))
})

test_that("log_error() captures environment and logs error", {
  log_init("logerror_test", overwrite = TRUE)

  my_error <- function(code = "A") {
    log_error("Error encountered", name = "logerror_test")
  }

  my_error()
  log <- log_get("logerror_test")
  args <- log$args[[1]]

  expect_equal(log$event[[1]], "error")
  expect_equal(args$code, "A")
})


## print --------------
test_that("print.piplog produces output without error", {
  skip() # we need to test when print.piplog is finished.
  msg <- "Printing test"
  log_init("testlog", overwrite = TRUE)
  log_info(msg, name = "testlog")
  log <- rlang::env_get(.piplogenv, "testlog")
  expect_output(print(log), msg, fixed = FALSE)
})

test_that("log_exists() works as expected", {
  log_init("existtest", overwrite = TRUE)
  expect_true(log_exists("existtest"))

  log_reset("existtest")
  expect_false(log_exists("existtest"))
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

test_that("log_summary returns correct counts", {
  log_init("testlog", overwrite = TRUE)

  log_info("Info msg", name = "testlog")
  log_warn("Warn msg", name = "testlog")
  log_error("Error msg", name = "testlog")

  s <- log_summary("testlog")
  expect_s3_class(s, "log_summary")
  expect_equal(sum(s$count), 3)
  expect_true(all(s$event %in% c("info", "warning", "error")))
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


## log get ------
test_that("log_get restores class if dropped", {
  log_init("reclass_test", overwrite = TRUE)
  log_info("Test", name = "reclass_test")

  # simulate class drop
  log <- rlang::env_get(.piplogenv, "reclass_test")
  setattr(log, "class", "data.table")
  rlang::env_poke(.piplogenv, "reclass_test", log)

  restored <- log_get("reclass_test")
  expect_s3_class(restored, "piplog")
})
