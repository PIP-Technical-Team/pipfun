withr::local_options(list(pipfun.log_init.ow = TRUE))

# log_init ------------
test_that("log_init creates an empty log with all columns", {
  log_init("testlog", overwrite = TRUE)
  log <- rlang::env_get(.piplogenv, "testlog")
  expect_s3_class(log, "piplog")
  expect_equal(nrow(log), 0)
  expect_named(log, c("time", "package", "fun", "event", "message", "args", "output", "trace"))
})

test_that("log_init fails if log already exists without overwrite", {
  log_init("testlog", overwrite = TRUE)
  expect_error(
    log_init("testlog", overwrite = FALSE),
    "already exists"
  )
})

test_that("log_init overwrites existing log when overwrite = TRUE", {
  log_init("testlog", overwrite = TRUE)
  log_add("info", "first entry", name = "testlog")
  expect_equal(nrow(rlang::env_get(.piplogenv, "testlog")), 1)

  log_init("testlog", overwrite = TRUE)
  expect_equal(nrow(rlang::env_get(.piplogenv, "testlog")), 0)
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
    log_add("info", "capturing args", name = "test_capture")
  }

  simulate_caller()

  log <- rlang::env_get(.piplogenv, "test_capture")
  last <- log[.N]

  expect_type(last$args[[1]], "list")
  expect_named(last$args[[1]], c("a", "b"))
  expect_equal(last$args[[1]]$a, 1)
  expect_equal(last$args[[1]]$b, "text")
})

test_that("log_add captures ... arguments", {
  log_init("test_dots", overwrite = TRUE)

  caller_with_dots <- function(a = 1, ...) {
    log_add("info", "with dots", name = "test_dots")
  }

  caller_with_dots(x = 10, y = "hello")

  log <- rlang::env_get(.piplogenv, "test_dots")
  args <- log$args[[1]]

  expect_true(all(c("a", "x", "y") %in% names(args)))
  expect_equal(args$x, 10)
  expect_equal(args$y, "hello")
})

test_that("log_add uses provided args if not NULL", {
  log_init("test_explicit", overwrite = TRUE)

  log_add("info", "manual args", name = "test_explicit", args = list(foo = 123))

  log <- rlang::env_get(.piplogenv, "test_explicit")
  last <- log[.N]

  expect_equal(last$args[[1]]$foo, 123)
})

test_that("log_add captures output if provided", {
  log_init("test_output", overwrite = TRUE)

  result <- sum(1:5)
  log_add("info", "with result", name = "test_output", output = result)

  log <- rlang::env_get(.piplogenv, "test_output")
  last <- log[.N]

  expect_equal(last$output[[1]], result)
})

test_that("log_add stores trace when .trace is NULL", {
  log_init("test_trace", overwrite = TRUE)

  log_add("info", "trace test", name = "test_trace")

  log <- rlang::env_get(.piplogenv, "test_trace")
  last <- log[.N]

  expect_true(inherits(last$trace[[1]], "call"))
})

test_that("log_add stores custom trace when .trace is provided", {
  log_init("test_custom_trace", overwrite = TRUE)

  custom_trace <- quote(my_function(x = 1))
  log_add("info", "custom trace", name = "test_custom_trace", .trace = custom_trace)

  log <- rlang::env_get(.piplogenv, "test_custom_trace")
  last <- log[.N]

  expect_equal(last$trace[[1]], custom_trace)
})

test_that("log_add captures logmeta", {
  log_init("test_logmeta", overwrite = TRUE)

  log_add("info", "with metadata", name = "test_logmeta",
          logmeta = list(stage = "processing", user = "analyst"))

  log <- rlang::env_get(.piplogenv, "test_logmeta")
  meta <- log$logmeta[[1]]

  expect_equal(meta$stage, "processing")
  expect_equal(meta$user, "analyst")
})

test_that("log_add stores NULL logmeta when not provided", {
  log_init("test_null_meta", overwrite = TRUE)

  log_add("info", "no metadata", name = "test_null_meta")

  log <- rlang::env_get(.piplogenv, "test_null_meta")
  last <- log[.N]

  expect_null(last$logmeta[[1]])
})

test_that("log_add respects args override and ignores caller environment", {
  log_init("test_override", overwrite = TRUE)

  caller_fn <- function(a = 1, b = 2) {
    log_add("info", "override test", name = "test_override",
            args = list(z = "onlythis"))
  }

  caller_fn()

  log <- rlang::env_get(.piplogenv, "test_override")
  args <- log$args[[1]]

  expect_equal(names(args), "z")
  expect_equal(args$z, "onlythis")
})

test_that("log_add converts event to lowercase", {
  log_init("test_case", overwrite = TRUE)

  log_add("INFO", "uppercase event", name = "test_case")

  log <- rlang::env_get(.piplogenv, "test_case")
  expect_equal(log$event[[1]], "info")
})

test_that("log_add converts message to character", {
  log_init("test_msg_convert", overwrite = TRUE)

  log_add("info", 42, name = "test_msg_convert")

  log <- rlang::env_get(.piplogenv, "test_msg_convert")
  expect_equal(log$message[[1]], "42")
  expect_type(log$message[[1]], "character")
})

test_that("log_add stores timestamp", {
  log_init("test_timestamp", overwrite = TRUE)

  before <- Sys.time()
  log_add("info", "timestamp test", name = "test_timestamp")
  after <- Sys.time()

  log <- rlang::env_get(.piplogenv, "test_timestamp")
  ts <- log$time[[1]]

  expect_true(ts >= before && ts <= after)
})

test_that("log_add returns TRUE invisibly", {
  log_init("test_return", overwrite = TRUE)

  result <- log_add("info", "return test", name = "test_return")

  expect_identical(result, TRUE)
})


# log_reset -------
test_that("log_reset removes a log", {
  log_init("reset_test", overwrite = TRUE)
  log_add("info", "entry", name = "reset_test")

  result <- log_reset("reset_test")

  expect_true(result)
  expect_false(rlang::env_has(.piplogenv, "reset_test"))
})

test_that("log_reset returns FALSE if log doesn't exist", {
  result <- log_reset("nonexistent_log_xyz")
  expect_false(result)
})


# log_get -------
test_that("log_get retrieves an existing log", {
  log_init("get_test", overwrite = TRUE)
  log_add("info", "test entry", name = "get_test")

  log <- log_get("get_test")

  expect_s3_class(log, "piplog")
  expect_equal(nrow(log), 1)
})

test_that("log_get restores piplog class if dropped", {
  log_init("reclass_test", overwrite = TRUE)
  log_add("info", "test", name = "reclass_test")

  log <- rlang::env_get(.piplogenv, "reclass_test")
  setattr(log, "class", "data.table")
  rlang::env_poke(.piplogenv, "reclass_test", log)

  restored <- log_get("reclass_test")
  expect_s3_class(restored, "piplog")
})

test_that("log_get errors if log doesn't exist", {
  expect_error(log_get("nonexistent"), "does not exist")
})

test_that("log_get returns invisibly", {
  log_init("invisible_test", overwrite = TRUE)

  # Capture the result to verify invisibility (should not print by default)
  result <- withVisible(log_get("invisible_test"))
  expect_false(result$visible)
})


# log_filter -------
test_that("log_filter filters by event type", {
  log_init("filter_test", overwrite = TRUE)
  log_add("info", "info message", name = "filter_test")
  log_add("warning", "warning message", name = "filter_test")
  log_add("error", "error message", name = "filter_test")

  filtered <- log_filter(name = "filter_test", event = "warning")

  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$event[[1]], "warning")
  expect_s3_class(filtered, "piplog")
})

test_that("log_filter filters by multiple event types", {
  log_init("filter_multi", overwrite = TRUE)
  log_add("info", "info", name = "filter_multi")
  log_add("warning", "warning", name = "filter_multi")
  log_add("error", "error", name = "filter_multi")

  filtered <- log_filter(name = "filter_multi", event = c("warning", "error"))

  expect_equal(nrow(filtered), 2)
  expect_true(all(filtered$event %in% c("warning", "error")))
})

test_that("log_filter returns empty piplog when no matches", {
  log_init("filter_empty", overwrite = TRUE)
  log_add("info", "test", name = "filter_empty")

  filtered <- log_filter(name = "filter_empty", event = "error")

  expect_equal(nrow(filtered), 0)
  expect_s3_class(filtered, "piplog")
})

test_that("log_filter does not modify original log", {
  log_init("filter_original", overwrite = TRUE)
  log_add("info", "info", name = "filter_original")
  log_add("error", "error", name = "filter_original")

  original_size <- nrow(log_get("filter_original"))
  log_filter(name = "filter_original", event = "info")

  expect_equal(nrow(log_get("filter_original")), original_size)
})


# log_save and log_load --------
test_that("log_save saves a log to disk", {
  root <- fs::path(tempdir(), "pipfun_test_save")
  fs::dir_create(root, recurse = TRUE)
  stamp::st_init(root, alias = "test_save")

  id <- fs::path(root, "test_log")

  log_init("test_save_log", overwrite = TRUE)
  log_add("info", "save test", name = "test_save_log")

  result <- log_save(name = "test_save_log", id = id, alias = "test_save", format = "qs2")

  expect_true(!is.null(result))
  expect_true(fs::file_exists(fs::path_ext_set(id, "qs2")))
})

test_that("log_save adds extension if missing", {
  root <- fs::path(tempdir(), "pipfun_test_ext")
  fs::dir_create(root, recurse = TRUE)
  stamp::st_init(root, alias = "test_ext")

  id <- fs::path(root, "test_no_ext")

  log_init("test_ext_log", overwrite = TRUE)
  log_add("info", "extension test", name = "test_ext_log")
  log_save(name = "test_ext_log", id = id, alias = "test_ext", format = "qs2")

  # File should exist with .qs2 extension
  expect_true(fs::file_exists(fs::path_ext_set(id, "qs2")))
})

test_that("log_save errors if log doesn't exist", {
  expect_error(log_save(name = "nonexistent_log"), "does not exist")
})

test_that("log_load loads a saved log", {
  root <- fs::path(tempdir(), "pipfun_test_load")
  fs::dir_create(root, recurse = TRUE)
  stamp::st_init(root, alias = "test_load")

  id <- fs::path(root, "test_load_log")

  log_init("save_me", overwrite = TRUE)
  log_add("info", "entry 1", name = "save_me")
  log_add("warning", "entry 2", name = "save_me")
  log_save(name = "save_me", id = id, alias = "test_load", format = "qs2")

  log_reset("save_me")
  loaded <- log_load(id = id, name = "loaded", alias = "test_load", format = "qs2")

  expect_s3_class(loaded, "piplog")
  expect_equal(nrow(loaded), 2)
  expect_true(rlang::env_has(.piplogenv, "loaded"))
})

test_that("log_load adds extension if missing", {
  root <- fs::path(tempdir(), "pipfun_test_load_ext")
  fs::dir_create(root, recurse = TRUE)
  stamp::st_init(root, alias = "test_load_ext")

  id <- fs::path(root, "no_ext")

  log_init("save_no_ext", overwrite = TRUE)
  log_add("info", "test", name = "save_no_ext")
  log_save(name = "save_no_ext", id = id, alias = "test_load_ext", format = "qs2")

  log_reset("save_no_ext")
  loaded <- log_load(id = id, name = "loaded_no_ext", alias = "test_load_ext", format = "qs2")

  expect_s3_class(loaded, "piplog")
})

test_that("log_load lists available versions", {
  root <- fs::path(tempdir(), "pipfun_test_versions")
  fs::dir_create(root, recurse = TRUE)
  stamp::st_init(root, alias = "test_versions")

  id <- fs::path(root, "version_test")

  log_init("v1", overwrite = TRUE)
  log_add("info", "version 1", name = "v1")
  log_save(name = "v1", id = id, alias = "test_versions", format = "qs2")

  log_init("v2", overwrite = TRUE)
  log_add("info", "version 2", name = "v2")
  log_save(name = "v2", id = id, alias = "test_versions", format = "qs2")

  versions <- log_load(id = id, version = "available", alias = "test_versions", format = "qs2")

  expect_true(is.data.table(versions))
  expect_true("vintage" %in% names(versions))
  expect_true(nrow(versions) >= 2)
})

test_that("log_load respects overwrite = FALSE", {
  root <- fs::path(tempdir(), "pipfun_test_no_overwrite")
  fs::dir_create(root, recurse = TRUE)
  stamp::st_init(root, alias = "test_no_ow")

  id <- fs::path(root, "no_ow_test")

  log_init("original", overwrite = TRUE)
  log_add("info", "original entry", name = "original")
  log_save(name = "original", id = id, alias = "test_no_ow", format = "qs2")

  log_init("same_name", overwrite = TRUE)

  expect_error(
    log_load(id = id, name = "same_name", alias = "test_no_ow", format = "qs2", overwrite = FALSE),
    "already exists"
  )
})

test_that("log_load respects overwrite = TRUE", {
  root <- fs::path(tempdir(), "pipfun_test_yes_overwrite")
  fs::dir_create(root, recurse = TRUE)
  stamp::st_init(root, alias = "test_yes_ow")

  id <- fs::path(root, "yes_ow_test")

  log_init("to_save", overwrite = TRUE)
  log_add("info", "saved entry", name = "to_save")
  log_save(name = "to_save", id = id, alias = "test_yes_ow", format = "qs2")

  log_init("target", overwrite = TRUE)
  log_add("info", "old entry", name = "target")
  expect_equal(nrow(log_get("target")), 1)

  log_load(id = id, name = "target", alias = "test_yes_ow", format = "qs2", overwrite = TRUE)

  loaded_log <- log_get("target")
  expect_equal(nrow(loaded_log), 1)
  expect_equal(loaded_log$message[[1]], "saved entry")
})

test_that("log_load errors if file doesn't exist", {
  expect_error(
    log_load(id = "nonexistent/path", name = "test")
  )
})
