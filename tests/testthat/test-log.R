
# log_init ------------
test_that("log_init creates an empty log", {
  log_init("testlog", overwrite = TRUE)
  log <- get("testlog", envir = .piplogenv)
  expect_s3_class(log, "piplog")
  expect_equal(nrow(log), 0)
})

# log_add ------------
test_that("log_add appends a new entry", {
  log_init("testlog", overwrite = TRUE)
  log_add(event = "info", message = "Test message", name = "testlog")
  log <- get("testlog", envir = .piplogenv)
  expect_equal(nrow(log), 1)
  expect_equal(log$message[[1]], "Test message")
  expect_equal(log$event[[1]], "info")
})

# helpers ------------
test_that("log_error adds an error entry", {
  log_init("testlog", overwrite = TRUE)
  log_error("An error occurred", name = "testlog")
  log <- get("testlog", envir = .piplogenv)
  expect_true("error" %in% log$event)
})

test_that("log_warn and log_info behave correctly", {
  log_init("testlog", overwrite = TRUE)
  log_warn("A warning", name = "testlog")
  log_info("Some info", name = "testlog")
  log <- get("testlog", envir = .piplogenv)
  expect_true(all(c("warning", "info") %in% log$event))
})

test_that("print.piplog produces output without error", {
  log_init("testlog", overwrite = TRUE)
  log_info("Printing test", name = "testlog")
  log <- get("testlog", envir = .piplogenv)
  expect_output(print(log), "LOG", fixed = FALSE)
})
