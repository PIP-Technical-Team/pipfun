#' Log an error/info/warning
#'
#' Wrapper functions for `log_add()` with predefined event types.
#'
#' @param message The log message.
#' @param ... Additional data to include.
#' @param name Name of the log (default: "default").
#' @param output Optional output to capture.
#' @param .trace Optional trace object.
#'
#' @return Invisibly returns the updated log.
#' @export
log_error <- function(message, ..., name = "default", output = NULL, .trace = NULL) {
  log_add("error", message, name = name, output = output, .trace = .trace, .env = parent.frame())
}

#' @rdname log_error
#' @export
log_info <- function(message, ..., name = "default", output = NULL, .trace = NULL) {
  log_add("info", message, name = name, output = output, .trace = .trace, .env = parent.frame())
}

#' @rdname log_error
#' @export
log_warn <- function(message, ..., name = "default", output = NULL, .trace = NULL) {
  log_add("warning", message, name = name, output = output, .trace = .trace, .env = parent.frame())
}


