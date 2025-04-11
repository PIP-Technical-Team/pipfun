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
log_error <- function(message, ...,
                      name = getOption("pipfun.log.default"),
                      output = NULL,
                      .trace = NULL) {
  log_add("error", message, name = name, output = output, .trace = .trace, .env = parent.frame())
}

#' @rdname log_error
#' @export
log_info <- function(message, ...,
                     name = getOption("pipfun.log.default"),
                     output = NULL,
                     .trace = NULL) {
  log_add("info", message, name = name, output = output, .trace = .trace, .env = parent.frame())
}

#' @rdname log_error
#' @export
log_warn <- function(message, ...,
                     name = getOption("pipfun.log.default"),
                     output = NULL,
                     .trace = NULL) {
  log_add("warning", message, name = name, output = output, .trace = .trace, .env = parent.frame())
}


#' @export
print.piplog <- function(x, ...) {
  cli::cli_h2("Log entries:")
  if (nrow(x) == 0) {
    cli::cli_alert_info("The log is empty.")
    return(invisible(x))
  }
  for (i in seq_len(nrow(x))) {
    cli::cli_alert("{.strong [{x$time[i]}]} {.emph {toupper(x$event[i])}} — {.code {x$message[i]}}")
    cli::cli_text("Function: {.code {x$fun[i]}} (from {x$package[i]})")
    if (!is.null(x$trace[[i]])) cli::cli_text("• Trace: {deparse(x$trace[[i]])}")
    cli::cli_text("")
  }
  invisible(x)
}
