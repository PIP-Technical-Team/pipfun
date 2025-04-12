#' Log an error, warning, or informational message
#'
#' These are wrapper functions around `log_add()` with predefined event types:
#' `"error"`, `"warning"`, or `"info"`. They automatically capture arguments
#' from the calling function, including `...`, and allow optional metadata
#' using `logmeta`.
#'
#' @param message The log message.
#' @param ... Additional arguments from the caller to be captured.
#' @param name Name of the log (default: `getOption("pipfun.log.default")`).
#' @param output Optional result or return value to include in the log.
#' @param logmeta Optional named list of additional metadata.
#' @param .trace Optional trace object or call stack override.
#'
#' @return Invisibly returns `TRUE` if the log was updated successfully.
#'
#' @examples
#' log_init("example", overwrite = TRUE)
#' log_info("Starting process", name = "example", user = "analyst", stage = "init")
#'
#' my_function <- function(x, ...) {
#'   result <- x^2
#'   log_info("Squared a value", x = x, output = result, name = "example")
#'   result
#' }
#' my_function(4)
#'
#' # With additional metadata
#' log_error("Failure to connect", name = "example", logmeta = list(server = "db01", status = 500))
#'
#' @export
log_info <- function(message, ...,
                     name = getOption("pipfun.log.default"),
                     output = NULL,
                     logmeta = NULL,
                     .trace = NULL) {
  log_add(event = "info",
          message = message,
          name    = name,
          output  = output,
          logmeta = logmeta,
          .trace  = .trace,
          .env    = parent.frame(),
          ...)
}

#' @rdname log_info
#' @export
log_warn <- function(message, ...,
                     name = getOption("pipfun.log.default"),
                     output = NULL,
                     logmeta = NULL,
                     .trace = NULL) {
  log_add(event = "warning",
          message = message,
          name    = name,
          output  = output,
          logmeta = logmeta,
          .trace  = .trace,
          .env    = parent.frame(),
          ...)
}

#' @rdname log_info
#' @export
log_error <- function(message, ...,
                      name = getOption("pipfun.log.default"),
                      output = NULL,
                      logmeta = NULL,
                      .trace = NULL) {
  log_add(event = "error",
          message = message,
          name    = name,
          output  = output,
          logmeta = logmeta,
          .trace  = .trace,
          .env    = parent.frame(),
          ...)
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
    if (!is.null(x$trace[[i]])) {
      trace_str <- tryCatch(
        deparse(x$trace[[i]]),
        error = function(e)
          NULL
      )
      if (!is.null(trace_str))
        cli::cli_text("Trace: {trace_str}")
    }
    cli::cli_text("")
  }

  invisible(x)
}


#' List all active logs
#'
#' Returns the names of all logs currently stored in `.piplogenv`.
#'
#' @return Character vector of log names.
#' @export
log_names <- function() {
  rlang::env_names(.piplogenv)
}


#' Check whether a log exists
#'
#' @param name Name of the log (default: `pipfun.log.default`)
#'
#' @return Logical. `TRUE` if log exists in env .piplogenv
#' @export
log_exists <- function(name = getOption("pipfun.log.default")) {
  rlang::env_has(.piplogenv, name)
}



#' Check whether a log contains any errors
#'
#' @param name Name of the log (default: `pipfun.log.default`)
#' @param show Logical: whether to return the filtered error log (default:
#'   FALSE).
#'
#' @return Logical if `show = FALSE`; a filtered `piplog` object if `show =
#'   TRUE`.
#' @export
log_has_errors <- function(name = getOption("pipfun.log.default"),
                           show = FALSE) {
  log <- log_filter(name = name, event = "error")
  if (!isTRUE(show)) {
    return(nrow(log) > 0)
  } else {
    return(log)
  }
}

#' Summarize log contents
#'
#' Provides a summary of the number of log entries by event type and function.
#'
#' @param name Name of the log (default: pipfun.log.default)
#' @param by Character vector of grouping variables (default: c("event"))
#'
#' @return A data.table with summary counts.
#' @export
log_summary <- function(name = getOption("pipfun.log.default"),
                        by = "event") {

  log <- log_get(name)
  setDT(log)

  if (!all(by %in% names(log))) {
    cli::cli_abort("Invalid grouping column{?s}:
                   {.field {by[!by %in% names(log)]}}")
  }

  summary <- log[, .N, by = by]
  setnames(summary, "N", "count")

  setattr(summary, "class", c("log_summary", class(summary)))
  return(summary)
}

