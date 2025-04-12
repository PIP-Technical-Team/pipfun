#' Log an error/info/warning
#'
#' Wrapper functions for `log_add()` with predefined event types.
#'
#' @param message The log message.
#' @param ... Additional objects passed to the log or the originating function.
#' @param name Name of the log (default from options).
#' @param output Optional result or return value to include in the log.
#' @param .trace Optional trace object or call stack override.
#'
#' @return Invisibly returns the updated log.
#' @export
log_error <- function(message, ...,
                      name = getOption("pipfun.log.default"),
                      output = NULL,
                      .trace = NULL) {
  log_add("error", message,
          name = name,
          output = output,
          .trace = .trace,
          .env = parent.frame())
}

#' @rdname log_error
#' @export
log_warn <- function(message, ...,
                     name = getOption("pipfun.log.default"),
                     output = NULL,
                     .trace = NULL) {
  log_add("warning", message,
          name = name,
          output = output,
          .trace = .trace,
          .env = parent.frame())
}

#' @rdname log_error
#' @export
log_info <- function(message, ...,
                     name = getOption("pipfun.log.default"),
                     output = NULL,
                     .trace = NULL) {
  log_add("info", message,
          name = name,
          output = output,
          .trace = .trace,
          .env = parent.frame())
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


#' Check whether a log contains any errors
#'
#' @param name Name of the log (default: `pipfun.log.default`)
#' @param show Logical: whether to return the filtered error log (default: FALSE).
#'
#' @return Logical if `show = FALSE`; a filtered `piplog` object if `show = TRUE`.
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





#' Summarize a log by event type
#'
#' @param name Name of the log (default: `pipfun.log.default`)
#'
#' @return A data.table with counts per event.
#' @export
log_summary <- function(name = getOption("pipfun.log.default")) {
  if (!rlang::env_has(.piplogenv, name)) {
    cli::cli_abort("Log {.field {name}} does not exist.")
  }

  log <- rlang::env_get(.piplogenv, name)

  if (!inherits(log, "piplog")) {
    cli::cli_abort("Object {.field {name}} is not a valid piplog.")
  }

  summary <- log[, .N, by = .(event)]
  setnames(summary, "N", "count")
  summary[]
}
