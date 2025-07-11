#' Log an error, warning, or info event
#'
#' These are wrapper functions for `log_add()` to log events of type "error", "warning", or "info".
#' They automatically capture arguments from the parent function and include them in the log.
#'
#' @param message The log message.
#' @param name Name of the log (default: `pipfun.log.default`).
#' @param output Optional result or return value to include in the log.
#' @param .trace Optional trace object or call stack override.
#' @param .env Environment from which to capture arguments (default: `parent.frame()`).
#' @param logmeta Optional named list of metadata to include (tags, user info, etc.).
#'
#' @return Invisibly returns TRUE after updating the log.
#'
#' @examples
#' log_init("mylog", overwrite = TRUE)
#'
#' # Simulate calling context
#' my_function <- function(a = 1, b = 2, ...) {
#'   log_info("This is an info message", name = "mylog")
#' }
#' my_function(x = 42)
#'
#' log_get("mylog")
#'
#' log_reset("mylog")
#'
#' @export

log_info <- function(message,
                     name    = getOption("pipfun.log.default"),
                     output  = NULL,
                     .trace  = NULL,
                     .env    = parent.frame(),
                     logmeta = NULL) {
  log_add(event   = "info",
          message = message,
          name    = name,
          args    = NULL,
          output  = output,
          .trace  = .trace,
          logmeta = logmeta,
          .env    = .env)
}


#' @rdname log_info
#' @export

log_warn <- function(message,
                     name    = getOption("pipfun.log.default"),
                     output  = NULL,
                     .trace  = NULL,
                     .env    = parent.frame(),
                     logmeta = NULL) {
  log_add(event   = "warning",
          message = message,
          name    = name,
          args    = NULL,
          output  = output,
          .trace  = .trace,
          logmeta = logmeta,
          .env    = .env)
}


#' @rdname log_info
#' @export

log_error <- function(message,
                      name    = getOption("pipfun.log.default"),
                      output  = NULL,
                      .trace  = NULL,
                      .env    = parent.frame(),
                      logmeta = NULL) {
  log_add(event   = "error",
          message = message,
          name    = name,
          args    = NULL,
          output  = output,
          .trace  = .trace,
          logmeta = logmeta,
          .env    = .env)
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


#' Print a log nicely (for use in vignettes or reports)
#' @param name Log name (default: pipfun.log.default)
#' @export
log_show <- function(name = getOption("pipfun.log.default")) {
  log <- log_get(name)
  setattr(log, "class", unique(c("piplog", class(log))))
  print(log)
  invisible(log)
}




#' Argument Inspector
#'
#' introspects how arguments are being resolved — showing what was passed
#' explicitly, what was set by default, and what's visible in the environment at
#' runtime. This is super helpful when trying to debug or understand how
#' log_add() captures arguments using environment()
#'
#' @param .env environment where the arguments are coming from
#'
#' @returns `inspect_args` class object
#' @export
inspect_args <- function(.env = parent.frame()) {
  fn <- sys.function(-1)
  call <- match.call(definition = fn, call = sys.call(-1), expand.dots = TRUE)

  # Explicitly passed arguments
  explicit <- as.list(call)[-1]

  # All formal arguments
  formals_all <- formals(fn)

  # All actual values in the environment
  env_values <- as.list(.env)

  resolved <- mapply(function(name, default) {
    if (name %in% names(explicit)) {
      list(source = "explicit", value = explicit[[name]])
    } else if (name %in% names(env_values)) {
      list(source = "default (evaluated)", value = env_values[[name]])
    } else {
      list(source = "missing", value = default)
    }
  }, name = names(formals_all), default = formals_all, SIMPLIFY = FALSE)

  structure(resolved, class = "inspect_args")
}

