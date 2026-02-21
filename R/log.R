#' Add a log entry
#'
#' Adds a structured entry to a named log, capturing metadata such as timestamp,
#' calling function, event type, message, arguments used, optional output, and
#' more.
#'
#' This function automatically captures all arguments from the calling function,
#' including `...`. You can also manually add custom metadata using the
#' `logmeta` argument. The `logmeta` list is merged with captured `args`.
#'
#' @param event Type of event (e.g. `"error"`, `"info"`, `"warning"`).
#' @param message Description of the log entry.
#' @param name Name of the log (default: `options("pipfun.log.default")`).
#' @param args Optional list of captured arguments (default: auto-captured).
#'   If `NULL`, arguments are automatically extracted from the calling function.
#' @param logmeta Optional named list of metadata to attach. Merged with `args`
#'   in the final log entry.
#' @param output Optional result or return value to store in the log.
#' @param .trace Optional call stack or trace override. If `NULL`, uses
#'   `sys.call(-1)`.
#' @param .env Internal use. Calling environment (default:
#'   `rlang::caller_env()`).
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @examples
#' log_init("demo_log", overwrite = TRUE)
#'
#' # Automatically captures arguments from the caller:
#' my_fun <- function(x, y = 1, ...) {
#'   result <- x + y
#'   log_info("Ran my_fun", name = "demo_log", output = result)
#'   return(result)
#' }
#' my_fun(3, z = 9)
#'
#' # Add custom metadata manually:
#' log_info("Logging manually", name = "demo_log",
#'          logmeta = list(stage = "processing", user = "analyst"))
#' @export
log_add <- function(event,
                    message,
                    name    = getOption("pipfun.log.default"),
                    args    = NULL,
                    logmeta = NULL,
                    output  = NULL,
                    .trace  = NULL,
                    .env    = rlang::caller_env()) {

  # --- Auto-capture args from caller if not supplied ---

  # 1. Get the current call and environment stacks
  call_stack <- sys.calls()      # All active calls (as language objects)
  env_stack  <- sys.frames()     # All active environments
  n          <- length(call_stack) # Number of calls in the stack

  # 2. Find the index of the true calling function (skip log_* wrappers)
  target_idx <- n - 1            # Start from the immediate caller
  while (target_idx > 0 && grepl("^log_", deparse(call_stack[[target_idx]])[1])) {
    target_idx <- target_idx - 1 # Move up until not a log_* function
  }

  # 3. Get the environment of the true calling function, or fallback
  target_env <- if (target_idx > 0) {
    env_stack[[target_idx]]
  } else {
    parent.frame()
  }

  # 4. Try to get the function object of the true caller (for argument capture)
  target_fun <- if (target_idx > 0) {
    tryCatch(eval(call_stack[[target_idx]][[1]],  envir = target_env),
             error = function(e) NULL)
  } else {
    NULL
  }

  # 5. Capture arguments from the caller if not provided
  if (is.null(args)) {
    # If .env was explicitly provided (not the default), use it for argument capture
    if (!identical(.env, rlang::caller_env())) {
      # Try to get all objects in .env except hidden ones
      env_names <- ls(envir = .env, all.names = TRUE)
      # Remove hidden/internal variables (starting with ".")
      env_names <- env_names[!grepl("^\\.", env_names)]
      # Get their values from .env
      args <- mget(env_names, envir = .env, ifnotfound = vector("list", length(env_names)))
      # Short comment: Use .env directly for argument capture if provided
    } else if (!is.null(target_fun) && is.function(target_fun)) {
      # Get all formal argument names except ...
      arg_names <- names(formals(target_fun))
      arg_names <- arg_names[arg_names != "..."]
      # Get their values from the caller's environment
      args <- mget(arg_names,
                   envir = target_env,
                   ifnotfound = vector("list", length(arg_names)))

      # If ... is present, capture those as well
      if ("..." %in% names(formals(target_fun))) {
        dots <- tryCatch(evalq(list(...), envir = target_env), error = function(e) NULL)
        if (!is.null(dots)) args <- c(args, dots)
      }
      # Short comment: Use call stack for argument capture if .env is default
    } else {
      # If no valid function, just use an empty list
      args <- list()
    }
  }

  # 6. Retrieve the log object from the logging environment
  log <- rlang::env_get(.piplogenv, name)

  # 7. Extract the calling function name as a string (for log entry)
  calling_fn <- if (target_idx > 0) {
    cf <- deparse(call_stack[[target_idx]]) |> trimws() |> paste(collapse = " ")
    invisible(cf)
  } else {
    "unknown"
  }

  new_row <- data.table(
    time    = Sys.time(),
    package = rlang::env_name(.env),
    fun     = calling_fn,
    event   = tolower(event),
    message = as.character(message),
    args    = list(args),
    logmeta = list(logmeta),
    output  = list(output),
    trace   = list(if (!is.null(.trace)) .trace else sys.call(-1))
  )

  log <- rbindlist(list(log, new_row),
                   use.names = TRUE,
                   fill = TRUE)
  setattr(log, "class", c("piplog", class(log)))
  rlang::env_poke(.piplogenv, name, log)

  # future = list(level = "debug", module = "foo")

  invisible(TRUE)
}


#' Initialize a new log
#'
#' Creates a new named log as an empty `piplog` data.table to store log entries.
#' If the log already exists, it will be reset (unless `overwrite = FALSE`).
#'
#' @details
#' The log is stored in the internal `.piplogenv` environment and can be
#' retrieved with `log_get()`, filtered with `log_filter()`, saved with
#' `log_save()`, or reset with `log_reset()`.
#'
#' @param name Name of the log to create (default: `getOption("pipfun.log.default")`).
#' @param overwrite Whether to overwrite an existing log with the same name
#'   (default: `getOption("pipfun.log_init.ow")`).
#'
#' @return Invisibly returns the initialized log name as a character string.
#'
#' @examples
#' \dontrun{
#' log_init("testlog")
#' # This checks whether it already exists
#' log_init("testlog", overwrite = FALSE)
#' }
#' @export
log_init <- function(name = getOption("pipfun.log.default"),
                     overwrite = getOption("pipfun.log_init.ow")) {

  if (rlang::env_has(.piplogenv, name)) {
    if (!isTRUE(overwrite)) {
      cli::cli_abort("Log {.field {name}} already exists.
                     Use {.code overwrite = TRUE} to replace it.")
    }
  }

  log <- data.table(
    time     = as.POSIXct(character()),
    package  = character(),
    fun      = character(),
    event    = character(),
    message  = character(),
    args     = list(),
    output   = list(),
    trace    = list()
  )

  class(log) <- c("piplog", class(log))
  rlang::env_poke(.piplogenv, name, log)

  invisible(name)
}



#' Reset or delete a log from memory
#'
#' Clears a named log from the internal `.piplogenv` environment. Use this to
#' start fresh or free memory.
#'
#' @param name Name of the log to remove (default:
#'   `getOption("pipfun.log.default", "default")`).
#'
#' @return Invisibly returns `TRUE` if the log was successfully removed,
#'   `FALSE` if the log did not exist.
#'
#' @examples
#' \dontrun{
#' log_init("mylog")
#' log_reset("mylog")
#' }
#'
#' @export
log_reset <- function(name = getOption("pipfun.log.default", "default")) {
  if (!rlang::env_has(.piplogenv, name)) {
    cli::cli_alert_info("Log {.field {name}} is not present.")
    return(invisible(FALSE))
  }

  rlang::env_unbind(.piplogenv, name)
  cli::cli_alert_success("Log {.field {name}} has been reset.")
  invisible(TRUE)
}


#' Filter log entries
#'
#' Subsets a log by event type, function name, or time range. Returns a new
#' `piplog` object without modifying the original.
#'
#' @param name Name of the log (default: `getOption("pipfun.log.default")`)
#' @param event Type of event to filter (e.g., `"info"`, `"warning"`, `"error"`).
#'   Can be a character vector to match multiple event types.
#' @param fun Optional: function name(s) to filter as a character vector.
#' @param after Optional: filter entries after this datetime. Coerced to
#'   `POSIXct` if needed.
#' @param before Optional: filter entries before this datetime. Coerced to
#'   `POSIXct` if needed.
#'
#' @return A filtered `piplog` object (a `data.table` with class `piplog`).
#'   If no rows match the filters, returns an empty `piplog`.
#'
#' @examples
#' \dontrun{
#' log_filter(name = "mylog", event = "error")
#' log_filter(name = "mylog", fun = "my_function", event = c("warning", "error"))
#' }
#'
#' @export
log_filter <- function(name    = getOption("pipfun.log.default"),
                       event   = NULL,
                       fun     = NULL,
                       after   = NULL,
                       before  = NULL) {

  log <- name |>
    log_get() |>
    copy()

  setDT(log)

  # not elegant but works
  e <- event
  f <- fun

  if (!is.null(event))  {
    log <- log[event %in% e]
  }
  if (!is.null(fun)) {
    log <- log[fun %in% f]
  }
  if (!is.null(after))  {
    log <- log[time >= as.POSIXct(after)]
  }
  if (!is.null(before)) {
    log <- log[time <= as.POSIXct(before)]
  }
  setattr(log, "class", c("piplog", class(log)))
  return(log)
}


#' Get a log
#'
#' Retrieves a named log from the internal `.piplogenv` environment.
#' Restores the `piplog` class if it was dropped by data.table operations.
#'
#' @param name Name of the log (default: `getOption("pipfun.log.default")`)
#'
#' @return Invisibly returns the `piplog` object as a `data.table` with all
#'   log entries.
#'
#' @examples
#' \dontrun{
#' log_init("mylog")
#' my_log <- log_get("mylog")
#' }
#'
#' @export
log_get <- function(name    = getOption("pipfun.log.default")) {

  if (!rlang::env_has(.piplogenv, name)) {
    cli::cli_abort("Log {.field {name}} does not exist.")
  }

  log <- rlang::env_get(.piplogenv, name)

  if (!inherits(log, "piplog")) {
    # Restore class silently if it's just been dropped by DT ops
    if (is.data.table(log)) {
      setattr(log, "class", unique(c("piplog", class(log))))
    } else {
      cli::cli_abort(c(x = "Object {.field {name}} is not a valid piplog.",
                       i = "{.field {name}}'s class is {class(log)}"))
    }
  }
  invisible(log)
}

#' Save a log to disk
#'
#' Saves a log from `.piplogenv` to disk using the {stamp} package. Optionally
#' attach metadata and code hashes. If `id` lacks an extension, one is added
#' based on the `format` parameter.
#'
#' @details
#' The log is saved with metadata including its class (`"piplog"`), the log name,
#' and the save timestamp. Additional metadata can be provided via the `metadata`
#' parameter.
#'
#' @param name Name of the log in memory (default:
#'   `getOption("pipfun.log.default", "default")`).
#' @param id File identifier or path (extension optional). Defaults to `name`.
#' @param format File format (default: `"qs2"`). Used to add extension if `id`
#'   lacks one.
#' @param metadata Optional named list of metadata to attach to the saved file.
#' @param code Optional code object whose hash will be stored in metadata.
#' @param alias Optional stamp alias to select which catalog/versions to use.
#' @param ... Additional arguments forwarded to `stamp::st_save()`.
#'
#' @return Invisibly, the result returned by `stamp::st_save()`.
#'
#' @examples
#' \dontrun{
#' log_init("mylog")
#' log_save("mylog", id = "results", metadata = list(run_id = "exp_001"))
#' }
#'
#' @export
log_save <- function(
    name     = getOption("pipfun.log.default", "default"),
    id       = name,
    format   = "qs2",
    metadata = list(),
    code     = NULL,
    alias    = NULL,
    ...
) {
  # Validate log exists
  if (!rlang::env_has(.piplogenv, name)) {
    cli::cli_abort("Log {.field {name}} does not exist in memory.")
  }

  log <- rlang::env_get(.piplogenv, name)

  # Restore class if dropped by serialization
  if (is.data.table(log)) {
    setattr(log, "class", unique(c("piplog", class(log))))
  }

  if (!inherits(log, "piplog")) {
    cli::cli_abort("Object is not a valid {.cls piplog}.")
      }

  # Ensure extension is present like pipload::pip_write
  if (is.null(fs::path_ext(id)) || identical(fs::path_ext(id), "")) {
    id <- fs::path_ext_set(path = id, ext = format)
  }
  file <- id

  out <- stamp::st_save(
    x        = log,
    file     = file,
    metadata = c(
      list(
        class    = "piplog",
        log_name = name,
        saved_at = Sys.time()
      ),
      metadata
    ),
    code   = code,
    format = format,
    alias  = alias,
    ...
  )

  invisible(out)
}

#' Load a log from disk
#'
#' Loads a previously saved `piplog` from disk using the {stamp} package.
#' Validates the loaded object and restores it to `.piplogenv` with a given name.
#' If `id` lacks an extension, one is added based on the `format` parameter.
#'
#' @details
#' Use `version = "available"` to list all available versions of the log file
#' without loading it.
#'
#' @param id File identifier or path (extension optional).
#' @param name Name to assign to the log in memory (default: `id`).
#' @param version Optional version identifier or `"available"` to list versions.
#'   Passed to `stamp::st_load()`. Default loads the latest version.
#' @param format File format (default: `"qs2"`). Used to add extension if `id`
#'   lacks one.
#' @param overwrite Logical: whether to overwrite an existing log in memory
#'   with the same name (default: `FALSE`).
#' @param verbose Logical: whether to announce loading progress (default: `TRUE`).
#' @param alias Optional stamp alias to select which catalog/versions to use.
#'
#' @return The loaded `piplog` object (visibly), or if `version = "available"`,
#'   a `data.table` of available versions with a `vintage` column.
#'
#' @examples
#' \dontrun{
#' log_load("results", name = "loaded_log")
#' log_load("results", version = "available")  # List versions
#' }
#'
#' @export
log_load <- function(
    id,
    name      = id,
    version   = NULL,
    format    = "qs2",
    overwrite = FALSE,
    verbose   = TRUE,
    alias     = NULL
) {
  # Ensure extension is present like pipload::pip_read
  if (is.null(fs::path_ext(id)) || identical(fs::path_ext(id), "")) {
    id <- fs::path_ext_set(path = id, ext = format)
  }
  file <- id

  # List available versions
  if (identical(version, "available")) {
    vr <- stamp::st_versions(file, alias = alias)
    if (nrow(vr) == 0) {
      cli::cli_abort("No versions found in {.path {file}}.")
    }
    vr[, vintage := (.I - 1) * -1]
    return(vr[])
  }

   ver <- if (is.null(version)) "latest" else version

  if (verbose) {
    if (is.null(alias)) {
      cli::cli_alert_info("Loading {.path {file}} (version = {.strong {ver}})")
    } else {
      cli::cli_alert_info("Loading {.path {file}} (version = {.strong {ver}}, alias = {.val {alias}})")
    }
  }

  # Load log, forwarding alias
  log <- stamp::st_load(file, version = version, alias = alias)

  # Restore class if dropped
  if (is.data.table(log)) {
    setattr(log, "class", unique(c("piplog", class(log))))
  }

  if (!inherits(log, "piplog")) {
    cli::cli_abort("File does not contain a valid {.cls piplog} object.")
  }

  # Handle overwrite
  if (rlang::env_has(.piplogenv, name) && !isTRUE(overwrite)) {
    cli::cli_abort(
      "A log named {.field {name}} already exists in memory.
       Use {.code overwrite = TRUE} to replace it."
    )
  }

  rlang::env_poke(.piplogenv, name, log)

  log
}