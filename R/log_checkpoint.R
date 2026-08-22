#' Save a named logging checkpoint
#'
#' Saves the current named `piplog` with a stage-specific identifier and
#' checkpoint metadata. The wrapper keeps checkpoint naming consistent across
#' DLW and pipeline stages while forwarding safe additional `log_save()` options.
#'
#' @param name Name of the in-memory log to save (default: `"default"`).
#' @param stage Checkpoint stage. Must be exactly `"dlw"` or `"pipeline"`.
#' @param alias Optional stamp alias used for persistence. Defaults to the
#'   stamp package default alias.
#' @param metadata Optional named list of custom checkpoint metadata. The
#'   canonical `stage` and `checkpoint_time` fields are always supplied by this
#'   function.
#' @param code Optional code object to include in the checkpoint code hash.
#' @param ... Additional named arguments passed to [log_save()] and then to
#'   `stamp::st_save()`.
#'
#' @return Invisibly returns the result from [log_save()].
#' @export
log_save_checkpoint <- function(
    name = getOption("pipfun.log.default", "default"),
    stage = "dlw",
    alias = NULL,
    metadata = list(),
    code = NULL,
    ...
) {
  if (!rlang::is_string(name) || is.na(name) || !nzchar(name)) {
    cli::cli_abort("{.arg name} must be one non-empty log name.")
  }

  if (fs::is_absolute_path(name) || grepl("[/\\\\]", name) ||
      grepl(":", name, fixed = TRUE)) {
    cli::cli_abort(
      "{.arg name} must be a log name, not a path."
    )
  }

  if (name %in% c(".", "..")) {
    cli::cli_abort("{.arg name} cannot be a path component.")
  }

  valid_stages <- c("dlw", "pipeline")
  if (!rlang::is_string(stage) || is.na(stage) ||
      !(stage %in% valid_stages)) {
    cli::cli_abort(
      "{.arg stage} must be exactly one of: {paste(valid_stages, collapse = ', ')}."
    )
  }

  if (!is.list(metadata) ||
      (length(metadata) > 0L && is.null(names(metadata)))) {
    cli::cli_abort("{.arg metadata} must be a named list.")
  }

  metadata_names <- names(metadata)
  if (length(metadata_names) > 0L &&
      (anyNA(metadata_names) || any(!nzchar(metadata_names)) ||
        anyDuplicated(metadata_names) > 0L)) {
    cli::cli_abort("{.arg metadata} must have unique, non-empty names.")
  }
  reserved_metadata <- intersect(
    metadata_names,
    c("stage", "checkpoint_time")
  )
  if (length(reserved_metadata) > 0L) {
    cli::cli_abort(
      "Canonical checkpoint metadata cannot be overridden: {paste(reserved_metadata, collapse = ', ')}."
    )
  }

  dots <- list(...)
  dot_names <- names(dots)
  if (length(dots) > 0L &&
      (is.null(dot_names) || anyNA(dot_names) || any(!nzchar(dot_names)))) {
    cli::cli_abort("Additional checkpoint arguments must be named.")
  }
  reserved <- intersect(
    dot_names,
    c("name", "id", "alias", "stage", "metadata", "code")
  )
  if (length(reserved) > 0L) {
    cli::cli_abort(
      "Reserved checkpoint argument(s) cannot be forwarded: {paste(reserved, collapse = ', ')}."
    )
  }

  id <- paste0(name, "_checkpoint_", stage)
  checkpoint_time <- Sys.time()
  checkpoint_code <- c(
    checkpoint_time = format(
      checkpoint_time,
      format = "%Y-%m-%dT%H:%M:%OS6Z",
      tz = "UTC"
    ),
    user_code = paste(deparse(code), collapse = "\n")
  )

  do.call(
    log_save,
    c(
      list(
        name = name,
        id = id,
        alias = alias,
        metadata = c(
          metadata,
          list(
            stage = stage,
            checkpoint_time = checkpoint_time
          )
        ),
        # The changing code hash forces stamp to persist repeated checkpoints
        # whose log content has not changed.
        code = checkpoint_code
      ),
      dots
    )
  )
}
