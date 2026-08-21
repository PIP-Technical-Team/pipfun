#' Save a named logging checkpoint
#'
#' Saves the current named `piplog` with a stage-specific identifier and
#' checkpoint metadata. The wrapper keeps checkpoint naming consistent across
#' DLW and pipeline stages while forwarding additional `log_save()` options.
#'
#' @param name Name of the in-memory log to save.
#' @param stage Checkpoint stage: `"dlw"` or `"pipeline"`.
#' @param alias Stamp alias used for persistence.
#' @param ... Additional arguments passed to [log_save()].
#'
#' @return Invisibly returns the result from [log_save()].
#' @export
log_save_checkpoint <- function(
    name = getOption("pipfun.log.default"),
    stage = c("dlw", "pipeline"),
    alias = "log_checkpoint",
    ...
) {
  stage <- match.arg(stage)
  id <- paste0(name, "_checkpoint_", stage)

  log_save(
    name = name,
    id = id,
    alias = alias,
    metadata = list(
      stage = stage,
      checkpoint_time = Sys.time()
    ),
    ...
  )
}
