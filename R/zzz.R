

pipfun_default_options <- list(
  pipfun.verbose     = TRUE,
  pipfun.ghowner     = "PIP-Technical-Team",
  pipfun.ppps        = c(2017, 2011), # must be descending order
  pipfun.working_dir = "PIP_ingestion_pipeline_v2",
  pipfun.identities  = c("TEST", "PROD", "INT"),
  pipfun.log.auto    = TRUE,
  pipfun.log.default = "default"
)

.onLoad <- function(libname, pkgname) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Options --------

  op    <- options()
  toset <- !(names(pipfun_default_options) %in% names(op))
  if (any(toset)) options(pipfun_default_options[toset])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## defined values --------
  # Initialize default log silently
  if (isTRUE(getOption("pipfun.log.auto", default = FALSE))) {
    if (!exists("default", envir = .piplogenv)) {
      pipfun::log_init(name = getOption("pipfun.log.default"))
    }
  }


  invisible()
}

