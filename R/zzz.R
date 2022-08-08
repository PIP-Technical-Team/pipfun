

pipfun_default_options <- list(
  pipfun.verbose  = TRUE
)

.onLoad <- function(libname, pkgname) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Options --------

  op    <- options()
  toset <- !(names(pipfun_default_options) %in% names(op))
  if (any(toset)) options(pipfun_default_options[toset])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## defined values --------

  invisible()
}

