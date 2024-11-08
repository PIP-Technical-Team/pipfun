

pipfun_default_options <- list(
  pipfun.verbose     = TRUE,
  pipfun.ghowner     = "PIP-Technical-Team",
  pipfun.ppps        = c(2011, 2017),
  pipfun.working_dir = "PIP_ingestion_pipeline_v2"
)

.onLoad <- function(libname, pkgname) {

  # make sure .pipenv is exported properly-----

  if (!exists(".pipenv", envir = asNamespace(pkgname))) {
    assign(".pipenv", new.env(parent = emptyenv()), envir = asNamespace(pkgname))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Options --------

  op    <- options()
  toset <- !(names(pipfun_default_options) %in% names(op))
  if (any(toset)) options(pipfun_default_options[toset])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## defined values --------

  invisible()
}

