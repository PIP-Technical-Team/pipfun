% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_from_gh.R
\name{load_from_gh}
\alias{load_from_gh}
\title{Load Raw Auxiliary data}
\usage{
load_from_gh(
  measure,
  owner = getOption("pipfun.ghowner"),
  repo = paste0("aux_", measure),
  branch = c("DEV", "PROD", "main"),
  tag = match.arg(branch),
  filename = measure,
  ext = "csv",
  ...
)
}
\arguments{
\item{measure}{character: measure to be loaded}

\item{owner}{character: Github repo owner. Default is
\code{getOption("pipfun.ghowner")}}

\item{repo}{character: name of the repo}

\item{branch}{character: either "DEV" or "PROD". Refers to the branch that
will be used to update either the development server or production.}

\item{tag}{character: specific release to be used in the update.}

\item{filename}{character: Name of file name without the ".csv" extension.
Default is \code{measure}}

\item{ext}{character: Extension of \code{filename}. Default "csv"}

\item{...}{parameters to be passed to the loading functions depending of the
extension used}
}
\value{
dataset
}
\description{
Load Raw Auxiliary data
}
\keyword{internal}
