\name{packageDependencies}
\alias{getDependencyMatrix}
\alias{getRemoteDependencies}
\alias{packageDependencies}
\title{Dependency resolution for installed packages}
\usage{
packageDependencies(pkg, recursive = TRUE, local = TRUE, reduce = TRUE,
  lib.loc = NULL, which = c("Depends", "Imports", "Suggests"))

getDependencyMatrix(pkg, instPkgs, local = TRUE, which)

getRemoteDependencies(pkg, contriburl = getOption("repos"), which)
}
\arguments{
  \item{pkg}{character; a package name.}

  \item{recursive}{logical; include indirect dependencies?}

  \item{local}{logical; look locally to resolve
  dependencies, not online?}

  \item{reduce}{logical; collapse dependencies to a minimum
  required version should different minimum versions of the
  same packge be resolved as dependencies of \code{pkg}.}

  \item{lib.loc}{Which package libraries to use when
  searching for installed packages. The default \code{NULL}
  uses all libraries in \code{.libPaths()}.}

  \item{which}{character; the level of dependencies to
  return. Not sure where these levels are defined..?}

  \item{instPkgs}{A matrix specifying all packages
  installed locally, as returned by
  \code{installed.packages}.}

  \item{contriburl}{URLs for the \code{contrib} sections of
  CRAN, or CRAN-like repositories. The default is to take
  this from the \code{repos} option.}
}
\value{
TODO
}
\description{
Utility function to capture
}
\details{
TO DO
}
\author{
Gavin L. Simpson based almost completely on
\code{tools::pkgDepends}, \code{tools:::getDepMtrx}, and
\code{tools:::getRemotePkgDepends} in R.
}

