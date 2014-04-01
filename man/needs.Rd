\name{needs_session}
\alias{needs}
\alias{needs_session}
\alias{print.needs}
\title{Resolve dependencies for the running session}
\usage{
needs_session(which = c("Depends", "Imports", "LinkingTo"))

needs(pkg, which = c("Depends", "Imports", "LinkingTo"))

\method{print}{needs}(x, ...)
}
\arguments{
  \item{pkg}{character; a package name (currently
  ignored).}

  \item{which}{logical; what dependencies should be looked
  for. See argument of same name in
  \code{\link{packageDescription}}.}

  \item{x}{an R object of class \code{\link{needs}}.}

  \item{...}{additional arguments passes to
  \code{\link{print}}.}
}
\value{
A list of class \code{"needs"} with five components
\item{basePkgs}{a character vector of the base R packages
(including recommended ones) dependencies} \item{depends}{a
data frame of packages depended upon, i.e. loaded and
attached.} \item{imports}{a data frame of packages
imported, i.e. those packages whose namespaces have been
loaded.} \item{other}{a data frame of additional
dependencies, arising from a recursive search of the
dependencies of packages listed in the \code{depends} and
\code{imports} components.} \item{r}{R version info,
currentl as returned by \code{\link{R.Version}}.}
}
\description{
Resolves the dependent packages required to reproduced the
current R session, for an installed R package, or a package
source tree/tarball.
}
\details{
TODO
}
\examples{
needs()

needs(which = "Depends")
}
\author{
Gavin L. Simpson \
}

