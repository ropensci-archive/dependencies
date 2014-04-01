\name{needs}
\alias{needs}
\alias{print.needs}
\title{Resolve dependencies for the running session}
\usage{
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
A list of class \code{"needs"}
}
\description{
Resolves the dependent packages required to reproduced the
current R session.
}
\details{
TODO
}
\author{
Gavin L. Simpson
}

