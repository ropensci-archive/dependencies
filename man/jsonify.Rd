\name{jsonify}
\alias{jsonify}
\title{Convert a dependency object to JSON}
\usage{
jsonify(x, file = NULL, ...)
}
\arguments{
  \item{x}{an object of class \code{\link{needs}}.}

  \item{file}{character; a file name to write to.}

  \item{...}{arguments passed to \code{\link{toJSON}}.}
}
\value{
the jsonified representation of the dependency object.
}
\description{
Create a JSON representation of a dependency object.
}
\author{
Gavin L. Simpson
}

