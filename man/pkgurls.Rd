\name{pkgurls}
\alias{pkgurls}
\title{Create package URIs given package name and version}
\usage{
pkgurls(input)
}
\arguments{
  \item{input}{A data.frame with two columns; see details.}
}
\value{
A vector of urls of length equal to rows in the input
data.frame
}
\description{
Create package URIs given package name and version
}
\details{
Input data.frame should have two columns \itemize{ \item
pkg character; a package name.  \item ver character; a
package version. }
}
\examples{
dat <- data.frame(pkg=c('vegan','ggplot2','stringr'), ver=c('2.0-10','0.9.3.1','0.6.2'))
pkgurls(input=dat)
}

