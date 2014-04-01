##' @title Resolve dependencies for the running session
##'
##' @description Resolves the dependent packages required to reproduced
##' the current R session, for an installed R package, or a package
##' source tree/tarball.
##'
##' @details TODO
##' @param pkg character; a package name (currently ignored).
##' @param which logical; what dependencies should be looked for. See
##' argument of same name in \code{\link{packageDescription}}.
##'
##' @return A list of class \code{"needs"} with five components
##' \item{basePkgs}{a character vector of the base R packages (including
##' recommended ones) dependencies}
##' \item{depends}{a data frame of packages depended upon, i.e. loaded
##' and attached.}
##' \item{imports}{a data frame of packages imported, i.e. those
##' packages whose namespaces have been loaded.}
##' \item{other}{a data frame of additional dependencies, arising from
##' a recursive search of the dependencies of packages listed in the
##' \code{depends} and \code{imports} components.}
##' \item{r}{R version info, currentl as returned by \code{\link{R.Version}}.}
##'
##' @author Gavin L. Simpson
##'\
##' @export
##'
##' @rdname needs
##'
##' @examples
##' needs()
##'
##' needs(which = "Depends")
`needs` <- function(pkg, which = c("Depends", "Imports", "LinkingTo")) {

    if (missing(pkg) || is.null(pkg)) {
        out <- needs_session(which = which)
    }

    ## R Version
    out$r <- R.Version()

    class(out) <- "needs"
    out
}

##' @param x an R object of class \code{\link{needs}}.
##' @param ... additional arguments passes to \code{\link{print}}.
##'
##' @rdname needs
##'
##' @S3method print needs
##' @method print needs
##'
`print.needs` <- function(x, ...) {
    writeLines("Session Dependencies")
    writeLines("====================")
    writeLines("\nR Version")
    writeLines("=========")
    writeLines(x$r$version.string)
    writeLines("\nDepends")
    writeLines("=======")
    print(x$depends, ...)
    writeLines("\nImports")
    writeLines("=======")
    print(x$imports, ...)
    writeLines("\nOther Dependencies")
    writeLines("==================")
    print(x$other, ...)
}
