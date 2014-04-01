`readDescription` <- function(path) {
    ## read DESCRIPTION from a source package
    desc <- read.dcf(file = path)
    desc
}

##' @title Dependency resolution for installed packages
##'
##' @description Utility function to capture
##'
##' @details TO DO
##'
##' @param pkg character; a package name.
##' @param recursive logical; include indirect dependencies?
##' @param local logical; look locally to resolve dependencies, not
##' online?
##' @param reduce logical; collapse dependencies to a minimum required
##' version should different minimum versions of the same packge be
##' resolved as dependencies of \code{pkg}.
##' @param lib.loc Which package libraries to use when searching for
##' installed packages. The default \code{NULL} uses all libraries in
##' \code{.libPaths()}.
##' @param which character; the level of dependencies to return. Not
##' sure where these levels are defined..?
##' @return TODO
##'
##' @author Gavin L. Simpson based almost completely on \code{tools::pkgDepends},
##' \code{tools:::getDepMtrx}, and \code{tools:::getRemotePkgDepends} in R.
##'
##' @rdname packageDependencies
`packageDependencies` <- function (pkg, recursive = TRUE, local = TRUE, reduce = TRUE,
                                   lib.loc = NULL,
                                   which = c("Depends","Imports","Suggests")) {
    ## copy of tools::pkgDepends that calls local versions of un-exported
    ## helpers from tools, modified to change the depLevel arguments
    which <- match.arg(which)
    if (length(pkg) != 1L)
        stop("argument 'pkg' must be of length 1")
    instPkgs <- utils::installed.packages(lib.loc = lib.loc)
    depMtrx <- getDependencyMatrix(pkg, instPkgs, local, which = which)
    if (is.null(depMtrx))
        stop(gettextf("package '%s' was not found", pkg), domain = NA)
    tools::getDepList(depMtrx, instPkgs, recursive, local, reduce, lib.loc)
}

##' @param instPkgs A matrix specifying all packages installed locally, as
##' returned by \code{installed.packages}.
##'
##' @rdname packageDependencies
##'
`getDependencyMatrix` <- function(pkg, instPkgs, local = TRUE, which)  {
    ## copy of tools:::getDepMtrx but allows you to specify
    ## the type of dependencies in package.dependencies
    row <- match(pkg, instPkgs[, "Package"])
    if (!is.na(row))
        pkgDeps <- tools::package.dependencies(instPkgs[row, ], depLevel = which)[[1L]]
    else {
        if (local)
            pkgDeps <- NULL
        else pkgDeps <- getRemoteDependencies(pkg)
    }
    pkgDeps
}

##' @param contriburl URLs for the \code{contrib} sections of CRAN, or
##' CRAN-like repositories. The default is to take this from the
##' \code{repos} option.
##'
##' @rdname packageDependencies
##'
`getRemoteDependencies` <- function (pkg, contriburl = getOption("repos"), which) {
    ## copy of tools:::getRemotePkgDepends but allows you to specify
    ## the type of dependencies in package.dependencies
    if (is.null(contriburl))
        contriburl <- utils::contrib.url(getOption("repos"))
    cran <- utils::available.packages(contriburl = contriburl)
    whichRow <- which(pkg == cran[, "Package"])
    if (length(whichRow)) {
        return(tools::package.dependencies(cran[whichRow, ], depLevel = which)[[1L]])
    }
    else NULL
}
