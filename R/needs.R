##' @title Resolve dependencies for the running session
##'
##' @description Resolves the dependent packages required to reproduced
##' the current R session.
##'
##' @details TODO
##' @param which logical; what dependencies should be looked for. See
##' argument of same name in \code{\link{packageDescription}}.
##'
##' @return A list of class \code{"needs"}
##'
##' @author Gavin L. Simpson
##'
##' @export
##'
##' @importFrom tools package_dependencies
##'
`needs` <- function(which = c("Depends", "Imports", "LinkingTo")) {
    ## return list filled in later
    out <- list()

    ## list packages on search path to get attached packages
    pkgs <- grep("^package:", search(), value = TRUE)
    keep <- sapply(pkgs, function(x) x == "package:base" ||
                   !is.null(attr(as.environment(x), "path")))
    pkgs <- sub("^package:", "", pkgs[keep])

    ## package descriptions for attached packages
    pkgDesc <- lapply(pkgs, packageDescription, encoding = NA)

    ## base packages only
    basePkgs <- sapply(pkgDesc, function(x) !is.null(x$Priority) &&
                       x$Priority == "base")
    out$basePkgs <- pkgs[basePkgs]

    ## function to extract a consistent set of variables from a packageDescription
    ## object
    takeFun <- function(x) {
        out <- character(4)
        take <- c("Package","Version","LinkingTo","SystemRequirements")
        names(out) <- take
        out[take] <- as.character(x[take])
        out
    }

    ## Packages attached that are not base - i.e. Depends:
    if (any(!basePkgs)) {
        otherPkgs <- pkgDesc[!basePkgs]
        out$depends <- as.data.frame(unclass(do.call(rbind, lapply(otherPkgs, takeFun))),
                                     stringsAsFactors = FALSE)
    }

    ## loaded namespaces - i.e. Imports:
    loadedOnly <- loadedNamespaces()
    loadedOnly <- loadedOnly[!(loadedOnly %in% pkgs)]
    if (length(loadedOnly)) {
        names(loadedOnly) <- loadedOnly
        pkgDesc <- c(pkgDesc, lapply(loadedOnly, packageDescription))
        loadedOnly <- pkgDesc[loadedOnly]
        out$imports <- as.data.frame(unclass(do.call(rbind, unname(lapply(loadedOnly, takeFun)))),
                                     stringsAsFactors = FALSE)
        dimnames(out$imports) <- list(seq_len(nrow(out$imports)), dimnames(out$imports)[[2]])
    }

    ## Other Dependencies -- polled via List of Packages & their LinkingTo fields
    if (!(is.null(out$depends) || is.null(out$imports))) {
        pdb <- installed.packages()
        ## take all attached & loaded packages & their LinkingTo
        need <- c(out$depends[, "Package"], out$imports[, "Package"])
        need <- c(need, out$depends[, "LinkingTo"], out$imports[, "LinkingTo"])
        need <- need[!is.null(need)]
        deps <- package_dependencies(need, pdb, which = which, recursive = TRUE)
        deps <- unique(unlist(deps))
        got <-  unname(unlist(c(out$depends[, "Package"], out$imports[, "Package"])))
        deps <- deps[!deps %in% got]
        ## remove base packages
        deps <- deps[!deps %in% pkgs[basePkgs]]

        depPkgs <- lapply(deps, packageDescription)
        out$otherDependencies <- as.data.frame(unclass(do.call(rbind, lapply(depPkgs, takeFun))))
    }

    ## R Version
    out$RVersion <- R.Version()

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
    writeLines(x$RVersion$version.string)
    writeLines("\nDepends")
    writeLines("=======")
    print(x$depends, ...)
    writeLines("\nImports")
    writeLines("=======")
    print(x$imports, ...)
    writeLines("\nOther Dependencies")
    writeLines("==================")
    print(x$otherDependencies, ...)
}
