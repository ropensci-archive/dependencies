##' @importFrom tools package_dependencies
##' @importFrom utils installed.packages
##'
##' @rdname needs
##'
`needs_session` <- function(which = c("Depends", "Imports", "LinkingTo")) {

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
    ## Packages attached that are not base - i.e. Depends:
    if (any(!basePkgs)) {
        otherPkgs <- pkgDesc[!basePkgs]
        out$depends <- as.data.frame(unclass(do.call(rbind,
                                                     lapply(otherPkgs,
                                                            flattenPkgDesc))),
                                     stringsAsFactors = FALSE)
    }

    ## loaded namespaces - i.e. Imports:
    loadedOnly <- loadedNamespaces()
    loadedOnly <- loadedOnly[!(loadedOnly %in% pkgs)]
    if (length(loadedOnly)) {
        names(loadedOnly) <- loadedOnly
        pkgDesc <- c(pkgDesc, lapply(loadedOnly, packageDescription))
        loadedOnly <- pkgDesc[loadedOnly]
        out$imports <- as.data.frame(unclass(do.call(rbind,
                                                     unname(lapply(loadedOnly,
                                                                   flattenPkgDesc)))),
                                     stringsAsFactors = FALSE)
        dimnames(out$imports) <- list(seq_len(nrow(out$imports)),
                                      dimnames(out$imports)[[2]])
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
        out$other <- as.data.frame(unclass(do.call(rbind,
                                                   lapply(depPkgs,
                                                          flattenPkgDesc))))
    }

    out
}
