##' @title Convert a dependency object to JSON
##'
##' @description Create a JSON representation of a dependency object.
##'
##' @param x an object of class \code{\link{needs}}.
##' @param file character; a file name to write to.
##' @param ... arguments passed to \code{\link{toJSON}}.
##' @return the jsonified representation of the dependency object.
##' @author Gavin L. Simpson
##'
##' @export
##'
##' @importFrom jsonlite toJSON
##'
`jsonify` <- function(x, file = NULL, ...) {
    pkgs <- do.call(rbind, x[c("depends","imports","otherdependencies")])
    json <- toJSON(pkgs, ...)

    ## write JSON to a file?
    if (!is.null(file)) {
        writeLines(json, con = file)
    }

    ## return
    json
}
