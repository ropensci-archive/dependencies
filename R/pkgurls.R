##' @title Create package URIs given package name and version
##'
##' @param input A data.frame with two columns; see details.
##' @details Input data.frame should have two columns 
##' \itemize{
##'  \item pkg character; a package name.
##'  \item ver character; a package version.
##' }
##'
##' @return A vector of urls of length equal to rows in the input data.frame 
##'
##' @export
##'
##' @rdname pkgurls
##'
##' @examples
##' dat <- data.frame(pkg=c('vegan','ggplot2','stringr'), ver=c('2.0-10','0.9.3.1','0.6.2'))
##' pkgurls(input=dat)
`pkgurls` <- function(input) {

    if(!is(input, 'data.frame'))
      stop("Input must be of class data.frame")
    
    # force column names
    names(input) <- c('pkg','ver')
    
    construct_url <- function(x){
      sprintf('http://cran.r-project.org/src/contrib/Archive/%s/%s_%s.tar.gz', x['pkg'], x['pkg'], x['ver'])
    }
    
    vapply(apply(input, 1, as.list), construct_url, "")
    
}