#' @name cv19rr
#' @title S3 methods for the cv19rr class
#'
#' @param x An object of class cv19rr
#' @param object An object of class cv19rr
#' @param growth Growth parameter
#' @param tests Number of tests to normalise against
#' @param lag Lag parameter
#' @param ylim Limits for the y axis of the Kontakttal R plot
#' @param page Which page of plots to display (1, 2 or both)
#' @param pngfile An optional pngfile to use
#' @param ... Other arguments provided for compatibility with S3 methods

#' @rdname cv19rr
#' @export
print.cv19rr <- function(x, ...){
	cat("TODO: print method")
}

#' @rdname cv19rr
#' @export
plot.cv19rr <- function(x, growth = 4.7, tests=50000, lag=7, ylim=c(0.5, 1.5), page = 2, pngfile=NULL, ...){
	settings <- list(growth=growth, tests=tests, lag=lag)
	plot_fit(dat=x$dat, opt=x$opt, settings=settings, ylim=ylim, page=page, pngfile=pngfile, ...)
}

#' @importFrom ggplot2 autoplot
#' @rdname cv19rr
#' @export
autoplot.cv19rr <- function(object, ...){
	cat("TODO: autoplot method")
}

