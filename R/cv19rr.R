#' @name cv19rr
#' @title S3 methods for the cv19rr class
#'
#' @param x An object of class cv19rr
#' @param object An object of class cv19rr
#' @param pngfile An optional pngfile to use
#' @param ... Other arguments provided for compatibility with S3 methods

#' @rdname cv19rr
#' @export
print.cv19rr <- function(x, ...){
	cat("TODO: print method")
}

#' @rdname cv19rr
#' @export
plot.cv19rr <- function(x, pngfile=NULL, ...){
	plot_fit(x$dat, x$opt)
}

#' @importFrom ggplot2 autoplot
#' @rdname cv19rr
#' @export
autoplot.cv19rr <- function(object, ...){
	cat("TODO: autoplot method")
}

