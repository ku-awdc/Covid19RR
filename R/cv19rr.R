#' @name cv19rr
#' @title S3 methods for the cv19rr class
#'
#' @param x An object of class cv19rr
#' @param object An object of class cv19rr
#' @param r2R function to convert daily growth rates to reproduction number
#' @param tests Number of tests to normalise against
#' @param lag Lag parameter
#' @param caption_date Date to use in the caption
#' @param rib_col Colour for the ribbon
#' @param x_by Increment for the date on the x axis
#' @param xlim Limits of the x axis (date) - if the second element is blank then the caption date is used as the maximum extent of the range
#' @param y_marks Marks to show on the y axis
#' @param ylim Limits for the y axis
#' @param page Which page of plots to display (1, 2, 3 or multiple)
#' @param row.names Unused arguments provided for compatibility with S3 methods
#' @param optional Unused arguments provided for compatibility with S3 methods
#' @param ... Unused arguments provided for compatibility with S3 methods
#'
#' @examples
#' library("Covid19RR")
#' dat <- download_data()
#' mod <- estimate_cv19rr(dat, silent=TRUE)
#' require("ggplot2")
#' # Replicate figure 12.1:
#' autoplot(mod, date = "2020-10-25",
#'     y_marks = c(0, 0.5, 1, 1.5, 2), ylim = c(0, 2.5))
#' ggsave("figur 12.1.pdf", height=7, width=11)
#' # Replicate figure 12.2:
#' autoplot(mod, caption_date = "2020-10-25", xlim = c("2020-02-23", NA),
#'     y_marks = c(0, 1, 2), ylim = c(0, 3))
#' ggsave("figur 12.2.pdf", height=7, width=11)
#'
#' # To produce graphs from a data frame:
#' mydf <- as.data.frame(mod)
#' Covid19RR:::autoplot.cv19rr(mydf)

#' @rdname cv19rr
#' @export
print.cv19rr <- function(x, ...){
	cat("TODO: print method")
}

#' @rdname cv19rr
#' @export
plot.cv19rr <- function(x, r2R = function(r) 1+4.7*r,tests=50000, lag=7, page = 3,
                        main = "Kontakttal R",...){
	settings <- list(tests=tests, lag=lag, beta=x$beta, beta_sd=x$beta_sd,r2R=r2R,main=main)
	plot_fit(dat=x$dat, opt=x$opt, settings=settings, page=page, ...)
}

#' @import ggplot2
#' @rdname cv19rr
#' @export
autoplot.cv19rr <- function(object, lag=7, caption_date = Sys.Date(), rib_col = "grey85", x_by=5, xlim = c("2020-03-24", NA), y_marks = waiver(), ylim = c(NA,NA), ...){

	if(!inherits(object, "cv19rr") && inherits(object, "data.frame")){
		df <- object
		if(gen_time != 4.7 || lag != 7){
			warning("Arguments gen_time and lag are ignored when object is a data frame")
		}
	}else{
		df <- as.data.frame(object, lag=lag)
	}
	if(!all(c("Date", "R", "LCI", "UCI") %in% names(df))){
		stop("Invalid data frame: columns for Date, R, LCI and UCI must be present")
	}

	if(is.na(xlim[2])) xlim[2] <- as.character(caption_date)
	xlim <- as.Date(xlim)
	stopifnot(all(!is.na(xlim)))
	xat <- seq(xlim[1], xlim[2], by=x_by)
	xlab <- tolower(strftime(xat, format="%b %d"))

	pt <- ggplot(df, aes_string(x="Date", y="R", ymin="LCI", ymax="UCI")) +
		geom_ribbon(col=rib_col, fill=rib_col) +
		geom_hline(yintercept=1, lty='dotted') +
		geom_hline(yintercept=0) +
		geom_line(lwd=0.25) +
		theme_bw() +
		scale_x_date(breaks=xat, labels=xlab, limits=xlim, expand=expansion()) +
		scale_y_continuous(limits=ylim, breaks=y_marks, minor_breaks=NULL, expand=expansion()) +
		theme(panel.grid.major.x=element_blank(),
					panel.grid.minor.x=element_blank(),
					axis.text.y = element_text(face="bold"),
					axis.text.x = element_text(angle=45, hjust=1, vjust=0.5, face="bold", margin=margin(b=10)),
					panel.border = element_blank(),
					axis.ticks = element_blank(),
					plot.caption = element_text(face="plain", size=7)) +
		labs(x=NULL, y="Kontakttal (Rt)", caption=strftime(as.Date(caption_date), "%d-%m-%Y"))

	return(pt)

}


#' @rdname cv19rr
#' @export
as.data.frame.cv19rr <- function(x, row.names, optional, gen_time=4.7, lag=7, r2R = function(r,tau=4.7)1+tau*r, ...){

	if(!all(c("dat", "opt", "beta", "beta_sd") %in% names(x))){
		stop("Invalid cv19rr object")
	}

	rv <- data.frame(Date = x$dat$Date[-1]-lag,
                         R = r2R(x$opt$est$r, gen_time),
                         UCI = r2R(x$opt$est$r+x$opt$sd$r, gen_time),
                         LCI = r2R(x$opt$est$r-x$opt$sd$r, gen_time)
                         )

	return(rv)

}

