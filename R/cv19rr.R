#' @name cv19rr
#' @title S3 methods for the cv19rr class
#'
#' @param x An object of class cv19rr
#' @param object An object of class cv19rr
#' @param r2R function to convert daily growth rates to reproduction number
#' @param tests Number of tests to normalise against
#' @param lag Lag parameter
#' @param main The plot title
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

#' @importFrom graphics plot par lines matplot polygon grid text
#' @importFrom grDevices png dev.off

#' @rdname cv19rr
#' @export
plot.cv19rr <- function(x, page = c(1,2),
                        main = "Kontakttal R",...){
    df <- as.data.frame(x,...)

    dat <- x$dat

    my.poly <- function(x1,y1,x2=NULL,y2=NULL,...)
    {
        if(is.null(x2)) x2 <- x1
        if(is.null(y2)) y2 <- numeric(length(y1))
        polygon(c(x1,rev(x2)),c(y1,rev(y2)),...)
    }

    par(mfrow=c(1,length(page)))
    
    ## Plot of corrected number of positive tests
    if(1 %in% page)
    {
        tyl <- range(c(df$CorrPos.LCI,df$CorrPos.UCI))
        plot(dat$Date,df$CorrPos,type="n",ylim=tyl,xlab="Dato",ylab="",log="",
             main=paste0("Antal positive ved ",
                         format(x$obj$data$RefTests, big.mark=".", decimal.mark=","), " daglige tests"))
        my.poly(df$Date,df$CorrPos.LCI,y2=df$CorrPos.UCI,col="grey")
        lines(df$Date,df$CorrPos)
        grid()
        betahat <- round(x$beta,digits=2)
        betacl <- round(x$beta - 2*x$beta_sd,digits=2)
        betacu <- round(x$beta + 2*x$beta_sd,digits=2)
        text(mean(dat$Date),max(df$CorrPos.UCI),bquote(beta == .(betahat)~(.(betacl)*","*.(betacu))))
    }
    
    ## If we want page number 2 of plots:
    ## Plot of reproduction number
    if(2 %in% page){
        cu <- df$R.UCI
        cl <- df$R.LCI

        ylim <- range(c(cl,cu),na.rm=TRUE)

        plot(df$Date,df$R,xlab="Dato",ylab="",main=main,type="n",ylim=ylim)
        my.poly(df$Date,cl,y2=cu,col="grey")
        lines(df$Date,df$R)
        grid()
    }
}

#' @import ggplot2
#' @rdname cv19rr
#' @export
autoplot.cv19rr <- function(object, lag=7, caption_date = Sys.Date(), rib_col = "grey85", x_by=5, xlim = c("2020-03-24", NA), y_marks = waiver(), ylim = c(NA,NA), ...){

	if(!inherits(object, "cv19rr") && inherits(object, "data.frame")){
		df <- object
		if(lag != 7){
			warning("The lag argument is ignored when object is a data frame")
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
as.data.frame.cv19rr <- function(x, row.names=NULL, optional=FALSE, lag=7, r2R = function(r)1+4.7*r, ...){

    if(!all(c("dat", "opt", "beta", "beta_sd") %in% names(x))){
        stop("Invalid cv19rr object")
    }
    
    rv <- data.frame(Date = x$dat$Date-lag,
                     CorrPos = exp(x$opt$est$logI + x$beta * log(x$obj$env$data$RefTests)),
                     CorrPos.LCI = exp(- x$opt$sd$logI + x$opt$est$logI + x$beta * log(x$obj$env$data$RefTests)),
                     CorrPos.UCI = exp(+ x$opt$sd$logI + x$opt$est$logI + x$beta * log(x$obj$env$data$RefTests)),
                     I = exp(x$opt$est$logI),
                     I.LCI = exp(x$opt$est$logI - x$opt$sd$logI),
                     I.UCI = exp(x$opt$est$logI + x$opt$sd$logI),
                     R = c(NA,r2R(x$opt$est$r)),
                     R.UCI = c(NA,r2R(x$opt$est$r+x$opt$sd$r)),
                     R.LCI = c(NA,r2R(x$opt$est$r-x$opt$sd$r))
                     )

    return(rv)
}

