#' @importFrom graphics plot par lines matplot polygon grid text
#' @importFrom grDevices png dev.off
plot_fit <- function(dat,opt,settings,page)
{

	## TODO: clean up pngfile and page arguments (do we need these?)

	## If we want page number 1 of plots:
	if(1 %in% page){
		par(mfrow=c(2,2))
		plot(dat$Date,opt$est$logI)
		plot(dat$Date[-1],settings$r2R(opt$est$r))
		plot(dat$Date,opt$repest$Epos)
		lines(dat$Date,dat$NewPositive)
		matplot(dat$Date,cbind(dat$NewPositive,exp(opt$est$logI),dat$NotPrevPos^settings$beta),log="y",type="l")
	}

	my.poly <- function(x1,y1,x2=NULL,y2=NULL,...)
	{
		if(is.null(x2)) x2 <- x1
		if(is.null(y2)) y2 <- numeric(length(y1))
		polygon(c(x1,rev(x2)),c(y1,rev(y2)),...)
	}

	par(mfrow=c(1,2))
	cl <- opt$est$logI - opt$sd$logI
	cu <- opt$est$logI + opt$sd$logI

	## settings$tests is multiplication factor
	faktor <- settings$tests^settings$beta
	## TODO: if the upper CI is infinite then go with 10 * the number of tests ... sensible?
	tyl <- pmin(faktor*10, faktor*exp(range(c(cl,cu))))
	plot(dat$Date,exp(cl),type="n",ylim=tyl,xlab="Dato",ylab="",log="",
			 main=paste0("Antal positive ved ", format(settings$tests, big.mark=".", decimal.mark=","), " daglige tests"))
	my.poly(dat$Date,faktor*exp(cl),y2=faktor*exp(cu),col="grey")
	lines(dat$Date,faktor*exp(opt$est$logI))
	grid()
	betahat <- round(settings$beta,digits=2)
	betacl <- round(settings$beta - 2*settings$beta_sd,digits=2)
	betacu <- round(settings$beta + 2*settings$beta_sd,digits=2)
	text(mean(dat$Date),faktor*exp(max(cu)),bquote(beta == .(betahat)~(.(betacl)*","*.(betacu))))

	## If we want page number 2 of plots:
	if(2 %in% page){
		par(mfrow=c(1,2))
		cl <- opt$est$logI - opt$sd$logI
		cu <- opt$est$logI + opt$sd$logI

		faktor <- settings$tests^settings$beta
		plot(dat$Date,exp(cl),type="n",ylim=faktor*exp(range(c(cl,cu))),xlab="Dato",ylab="",log="",
				 main=paste0("Antal positive ved ", format(settings$tests, big.mark=".", decimal.mark=","), " daglige tests"))
		my.poly(dat$Date,faktor*exp(cl),y2=faktor*exp(cu),col="grey")
		lines(dat$Date,faktor*exp(opt$est$logI))
		grid()

		cu <- opt$est$r+opt$sd$r
		cl <- opt$est$r-opt$sd$r

                ylim <- range(c(settings$r2R(cl),settings$r2R(cu)))

		plot(dat$Date[-1]-settings$lag,settings$r2R(opt$est$r),
                     xlab="Dato",ylab="",main=settings$main,type="n")
		my.poly(dat$Date[-1],settings$r2R(cl),y2=settings$r2R(cu),col="grey")
		lines(dat$Date[-1],settings$r2R(opt$est$r))
		grid()
	}

	## If we want page number 3 of plots:
	if(3 %in% page){
            cu <- settings$r2R(opt$est$r+opt$sd$r)
            cl <- settings$r2R(opt$est$r-opt$sd$r)

            ylim <- range(c(cl,cu))

            plot(dat$Date[-1]-settings$lag,settings$r2R(opt$est$r),xlab="Dato",ylab="",main=settings$main,type="n",ylim=ylim)
            my.poly(dat$Date[-1],cl,y2=cu,col="grey")
            lines(dat$Date[-1],settings$r2R(opt$est$r))
		grid()

	}
}
