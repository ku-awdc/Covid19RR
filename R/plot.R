#' @importFrom graphics plot par lines matplot polygon grid
#' @importFrom grDevices png dev.off
plot_fit <- function(dat,opt,settings,ylim,pngfile,page)
{
	if(1 %in% page){
		par(mfrow=c(2,2))
		plot(dat$Date,opt$est$logI)
		plot(dat$Date[-1],opt$est$logr*settings$growth+1)
		plot(dat$Date,opt$repest$Epos)
		lines(dat$Date,dat$NewPositive)
		matplot(dat$Date,cbind(dat$NewPositive,exp(opt$est$logI),dat$NotPrevPos^opt$solution["beta"]),log="y",type="l")
	}

	(opt$objective)

	my.poly <- function(x1,y1,x2=NULL,y2=NULL,...)
	{
		if(is.null(x2)) x2 <- x1
		if(is.null(y2)) y2 <- numeric(length(y1))
		polygon(c(x1,rev(x2)),c(y1,rev(y2)),...)
	}

	if(!is.null(pngfile)) png(filename="Inc-R.png",width=840)

	if(2 %in% page){
		par(mfrow=c(1,2))
		cl <- opt$est$logI - opt$sd$logI
		cu <- opt$est$logI + opt$sd$logI

		faktor <- settings$tests^opt$opt$solution["beta"]
		plot(dat$Date,exp(cl),type="n",ylim=faktor*exp(range(c(cl,cu))),xlab="Dato",ylab="",log="",
				 main=paste0("Antal positive ved ", format(settings$tests, big.mark=".", decimal.mark=","), " daglige tests"))
		my.poly(dat$Date,faktor*exp(cl),y2=faktor*exp(cu),col="grey")
		lines(dat$Date,faktor*exp(opt$est$logI))
		grid()

		r2R <- function(r) settings$growth*r+1
		plot(dat$Date[-1]-settings$lag,r2R(opt$est$logr),xlab="Dato",ylab="",main="Kontakttal R",type="n",ylim=ylim)
		cu <- opt$est$logr+opt$sd$logr
		cl <- opt$est$logr-opt$sd$logr
		my.poly(dat$Date[-1],r2R(cl),y2=r2R(cu),col="grey")
		lines(dat$Date[-1],r2R(opt$est$logr))
		grid()
	}

	if(!is.null(pngfile)) dev.off()
}
