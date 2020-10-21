#' @importFrom graphics plot par lines matplot polygon grid text
#' @importFrom grDevices png dev.off
plot_fit <- function(dat,opt,settings,ylim,pngfile,page)
{

	if(1 %in% page){
		par(mfrow=c(2,2))
		plot(dat$Date,opt$est$logI)
		plot(dat$Date[-1],opt$est$logr*settings$gen_time+1)
		plot(dat$Date,opt$repest$Epos)
		lines(dat$Date,dat$NewPositive)
		matplot(dat$Date,cbind(dat$NewPositive,exp(opt$est$logI),dat$NotPrevPos^opt$solution["beta"]),log="y",type="l")
	}

    ## TODO: 4.7 is the generation time. Is this the right conversion?
	gen_time <- settings$gen_time
    r2R <- function(r) gen_time*r+1


    my.poly <- function(x1,y1,x2=NULL,y2=NULL,...)
    {
        if(is.null(x2)) x2 <- x1
        if(is.null(y2)) y2 <- numeric(length(y1))
        polygon(c(x1,rev(x2)),c(y1,rev(y2)),...)
    }

    if(!is.null(pngfile)) png(filename=pngfile,width=840)

    par(mfrow=c(1,2))
    cl <- opt$est$logI - opt$sd$logI
    cu <- opt$est$logI + opt$sd$logI

    ## TODO:  50000 / 50.000 daglige is multiplication factor
    faktor <- settings$tests^opt$opt$solution["beta"]
    plot(dat$Date,exp(cl),type="n",ylim=faktor*exp(range(c(cl,cu))),xlab="Dato",ylab="",log="",
    		 main=paste0("Antal positive ved ", format(settings$tests, big.mark=".", decimal.mark=","), " daglige tests"))
    my.poly(dat$Date,faktor*exp(cl),y2=faktor*exp(cu),col="grey")
    lines(dat$Date,faktor*exp(opt$est$logI))
    grid()
    betahat <- round(opt$opt$solution["beta"],digits=2)
    betacl <- round(opt$opt$solution["beta"] - 2*opt$sd$beta,digits=2)
    betacu <- round(opt$opt$solution["beta"] + 2*opt$sd$beta,digits=2)
    text(mean(dat$Date),faktor*exp(max(cu)),bquote(beta == .(betahat)~(.(betacl)*","*.(betacu))))

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

		plot(dat$Date[-1]-settings$lag,r2R(opt$est$logr),xlab="Dato",ylab="",main="Kontakttal R",type="n",ylim=ylim)
		cu <- opt$est$logr+opt$sd$logr
		cl <- opt$est$logr-opt$sd$logr
		my.poly(dat$Date[-1],r2R(cl),y2=r2R(cu),col="grey")
		lines(dat$Date[-1],r2R(opt$est$logr))
		grid()
	}

	if(3 %in% page){

		plot(dat$Date[-1]-settings$lag,r2R(opt$est$r),xlab="Dato",ylab="",main="Kontakttal R",type="n",ylim=ylim)
    	cu <- opt$est$r+opt$sd$r
    	cl <- opt$est$r-opt$sd$r
    	my.poly(dat$Date[-1],r2R(cl),y2=r2R(cu),col="grey")
    	lines(dat$Date[-1],r2R(opt$est$r))
    	grid()

	}

    if(!is.null(pngfile)) dev.off()
}
