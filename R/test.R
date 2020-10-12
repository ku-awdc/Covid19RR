#' Title
#'
#' @return
#' @export
#'
#' @examples
download.data <- function()
{
    url <- "https://www.ssi.dk/sygdomme-beredskab-og-forskning/sygdomsovervaagning/c/covid19-overvaagning/arkiv-med-overvaagningsdata-for-covid19"
    webpage <- xml2::read_html(url)
    anchors <- rvest::html_nodes(webpage,"a")
    urls <- rvest::html_attr(anchors,"href")
    MostRecentFileNr <- grep("Data-Epidemiologiske-Rapport",urls)[1]
    MostRecentFile <- urls[[MostRecentFileNr]]


    td <- tempdir()
    download.file(MostRecentFile,file.path(td, "data.zip"))

    library('tidyverse')
    library('zip')
    unzip(file.path(td, "data.zip"), exdir=file.path(td,"data"))
    text <- readLines(file.path(td, "data/Test_pos_over_time.csv")) %>%
        str_replace_all(" ", "")
    dat <- readr::read_delim(paste(text, collapse='\n'), delim=";", locale=readr::locale(decimal_mark=",", grouping_mark='.'))

    return(dat)

}

#' Title
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
preprocess.data <- function(dat)
{
    dat <- tail(dat,-60)  ## Skip first 60 days since testing activity is rubbish
    dat <- head(dat,-2)   ## Skip last two days because data isn't there yet


    dat$Date <- as.Date(dat$Date)
    return(dat)
}

#' Title
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
setup.TMB.object <- function(dat)
{
    data <- list(nTests = dat$NotPrevPos,
                 nPos = dat$NewPositive,
                 modelswitch = 2)

    parameters <- list(logI = numeric(nrow(dat)),
                       logr = numeric(nrow(dat)-1),
##                       resI = numeric(nrow(dat)),
##                       resr = numeric(nrow(dat)-1),
                       beta = 1,
                       logIsigma = -8,
                       logIzeta = log(1),
                       logIsigmaRes = -6,
                       logrsigma = -4,
                       logrsigmares = -5,
                       logrzeta = log(1))


    fixed <- as.factor(NA)
#    map <- c(map,list(logIzeta=fixed,logrzeta=fixed))
    map <- list(logIzeta=fixed,logrzeta=fixed)

    obj <- MakeADFun(data, parameters, DLL="Covid19RR",
                     map = map,
                     random=c("logI","logr"))  # "resI","resr"

    return(obj)
}

#' Title
#'
#' @param obj
#' @param fix
#'
#' @return
#' @export
#'
#' @examples
fit <- function(obj,fix=NULL)
{
    opts <- list(algorithm="NLOPT_LD_AUGLAG",
                 xtol_abs=1e-12,
                 maxeval=2E+4,
                 print_level=3,
                 local_opts= list(algorithm="NLOPT_LD_AUGLAG_EQ",xtol_rel=1e-4))

    lb <- c(0,rep(-10,length(obj$par)-1-length(fix)))
    ub <- c(1,rep(  0,length(obj$par)-1-length(fix)))

    fix.indeces <- names(obj$par) %in% names(fix)

    obj$par[fix.indeces] <- fix

    par <- obj$par[!fix.indeces]

    fn <- function(p)
    {
        pp <- obj$par
        pp[!fix.indeces] <- p
        return(obj$fn(pp))
    }

    gr <- function(p)
    {
        pp <- obj$par
        pp[!fix.indeces] <- p
        return(obj$gr(pp)[!fix.indeces])
    }

    opt <- nloptr(par,fn,gr,lb=lb,ub=ub,opts=opts)
    names(opt$solution) <- names(obj$par[!fix.indeces])
    rep <- sdreport(obj)

    est <- as.list(rep,"Est")
    sd <- as.list(rep,"Std")

    repest <- as.list(rep,"Est",report=TRUE)
    repsd <- as.list(rep,"Std",report=TRUE)

    return(list(opt=opt,est=est,sd=sd,repest=repest,repsd=repsd))
}

#' Title
#'
#' @param dat
#' @param opt
#' @param pngfile
#'
#' @return
#' @export
#'
#' @examples
plot_fit <- function(dat,opt,pngfile=NULL)
{
    par(mfrow=c(2,2))
    plot(dat$Date,opt$est$logI)
    ## TODO: 4.7
    plot(dat$Date[-1],opt$est$logr*4.7+1)
    plot(dat$Date,opt$repest$Epos)
    lines(dat$Date,dat$nPos)
    matplot(dat$Date,cbind(dat$nPos,exp(opt$est$logI),dat$nTests^opt$solution["beta"]),log="y",type="l")

    (opt$objective)

    my.poly <- function(x1,y1,x2=NULL,y2=NULL,...)
    {
        if(is.null(x2)) x2 <- x1
        if(is.null(y2)) y2 <- numeric(length(y1))
        polygon(c(x1,rev(x2)),c(y1,rev(y2)),...)
    }

    if(!is.null(pngfile)) png(file="Inc-R.png",width=840)

    par(mfrow=c(1,2))
    cl <- opt$est$logI - opt$sd$logI
    cu <- opt$est$logI + opt$sd$logI

    ## TODO:  50000 / 50.000 daglige is multiplication factor
    faktor <- 50000^opt$opt$solution["beta"]
    plot(dat$Date,exp(cl),type="n",ylim=faktor*exp(range(c(cl,cu))),xlab="Dato",ylab="",log="",
         main="Antal positive ved 50.000 daglige tests")
    my.poly(dat$Date,faktor*exp(cl),y2=faktor*exp(cu),col="grey")
    lines(dat$Date,faktor*exp(opt$est$logI))
    grid()

    ## TODO: 4.7 is conversion growth to RR
    ##       -7 is lag from infection to test
    r2R <- function(r) 4.7*r+1
    plot(dat$Date[-1]-7,r2R(opt$est$logr),xlab="Dat0",ylab="",main="Kontakttal R",type="n",ylim=c(0.5,1.5))
    cu <- opt$est$logr+opt$sd$logr
    cl <- opt$est$logr-opt$sd$logr
    my.poly(dat$Date[-1],r2R(cl),y2=r2R(cu),col="grey")
    lines(dat$Date[-1],r2R(opt$est$logr))
    grid()

    if(!is.null(pngfile)) dev.off()
}


