## Functions in here are NOT exported to the user

# Package environment to track if the dynlib is loaded etc
cv19_private <- new.env()
assign("dynmod_loaded", FALSE, envir=cv19_private)

#' @importFrom utils head tail
preprocess.data <- function(dat)
{
	dat <- tail(dat,-60)  ## Skip first 60 days since testing activity is rubbish
	dat <- head(dat,-2)   ## Skip last two days because data isn't there yet

	dat$Date <- as.Date(dat$Date)
	return(dat)
}


#' @importFrom TMB MakeADFun sdreport
setup.TMB.object <- function(dat, RefTests=50000, beta = NA, logIsigma = NA, logtau = NA)
{
	data <- list(nTests = dat$NotPrevPos,
                     nPos = dat$NewPositive,
                     modelswitch = 2,
                     RefTests = RefTests)

	defaults <- c(
            beta = 1,
            logIsigma = -8,
            logtau = log(10),
            logrzeta = 0
	)

        
	parameters <- c(beta=beta, logIsigma = logIsigma, logtau = logtau,logrzeta=NA)

	fixed <- as.factor(NA)

	map <-  lapply(as.list(parameters[!is.na(parameters)]),function(x)x[1]<-fixed)

        parameters[is.na(parameters)] <- defaults[is.na(parameters)]

	if(!cv19_private$dynmod_loaded){
		ss <- try( library.dynam("Covid19RR", "Covid19RR", .libPaths()) )
		if(inherits(ss, "try-error")) stop("An error occured when loading the DLL")
		cv19_private$dynmod_loaded <- TRUE
	}

        parameters <- c(list(logI=numeric(nrow(dat)),r=numeric(nrow(dat)-1)),as.list(parameters))
        
	obj <- MakeADFun(data, parameters, DLL="Covid19RR",
                         map = map,
                         random=c("logI","r"),
                         silent = TRUE)

	return(obj)
}


#' @importFrom nloptr nloptr
fit.TMB.object <- function(obj, silent=TRUE)
{
	opts <- list(algorithm="NLOPT_LD_AUGLAG",
                     xtol_abs=1e-12,
                     maxeval=2E+4,
                     print_level=if(silent) 0 else 3,
                     local_opts= list(algorithm="NLOPT_LD_AUGLAG_EQ",xtol_rel=1e-4))

        lb <- c(beta=0,logIsigma=-10,logtau=log(1),logrzeta=-10)
        ub <- c(beta=1,logIsigma=0,logtau=log(100),logrzeta=10)

	fix.indeces <- !(names(lb) %in% names(obj$par))

        fn <- function(p) obj$fn(p)
        gr <- function(p) obj$gr(p)

	opt <- nloptr(obj$par,fn,gr,lb=lb[!fix.indeces],ub=ub[!fix.indeces],opts=opts)

	names(opt$solution) <- names(obj$par[!fix.indeces])
	rep <- sdreport(obj)

	est <- as.list(rep,"Est")
	sd <- as.list(rep,"Std")

	return(list(opt=opt,est=est,sd=sd))
}

