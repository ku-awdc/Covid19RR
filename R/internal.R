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
setup.TMB.object <- function(dat, parameters=NULL, silent=FALSE)
{
	data <- list(nTests = dat$NotPrevPos,
							 nPos = dat$NewPositive,
							 modelswitch = 2)

	setup <- list(
		beta = 1,
		logIsigma = -8,
		logtau = log(10),
		logrzeta = log(1)
	)

	if(!is.null(parameters)){
		if(any(!names(parameters) %in% names(setup))){
			warning("The following invalid parameter names were ignored: ", names(parameters)[!names(parameters) %in% names(setup)])
			parameters <- parameters[names(parameters) %in% names(setup)]
		}
		stopifnot(all(names(parameters) %in% names(setup)))

		setup[names(setup) %in% names(parameters)] <- NULL
		setup <- c(setup, parameters)
	}
	parameters <- c(list(logI = numeric(nrow(dat)),
										 r = numeric(nrow(dat)-1)
										 ), setup)

	fixed <- as.factor(NA)
	map <- list(logrzeta=fixed)

	if(!cv19_private$dynmod_loaded){
		ss <- try( library.dynam("Covid19RR", "Covid19RR", .libPaths()) )
		if(inherits(ss, "try-error")) stop("An error occured when loading the DLL")
		cv19_private$dynmod_loaded <- TRUE
	}

	obj <- MakeADFun(data, parameters, DLL="Covid19RR",
									 map = map,
									 random=c("logI","r"),
									 silent = silent)

	return(obj)
}


#' @importFrom nloptr nloptr
fit <- function(obj, fix=NULL, silent=FALSE)
{
	opts <- list(algorithm="NLOPT_LD_AUGLAG",
							 xtol_abs=1e-12,
							 maxeval=2E+4,
							 print_level=if(silent) 0 else 3,
							 local_opts= list(algorithm="NLOPT_LD_AUGLAG_EQ",xtol_rel=1e-4))

	lb <- c(0,-10,log(1))
	ub <- c(1,0,log(100))

	fix.indeces <- names(obj$par) %in% names(fix)
	obj$par[fix.indeces] <- fix[names(fix) %in% names(obj$par)]
	if(any(!names(fix) %in% names(obj$par))){
		warning("The following invalid fix names were ignored: ", names(fix)[!names(fix) %in% names(obj$par)])
	}

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

	opt <- nloptr(par,fn,gr,lb=lb[!fix.indeces],ub=ub[!fix.indeces],opts=opts)
	names(opt$solution) <- names(obj$par[!fix.indeces])
	rep <- sdreport(obj)

	est <- as.list(rep,"Est")
	sd <- as.list(rep,"Std")

	repest <- as.list(rep,"Est",report=TRUE)
	repsd <- as.list(rep,"Std",report=TRUE)

	return(list(opt=opt,est=est,sd=sd,repest=repest,repsd=repsd))
}

## TODO: 4.7 is the generation time. Is this the right conversion?
r2R <- function(r, gen_time) gen_time*r+1
