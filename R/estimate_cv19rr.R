#' Estimation of RR
#'
#' @details
#' Intended to have a similar interface to \code{\link[EpiEstim]{estimate_R}}
#'
#' @param dat Incidence data
#' @param parameters Option to override the default parameter values
#' @param fix Option to override the default fixed values
#' @param silent Option to suppress messages during fitting
#' @param ... Other arguments provided for compatibility with \code{\link[EpiEstim]{estimate_R}} but currently ignored
#'
#' @return An object of class \link{cv19rr}
#'
#' @examples
#' dat <- download_data()
#'
#' @export
estimate_cv19rr <- function(dat, parameters=NULL, fix=NULL, silent=FALSE, RefTests = 50000,...){

	## TODO: better argument checks

	if(!is.null(parameters) && !is.list(parameters)) stop("Invalid parameters argument (must be a list)")
	if(!is.null(fix) && !is.numeric(fix)) stop("Invalid fix argument (must be a numeric vector)")

	dots <- list(...)
	if(length(dots)>0) warning("Unused argument ignored")

	obj <- setup.TMB.object(dat, parameters=parameters, silent=silent, RefTests=RefTests)
	opt <- fit(obj, fix=fix, silent=silent)

	# If beta was fixed:
	if("beta" %in% names(fix)){
		beta <- fix["beta"]
		beta_sd <- 0
	# Otherwise:
	}else{
		beta <- opt$opt$solution["beta"]
		beta_sd <- opt$sd$beta
	}

	rv <- list(obj=obj, dat=dat, opt=opt, beta=beta, beta_sd=beta_sd)
	class(rv) <- "cv19rr"

	return(rv)
}
