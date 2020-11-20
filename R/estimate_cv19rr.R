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
estimate_cv19rr <- function(dat, RefTests=50000, beta = NA, logIsigma = NA, logtau = NA,fix=NULL){

    if(!is.null(fix)) for(i in 1:length(fix)) assign(names(fix)[[i]],fix[[i]])
    
    obj <- setup.TMB.object(dat, RefTests=RefTests, beta = beta,
                            logIsigma = logIsigma, logtau = logtau)

    fit <- fit.TMB.object(obj)

    rv <- c(list(dat=dat, obj=obj), fit)
    class(rv) <- "cv19rr"

    return(rv)
}
