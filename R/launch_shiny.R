## COPIED DIRECTLY FROM Covid19tools on 2020-10-12

#' @title Launch an interactive Shiny app contained within this package
#'
#' @description
#' This function provides a convinient way in which to launch one of the interactive shiny apps contained within this package
#'
#' @param appname the name of an app to launch (see the details section)
#' @param ... further arguments to be passed to \code{\link[shiny]{runApp}}
#' @details
#' TODO
#' The apps that are currently available are:
#'
#' @example
#' launch_shiny('RRest')
#'
#' @importFrom shiny runApp
#'
#' @export
launch_shiny <- function(appname, ...){

  # shiny::runApp(appDir="inst/shiny/sensitivity")

	ad <- system.file("shiny", package = "Covid19RR")
	if(ad == ""){
		stop("Package directory not found - is the Covid19RR package installed?", call. = FALSE)
	}
	apa <- list.files(ad)

	if(missing(appname) || !is.character(appname) || length(appname)!=1 || is.na(appname)){
		stop("Please specify one of the following appnames to run:\n\t", paste(apa, collpase=' '), call. = FALSE)
	}
	appname <- apa[pmatch(appname, apa)]
	if(is.na(appname)){
		stop("Appname not recognised: please specify one of the following appnames to run:\n\t", paste(apa, collpase=' '), call. = FALSE)
	}

	# TODO: replace this with the app-specific package requirements from top of global.R inside requested app:
	pn <- c('shiny', 'shinydashboard', 'plotly', 'shinyWidgets')
	# Also write a test to make sure they are all listed as imports/suggests by Covid19tools

	if(!all(sapply(pn, requireNamespace, quietly=TRUE))){
		stop("Additional packages must be installed for the specified shiny app - you should run:", iptext(pn), call. = FALSE)
	}

	shiny::runApp(appDir=file.path(ad, appname), ...)

}

iptext <- function(...) paste0('\n\tinstall.packages(c(', do.call('paste', c(lapply(list(...), function(x) paste0('"', paste(x, collapse='", "'), '"')), list(sep=', '))), '))\n')
