#' @keywords internal
#' @useDynLib Covid19RR
#' @examples
#' dat <- preprocess.data(download.data())
#' obj <- setup.TMB.object(dat)
#' opt <- fit(obj,fix=c(logrsigma=-6))
#' plot_fit(dat,opt)
#' save.image()
#'
#' # Or to launch the shiny whatsit:
#' \dontrun{
#' launch_shiny("RRest")
#' }

"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
