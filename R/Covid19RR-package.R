#' @keywords internal
#' @useDynLib Covid19RR
#' @examples
#' ## Using the interface:
#' dat <- download_data()
#' mod <- estimate_cv19rr(dat)
#' # Or with non-default parameters or fix:
#' mod <- estimate_cv19rr(dat, parameters=list(beta=1), fix=c(logrsigma=-6))
#' plot(mod)
#' save.image()
#'
#' ## Old version using internal (now non-exported) code:
#' obj <- Covid19RR:::setup.TMB.object(dat)
#' opt <- Covid19RR:::fit(obj,fix=c(logrsigma=-6))
#' Covid19RR:::plot_fit(dat,opt)
#' save.image()
#'
#' # Or to launch the shiny whatsit:
#' \dontrun{
#' launch_shiny("RRest")
#' }
#'
#' # Or to read the vignette:
#' \dontrun{
#' vignette("Covid19RR")
#' }

"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
