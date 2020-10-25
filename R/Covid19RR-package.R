#' @keywords internal
#' @examples
#' ## Using the interface:
#' dat <- download_data()
#' mod <- estimate_cv19rr(dat, silent=TRUE)
#' plot(mod)
#' # Or with non-default parameters or fix:
#' mod <- estimate_cv19rr(dat, parameters=list(beta=0.7))
#' plot(mod)
#' mod <- estimate_cv19rr(dat, fix=c(beta=0.7))
#' plot(mod)
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
