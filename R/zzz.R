.onAttach <- function(lib, pkg) {
    cat("Loading compiled code...\n")
    library.dynam("Covid19RR", pkg, lib)
}
