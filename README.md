# Covid19RR
Estimation of reproduction rates for the COVID-19 pandemic in Denmark based on publicly available data

## Installation

To compile and install this package, first download and install R from https://cran.r-project.org - then run the following code:

install.packages("remotes")
remotes::install_github("ku-awdc/Covid19RR")

Alternatively, you can install the latest stable build from source (or perhaps using a pre-built binary, depending on your platform and R version) using:

install.packages('Covid19RR', repos=c(CRAN="https://cran.rstudio.com/", "ku-awdc"="https://ku-awdc.github.io/drat/"))

The source code for the underlying model can be found under the src/ directory - note that this depends on the TMB package for building, so cannot be built outside an R package.  

## TODO

- Improve documentation
- Write tests
