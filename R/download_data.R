#' Title
#'
#' @param preprocess Should the downloaded data be processed automatically?
#'
#' @return dataset
#'
#' #TODO clean up tempfile
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr
#' @importFrom utils download.file
#' @importFrom zip unzip
#' @importFrom readr read_delim locale
#'
#' @export
download_data <- function(preprocess=TRUE)
{
    url <- "https://covid19.ssi.dk/overvagningsdata/download-fil-med-overvaagningdata"
    webpage <- read_html(url)
    anchors <- html_nodes(webpage, "a")
    urls <- html_attr(anchors, "href")
    MostRecentFileNr <- grep("Data-Epidemiologisk", 
                             urls,ignore.case=TRUE)[1]
    MostRecentFile <- urls[[MostRecentFileNr]]


    td <- tempdir()
    download.file(MostRecentFile,file.path(td, "data.zip"))

    unzip(file.path(td, "data.zip"), exdir=file.path(td,"data"))
    text <- gsub(" ", "", readLines(file.path(td, "data/Test_pos_over_time.csv")))
    dat <- read_delim(paste(text, collapse='\n'), delim=";", locale=locale(decimal_mark=",", grouping_mark='.'))

    dat <- head(dat,-2)

    ## Preprocess:
    if(preprocess){
        dat <- preprocess.data(dat)
    }
    
    return(dat)
}
