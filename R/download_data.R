#' Title
#'
#' @param preprocess Should the downloaded data be processed automatically?
#' @param dir Name of directory to download to. If NULL, which is the default, a temporary directory is created and used
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
download_data <- function(preprocess=TRUE,dir=NULL)
{
    url <- "https://covid19.ssi.dk/overvagningsdata/download-fil-med-overvaagningdata"
    webpage <- read_html(url)
    anchors <- html_nodes(webpage, "a")
    urls <- html_attr(anchors, "href")
    MostRecentFileNr <- grep("Data-Epidemiologisk", 
                             urls,ignore.case=TRUE)[1]
    MostRecentFile <- urls[[MostRecentFileNr]]


    if(is.null(dir))
    {
        td <- tempdir()
    } else {
        td <- dir
        if(!file.exists(td)) dir.create(td)
    }

    ## TODO : On at least one windows machine, the flag mode="wb" must be added - this maybe sets write permissions?
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
