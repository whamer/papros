##' List files in CDC FTP folder
##' @title List files in CDC FTP folder
##' @param path the path to be explored
##' @return a vector with files stored in specific path
##' @import dplyr
##' @importFrom magrittr %>%
##' @importFrom magrittr %<>%
##' @importFrom RCurl getURL
##' @author Wolfgang Hamer
##' @examples
##'
##' list_files_in_CDC_folder("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/air_temperature/historical/")
list_files_in_CDC_folder <- function(path){
  filenames <- RCurl::getURL(path,
                             ftp.use.epsv = TRUE,
                             verbose=TRUE,
                             dirlistonly = TRUE) %>%
    gsub("zip","zipqwert", .) %>%
    strsplit(., "qwert")
  filenames <- substring(filenames[[1]],3)
  filenames[1] <- paste("st",filenames[1],sep="")
  #Sys.sleep(sample(10:15,1))
  return(filenames)
}

