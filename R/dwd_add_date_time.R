##' This functions replaces DateTime column by Date (date) and Time (numeric) columns
##' @title Replaces DateTime column by Date (date) and Time (numeric) columns
##' @param dataframe a dataframe
##' @param columnname default = "DateTime"; should contain values in the format "2017072602" for 2 oclock at the 26 th of Julya in 2017
##' @return a dataframe like dataframe with two new columns
##' @import dplyr sp
##' @importFrom magrittr %>%
##' @importFrom magrittr %<>%
##' @export dwd_add_date_time
##' @author Wolfgang Hamer
##' @examples
##'
##' locs <- get_dwd_locations(sp = TRUE)
##' mapview::mapview(locs)
dwd_add_date_time <- function(dataframe, columnname = "DateTime"){
  dataframe2 <- dataframe %>%
    dplyr::mutate(Date = as.Date(substr(DateTime,1,8),format ="%Y%m%d"),
                  Time = as.numeric(substr(DateTime,9,10)))
  return(dataframe2)
}


