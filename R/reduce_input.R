##' This functions reduces the size of an given dataset in respect to the weather data of "infection" days relevant for an infestation "incubation" days later
##' @title Reduce the size of an given dataset
##' @param dataframe dataframe containing variables of interest which should be reduced
##' @param DateTime name of the DateTime column
##' @param infection duration of the assumed infection
##' @param incubation duration of the assumed incubation
##' @param event_dates Character string with the name of the aim variable
##' @return reduced dataframe input
##' @export reduce_input
##' @author Wolfgang Hamer
##' @examples
reduce_input <- function(dataframe, DateTime, infection, incubation, event_dates){
  rel_dates <- event_dates - incubation

  for(i in 1:length(infection)){
    rel_dates <- c(rel_dates,rel_dates+1)
  }

  rel_dates <- as.numeric(gsub("-","",unique(rel_dates)))

  return(dataframe[is.element(as.numeric(substr(dataframe[,DateTime],1,8)),rel_dates),])
}
