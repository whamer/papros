##' This functions calculates response of relative development rate to temperature depending on crop parameters
##' (Based on \url{https://www.researchgate.net/publication/281674392_Modeling_physiology_of_crop_development_growth_and_yield})
##' @title Calculate response of relative development rate to temperature
##' @param TMP temperature in °C (vector or raster file)
##' @param TBD thermal base temperature; default = 0 for wheat
##' @param TP1D lower optimum temperature; default = 25 for wheat
##' @param TP2D upper optimum temperature; default = 28 for wheat
##' @param TCD thermal ceiling temperature; default = 40 for wheat
##' @return a vector or raster file (depending on input) with the relative development rate based on temperature
##' @export tempfun
##' @author Wolfgang Hamer
##' @examples
##' tempfun(10)
##' tempfun(c(23,12,23))
tempfun <- function(TMP,TBD=0,TP1D=25,TP2D=28,TCD=40){
  tempfun <- TMP
  tempfun[TMP<=TBD] <- 0
  tempfun[TMP>TBD&TMP<TP1D] <- (TMP[TMP>TBD&TMP<TP1D]-TBD)/(TP1D-TBD)
  tempfun[TMP>=TP1D&TMP<=TP2D] <- 1
  tempfun[TMP>TP2D&TMP<TCD] <- (TCD-TMP[TMP>TP2D&TMP<TCD])/(TCD-TP2D)
  tempfun[TMP>=TCD] <- 0
  return(tempfun)
}



##' This functions calculates the Daily Thermal Unit
##' (Based on \url{https://www.researchgate.net/publication/281674392_Modeling_physiology_of_crop_development_growth_and_yield})
##' @title Calculate Daily Thermal Unit
##' @param TMP temperature in °C (vector or raster file)
##' @param TBD thermal base temperature; default = 0 for wheat
##' @param TP1D lower optimum temperature; default = 25 for wheat
##' @param TP2D upper optimum temperature; default = 28 for wheat
##' @param TCD thermal ceiling temperature; default = 40 for wheat
##' @return a vector or raster file (depending on input) with the relative development rate based on temperature
##' @export dtu
##' @author Wolfgang Hamer
##' @examples
##' dtu(10)
##' dtu(c(23,12,23))
dtu <- function(TMP,TBD=0,TP1D=25,TP2D=28,TCD=40){
  DTU <- (TMP-TBD)*tempfun(TMP,TBD,TP1D,TP2D,TCD)
  return(DTU)
}


##' This functions calculates cumulative thermal unit using the daily thermal unit (dtu)
##' (Based on \url{https://www.researchgate.net/publication/281674392_Modeling_physiology_of_crop_development_growth_and_yield})
##' @title Calculate Cumulative Thermal Unit
##' @param TMP temperature in °C (vector or raster file)
##' @param TBD thermal base temperature; default = 0 for wheat
##' @param TP1D lower optimum temperature; default = 25 for wheat
##' @param TP2D upper optimum temperature; default = 28 for wheat
##' @param TCD thermal ceiling temperature; default = 40 for wheat
##' @return a vector or raster file (depending on input) with the relative development rate based on temperature
##' @export ctu
##' @author Wolfgang Hamer
##' @examples
##' ctu(10)
##' ctu(c(23,12,23))
ctu <- function(TMP,TBD=0,TP1D=25,TP2D=28,TCD=40){
  CTU <- cumsum(dtu(TMP,TBD,TP1D,TP2D,TCD))
  return(CTU)
}




##' This functions calculates Cumulative Thermal Units for large datasets using the daily thermal unit (dtu)
##' (Based on \url{https://www.researchgate.net/publication/281674392_Modeling_physiology_of_crop_development_growth_and_yield})
##'
##' @title Apply ctu on large dataset
##' @param dataset a dataset
##' @param temp_column name of the  temperature column
##' @param date_column name of the  date column
##' @param start_date start date of the growing plant; defalut = "10-01" for October the 10th
##' @param location_column name of the  location column; defalut = FALSE
##' @param vector default = TRUE; boolean operator defining if a dataset with additional column or only the new column should be given out
##' @param TBD thermal base temperature; default = 0 for wheat
##' @param TP1D lower optimum temperature; default = 25 for wheat
##' @param TP2D upper optimum temperature; default = 28 for wheat
##' @param TCD thermal ceiling temperature; default = 40 for wheat
##' @return a vector or raster file (depending on input) with the relative development rate based on temperature
##' @import dplyr
##' @importFrom magrittr %>%
##' @importFrom magrittr %<>%
##' @export large_ctu
##' @author Wolfgang Hamer
##' @examples
##' # Download example data
##' shdat <- download_statewide_hourly_station_data(state = "Schleswig-Holstein", coord = TRUE)
##' shdat2 <- shdat %>% filter(DateTime > 1995093023)
##'
##' shdat2 %<>% dplyr::mutate(Date = as.Date(substr(DateTime,1,8),"%Y%m%d"))
##'
##' shdat2 %<>% dplyr::mutate( CTU = large_ctu(dataset = shdat2,
##'                                            temp_column = "Temperature",
##'                                            date_column = "Date",
##'                                            start_date = "10-01",
##'                                            location_column = "ID"))
large_ctu <- function(dataset, temp_column, date_column, start_date = "10-01", location_column = FALSE, vector=TRUE, TBD=0,TP1D=25,TP2D=28,TCD=40){
  names(dataset)[which(names(dataset)== temp_column)] <- "Temperature"
  names(dataset)[which(names(dataset)== date_column)] <- "Date"


  if(!is.logical(location_column)){
    names(dataset)[which(names(dataset)== location_column)] <- "Location"
    dataseto <- dataset[!is.na(dataset$Temperature),]
    un <- unique(dataseto$Location)
  }else{
    dataseto <- dataset[!is.na(dataset$Temperature),]
    un <- FALSE
  }




  dataset4 <- do.call("rbind",Map(function(unc){
    if(!is.logical(location_column)){
      dataset2 <- dataseto[dataseto$Location==un[unc],]
    }else{
      dataset2 <- dataseto
    }


    dataset2 %<>% group_by(Location, Date) %>%
      summarise(Av_Temp = mean(Temperature))

    startrow <- nrow(dataset2)

    dataset2 %<>% arrange(Date) %>%
      filter(between(row_number(), min(which(format(Date, "%m-%d")==start_date)),
                     dim(dataset2)[1]))

    #yea <- unique(format(dataset2$Date, "%Y"))

    availy <- dataset2$Date[which(format(dataset2$Date, "%m-%d")==start_date)]

    dataset3 <- do.call("rbind",Map(function(siy){
      dataset2 %>%
        filter(Date %in% one_year(siy)) %>%
        mutate(CTU = ctu(TMP = Av_Temp,TBD,TP1D,TP2D,TCD))},
      siy = availy))

    if((startrow-nrow(dataset3))>0){
      warning(paste(startrow-nrow(dataset3),"days gone missing due to lack of startpoints for location", un[unc]))
    }
    return(dataset3)},
    unc = 1:length(un)))

  retdat <- merge(dataset, dataset4[,c("Location","Date","CTU")], by=c("Location","Date"),all.x = TRUE)

  if(vector){
    return(retdat$CTU)
  }else{
    names(retdat)[which(names(retdat)== "Temperature")] <- temp_column
    names(retdat)[which(names(retdat)== "Date")] <- date_column

    if(!is.logical(location_column)){
      names(retdat)[which(names(retdat)== "Location")] <- location_column
    }
    return(retdat)
  }
}






##' This function gives the sequence of one year following the date given
##' @title Give out one year
##' @param fromdate name of the  temperature column
##' @return a vector of one year following the date given
##' @export one_year
##' @author Wolfgang Hamer
##' @examples
##' one_year(as.Date("1996-10-01","%Y-%m-%d"))
one_year <- function(fromdate){
  seq(from = fromdate,
      to = as.Date(paste0(as.numeric(format(fromdate, "%Y"))+1,"-10-01"),"%Y-%m-%d")-1,
      by = 1)
}
