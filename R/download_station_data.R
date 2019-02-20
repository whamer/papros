##' Downloads hourly data of the DWD stations in Germany
##' @title Download hourly weather data of the DWD stations
##' @param station the station ID
##' @param parameter one ore multiple paramters of c("temperature", "humidity", "precipitation", "windspeed", "winddirection")
##' @param time either "recent" often updated data or "historical" data which go longer in the past
##' @param astbl default = FALSE; should the explort be a dataframe or a tibble
##' @return a p value of the comparison between the selected and random points
##' @import dplyr sp
##' @importFrom magrittr %>%
##' @importFrom magrittr %<>%
##' @export download_hourly_station_data
##' @author Wolfgang Hamer
##' @examples
##'
##' # Select Location
##' mapview::mapview(get_all_dwd_locations(TRUE))
##'
##' Fehmarn <- download_hourly_station_data(5516)
##' head(Fehmarn)
##'
##' LeuchtturmKiel <- download_hourly_station_data(02961, parameter = "windspeed")
##' head(LeuchtturmKiel)
download_hourly_station_data <- function(station,parameter=c("temperature","precipitation","windspeed"),time="recent",astbl=FALSE){

  station <- formatC(station, width = 5, format = "d", flag = "0")

  comb <- c()

  if(any(is.element(parameter,c("temperature","humidity")))){
    path0 <- paste0("ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/",time,"/")
    message(paste("Start list files temperature/humidity data for station ID:",station))
    allf <- list_files_in_CDC_folder(path0)
    fitfile <- allf[grep(pattern=paste0("_",station,"_"), x = allf)]

    if(length(fitfile)!=0){
      path <- paste0(path0,fitfile)
      message(paste("Start download temperature/humidity data for station ID:",station))
      tmp <- tempfile()
      trye <- try(download.file(path,tmp),silent=TRUE)
      if(class(trye)!="try-error"){
        message(paste("Start unzip files temperature/humidity data for station ID:",station))
        temp <- read.csv2(unz(tmp, unzip(tmp, list = T)$Name[grep("produkt_",unzip(tmp, list = T)$Name)])) %>%
          dplyr::select(-one_of("QN_9","eor")) %>%
          dplyr::rename(Temperature = TT_TU,
                        Humidity = RF_TU) %>%
          dplyr::mutate(Temperature = as.numeric(as.character(Temperature)),
                        Humidity = as.numeric(as.character(Humidity))) %>%
          dplyr::mutate(Temperature=replace(Temperature, Temperature==-999, NA),
                        Humidity=replace(Humidity, Humidity==-999, NA))
        unlink(tmp)
        comb <- c(comb,"temp")
      }else{
        message(paste("No temperature/humidity data for station ID:",station))
      }
    }else{
      message(paste("No temperature/humidity data for station ID:",station))
    }
  }

  if(any(is.element(parameter,c("precipitation")))){
    path0 <- paste0("ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/precipitation/",time,"/")
    message(paste("Start list files precipitation data for station ID:",station))
    allf <- list_files_in_CDC_folder(path0)
    fitfile <- allf[grep(pattern=paste0("_",station,"_"), x = allf)]

    if(length(fitfile)!=0){
      path <- paste0(path0,fitfile)
      message(paste("Start download precipitation data for station ID:",station))
      tmp <- tempfile()
      trye <- try(download.file(path,tmp),silent=TRUE)
      if(class(trye)!="try-error"){
        message(paste("Start unzip precipitation data for station ID:",station))
        prec <- read.csv2(unz(tmp, unzip(tmp, list = T)$Name[grep("produkt_",unzip(tmp, list = T)$Name)])) %>%
          dplyr::select(-one_of("QN_8","RS_IND", "WRTR","eor")) %>%
          dplyr::rename(Precipitation = R1) %>%
          dplyr::mutate(Precipitation = as.numeric(as.character(Precipitation))) %>%
          dplyr::mutate(Precipitation=replace(Precipitation, Precipitation==-999, NA))
        unlink(tmp)
        comb <- c(comb,"prec")
      }else{
        message(paste("No precipitation data for station ID:",station))
      }
    }else{
      message(paste("No precipitation data for station ID:",station))
    }
  }


  if(any(is.element(parameter,c("windspeed","winddirection")))){
    path0 <- paste0("ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/wind/",time,"/")
    message(paste("Start list files wind data for station ID:",station))
    allf <- list_files_in_CDC_folder(path0)
    fitfile <- allf[grep(pattern=paste0("_",station,"_"), x = allf)]

    if(length(fitfile)!=0){
      path <- paste0(path0,fitfile)
      message(paste("Start download wind data for station ID:",station))
      tmp <- tempfile()
      trye <- try(download.file(path,tmp),silent=TRUE)
      if(class(trye)!="try-error"){
        message(paste("Start unzip wind data for station ID:",station))
        wind <- read.csv2(unz(tmp, unzip(tmp, list = T)$Name[grep("produkt_",unzip(tmp, list = T)$Name)])) %>%
          dplyr::select(-one_of("QN_3","eor")) %>%
          dplyr::rename(WindSpeed = F,
                        WindDirection = D) %>%
          dplyr::mutate(WindSpeed = as.numeric(as.character(WindSpeed)),
                        WindDirection = as.numeric(as.character(WindDirection))) %>%
          dplyr::mutate(WindSpeed=replace(WindSpeed, WindSpeed==-999, NA),
                        WindDirection=replace(WindDirection, WindDirection==-999, NA))
        unlink(tmp)
        comb <- c(comb,"wind")
      }else{
        message(paste("No wind data for station ID:",station))
      }
    }else{
      message(paste("No wind data for station ID:",station))
    }
  }

  if(length(comb)==0){
    message(paste("No data for station ID:",station))
  }else{
    if(length(comb) == 1){
      co2 <- get(comb[1])
    }
    if(length(comb) > 1){
      co2 <- merge(get(comb[1]),
                   get(comb[2]),
                   by = c("STATIONS_ID", "MESS_DATUM"),
                   all = TRUE)
    }
    if(length(comb) == 3){
      co2 <- merge(co2,
                   get(comb[3]),
                   by = c("STATIONS_ID", "MESS_DATUM"),
                   all = TRUE)
    }
    co2 %<>% dplyr::rename(ID = STATIONS_ID,
                           DateTime = MESS_DATUM)
    if(astbl == TRUE){co2 %<>% as.tbl(.)}
    return(co2)
  }
}



##' Downloads historical and recent hourly data of the DWD stations in Germany
##' @title Download alltime hourly weather data of the DWD stations
##' @param station the station ID
##' @param parameter one ore multiple paramters of c("temperature", "humidity", "precipitation", "windspeed", "winddirection")
##' @param astbl default = FALSE; should the explort be a dataframe or a tibble
##' @return a p value of the comparison between the selected and random points
##' @import dplyr sp
##' @importFrom magrittr %>%
##' @importFrom magrittr %<>%
##' @export download_alltime_hourly_station_data
##' @author Wolfgang Hamer
##' @examples
##'
##' # Select Location
##' mapview::mapview(get_all_dwd_locations(TRUE))
##'
##' Fehmarn <- download_alltime_hourly_station_data(5516)
##' head(Fehmarn)
##'
##' LeuchtturmKiel <- download_alltime_hourly_station_data(02961, parameter = "windspeed")
##' head(LeuchtturmKiel)
download_alltime_hourly_station_data <- function(station,parameter=c("temperature","precipitation","windspeed"),astbl=FALSE){
  rec <- download_hourly_station_data(station = station,
                                      parameter = parameter,
                                      time = "recent")

  his <- download_hourly_station_data(station = station,
                                      parameter = parameter,
                                      time = "historical")
  allS <-  rbind(rec,his) %>%
    dplyr::distinct()

  if(astbl == TRUE){allS %<>% as.tbl(.)}
  return(allS)
}



##' Downloads hourly data of the DWD stations in one federal state in Germany
##' @title Download hourly weather data of the DWD stations of federal states
##' @param state the Federal State (e.g. "Schleswig-Holstein")
##' @param parameter one ore multiple paramters of c("temperature","humidity","precipitation","windspeed","winddirection")
##' @param time either "recent" often updated data or "historical" data which go longer in the past
##' @param coord default = FALSE; should the explort contain coordinates or not
##' @param savefile default = FALSE; where should the file be saved as .csv file?
##' @return a p value of the comparison between the selected and random points
##' @import dplyr sp
##' @importFrom magrittr %>%
##' @importFrom magrittr %<>%
##' @export download_statewide_hourly_station_data
##' @author Wolfgang Hamer
##' @examples
##'
##' shdat <- download_statewide_hourly_station_data(state = "Schleswig-Holstein", coord = TRUE)
##' head(shdat)
download_statewide_hourly_station_data <- function(state,parameter=c("temperature","precipitation","windspeed"),time="recent",coord = FALSE, savefile=FALSE){
  comb <- c()

  if(any(is.element(parameter,c("temperature","humidity")))){
    tempS <- get_dwd_locations(sp = FALSE,parameter="temperature") %>%
      dplyr::filter(Bundesland %in% state)
    stations <- unique(tempS$Stations_id)

    path0 <- paste0("ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/",time,"/")
    message(paste("Start list files temperature/humidity data for station ID:",stations))
    allf <- list_files_in_CDC_folder(path0)
    fitfile <- allf[grep(pattern=paste(paste0("_",stations,"_"),collapse="|"), x = allf)]
    fitfile <- fitfile[nchar(fitfile)<100]

    if(length(fitfile)!=0){
      tempfiles <- Map(function(fitfile){
        path <- paste0(path0,fitfile)
        message(paste("Start download temperature/humidity data for:",fitfile))
        tmp <- tempfile()
        trye <- try(download.file(path,tmp),silent=TRUE)
        if(class(trye)!="try-error"){
          message(paste("Start unzip files temperature/humidity data for:",fitfile))
          temp <- read.csv2(unz(tmp, unzip(tmp, list = T)$Name[grep("produkt_",unzip(tmp, list = T)$Name)])) %>%
            dplyr::select(-dplyr::one_of("QN_9","eor")) %>%
            dplyr::rename(Temperature = TT_TU,
                          Humidity = RF_TU) %>%
            dplyr::mutate(Temperature = as.numeric(as.character(Temperature)),
                          Humidity = as.numeric(as.character(Humidity))) %>%
            dplyr::mutate(Temperature=replace(Temperature, Temperature==-999, NA),
                          Humidity=replace(Humidity, Humidity==-999, NA))
          unlink(tmp)
          comb <- c(comb,"temp")
        }else{
          message(paste("No temperature/humidity data for:",fitfile))
        }
        return(temp)
      },
      fitfile = fitfile)
      temp <- do.call(rbind.data.frame, tempfiles)
      comb <- c(comb,"temp")
    }else{
      message(paste("No temperature/humidity data for station ID:",station))
    }
  }

  if(any(is.element(parameter,c("precipitation")))){
    precS <- get_dwd_locations(sp = FALSE,parameter="precipitation") %>%
      dplyr::filter(Bundesland %in% state)
    stations <- unique(precS$Stations_id)

    path0 <- paste0("ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/precipitation/",time,"/")
    message(paste("Start list files precipitation data for station ID:",stations))
    allf <- list_files_in_CDC_folder(path0)
    fitfile <- allf[grep(pattern=paste(paste0("_",stations,"_"),collapse="|"), x = allf)]
    fitfile <- fitfile[nchar(fitfile)<100]

    if(length(fitfile)!=0){
      precfiles <- Map(function(fitfile){
        path <- paste0(path0,fitfile)
        message(paste("Start download precipitation data for:",fitfile))
        tmp <- tempfile()
        trye <- try(download.file(path,tmp),silent=TRUE)
        if(class(trye)!="try-error"){
          message(paste("Start unzip files precipitation data for:",fitfile))
          prec <- read.csv2(unz(tmp, unzip(tmp, list = T)$Name[grep("produkt_",unzip(tmp, list = T)$Name)])) %>%
            dplyr::select(-dplyr::one_of("QN_8","RS_IND", "WRTR","eor")) %>%
            dplyr::rename(Precipitation = R1) %>%
            dplyr::mutate(Precipitation = as.numeric(as.character(Precipitation))) %>%
            dplyr::mutate(Precipitation=replace(Precipitation, Precipitation==-999, NA))
          unlink(tmp)

        }else{
          message(paste("No precipitation data for:",fitfile))
        }
        return(prec)
      },
      fitfile = fitfile)
      prec <- do.call(rbind.data.frame, precfiles)
      comb <- c(comb,"prec")
    }else{
      message(paste("No precipitation data for station ID:",station))
    }
  }


  if(any(is.element(parameter,c("windspeed","winddirection")))){
    windS <- get_dwd_locations(sp = FALSE,parameter="wind") %>%
      dplyr::filter(Bundesland %in% state)
    stations <- unique(windS$Stations_id)

    path0 <- paste0("ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/wind/",time,"/")
    message(paste("Start list files wind data for station ID:",stations))
    allf <- list_files_in_CDC_folder(path0)
    fitfile <- allf[grep(pattern=paste(paste0("_",stations,"_"),collapse="|"), x = allf)]
    fitfile <- fitfile[nchar(fitfile)<100]

    if(length(fitfile)!=0){
      windfiles <- Map(function(fitfile){
        path <- paste0(path0,fitfile)
        message(paste("Start download wind data for:",fitfile))
        tmp <- tempfile()
        trye <- try(download.file(path,tmp),silent=TRUE)
        if(class(trye)!="try-error"){
          message(paste("Start unzip files wind data for:",fitfile))
          wind <- read.csv2(unz(tmp, unzip(tmp, list = T)$Name[grep("produkt_",unzip(tmp, list = T)$Name)])) %>%
            dplyr::select(-dplyr::one_of("QN_3","eor")) %>%
            dplyr::rename(WindSpeed = F,
                          WindDirection = D) %>%
            dplyr::mutate(WindSpeed = as.numeric(as.character(WindSpeed)),
                          WindDirection = as.numeric(as.character(WindDirection))) %>%
            dplyr::mutate(WindSpeed=replace(WindSpeed, WindSpeed==-999, NA),
                          WindDirection=replace(WindDirection, WindDirection==-999, NA))
          unlink(tmp)

        }else{
          message(paste("No wind data for:",fitfile))
        }
        return(wind)
      },
      fitfile = fitfile)
      wind <- do.call(rbind.data.frame, windfiles)
      comb <- c(comb,"wind")
    }else{
      message(paste("No wind data for station ID:",station))
    }
  }


  if(length(comb)==0){
    message(paste("No data for station ID:",stations))
  }else{
    if(length(comb) == 1){
      co2 <- get(comb[1])
    }
    if(length(comb) > 1){
      co2 <- merge(get(comb[1]),
                   get(comb[2]),
                   by = c("STATIONS_ID", "MESS_DATUM"),
                   all = TRUE)
    }
    if(length(comb) == 3){
      co2 <- merge(co2,
                   get(comb[3]),
                   by = c("STATIONS_ID", "MESS_DATUM"),
                   all = TRUE)
    }
    co2 %<>% dplyr::rename(ID = STATIONS_ID,
                           DateTime = MESS_DATUM)

    if(coord){
      allS <- get_all_dwd_locations() %>%
        dplyr::filter(Bundesland %in% state)

      allSm <- allS[,c("Stations_id","geoBreite", "geoLaenge")] %>%
        dplyr::mutate(Stations_id = as.numeric(Stations_id)) %>%
        dplyr::rename(lat = geoBreite,
                      lon = geoLaenge) %>%
        dplyr::distinct()

      co2 <- merge(co2,allSm,
                   by.x = "ID",
                   by.y = "Stations_id",
                   all.x = TRUE)
    }
    return(co2)
  }

  if(!is.logical(savefile)){
    dire <- try(paste0(strsplit(savefile,"/")[[1]][1:(length(strsplit(savefile,"/")[[1]])-1)],collapse = "/"),silent=TRUE)
    if(class(dire)!="try-error"){
      if(!dir.exists(dire)){dir.create(dire)}
      write.csv2(co2, savefile, row.names=FALSE)
    }
  }
}
