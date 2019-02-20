##' This functions downloads DWD location data from the CDC Server
##' @title Download DWD location data from the CDC Server
##' @param sp default = FALSE; if TRUE returns not the plain data frame but a spatialised version
##' @param parameter default = "temperature"; should the "temperature" (and humidity), "precipitation" or "wind" station network be downloaded
##' @return a dataframe or a SpatialPointsDataFrame containing information about DWD locations in Germany
##' @import dplyr sp
##' @importFrom magrittr %>%
##' @importFrom magrittr %<>%
##' @author Wolfgang Hamer
##' @examples
##'
##' locs <- get_dwd_locations(sp = TRUE)
##' mapview::mapview(locs)
get_dwd_locations <- function(sp = FALSE,parameter="temperature"){
  tmp <- tempfile()
  stations <- ifelse(parameter == "temperature",
                     download.file("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/air_temperature/historical/TU_Stundenwerte_Beschreibung_Stationen.txt",tmp),
                     ifelse(parameter == "precipitation",
                            download.file("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/precipitation/recent/RR_Stundenwerte_Beschreibung_Stationen.txt",tmp),
                            download.file("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/wind/recent/FF_Stundenwerte_Beschreibung_Stationen.txt",tmp)))
  avail_loc <- read.table(tmp, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  unlink(tmp)

  avail_loc <- do.call(rbind.data.frame, Map(function(x){
    betw <- substring(avail_loc[x,], c(1,7,16,36,44,54,62,103), c(5,14,23,38,50,60,99,124))
    return(gsub(" ", "", betw, fixed = TRUE))},
    x = 3:nrow(avail_loc)))
  names(avail_loc) <- c("Stations_id","von_datum","bis_datum","Stationshoehe","geoBreite","geoLaenge","Stationsname","Bundesland")

  avail_loc %<>% dplyr::mutate(Stations_id = as.character(Stations_id),
                               von_datum = as.Date(as.character(von_datum),"%Y%m%d"),
                               bis_datum = as.Date(as.character(bis_datum),"%Y%m%d"),
                               Stationshoehe = as.numeric(as.character(Stationshoehe)),
                               geoBreite = as.numeric(as.character(geoBreite)),
                               geoLaenge = as.numeric(as.character(geoLaenge)),
                               Stationsname = as.character(Stationsname),
                               Bundesland = as.character(Bundesland))


  if(sp == TRUE){
    return(SpatialPointsDataFrame(avail_loc[,c("geoLaenge", "geoBreite")],
                                  avail_loc,
                                  proj4string = CRS("+init=epsg:4326")))
  }else{
    return(avail_loc)
  }
}



##' This functions downloads DWD location data from the CDC Server
##' @title Download all available DWD location data from the CDC Server
##' @param sp default = FALSE; if TRUE returns not the plain data frame but a spatialised version
##' @return a dataframe or a SpatialPointsDataFrame containing information about DWD locations in Germany
##' @import dplyr sp
##' @importFrom magrittr %>%
##' @importFrom magrittr %<>%
##' @export get_all_dwd_locations
##' @author Wolfgang Hamer
##' @examples
##'
##' mapview::mapview(get_all_dwd_locations(TRUE))
get_all_dwd_locations <- function(sp = FALSE){
  temp <- get_dwd_locations(parameter="temperature") %>%
    dplyr::mutate(Network = "Temperature")
  prec <- get_dwd_locations(parameter="precipitation")%>%
    dplyr::mutate(Network = "Precipitation")
  wind <- get_dwd_locations(parameter="wind")%>%
    dplyr::mutate(Network = "Wind")

  allN <- rbind(temp,prec,wind) %>%
    dplyr::group_by(Stations_id,von_datum,bis_datum,Stationshoehe,geoBreite,geoLaenge,Stationsname,Bundesland) %>%
    dplyr::summarise(Network = paste(Network,collapse = " "))

  if(sp == TRUE){
    allN2 <- rbind(temp,prec,wind) %>%
      dplyr::select(-one_of("von_datum","bis_datum")) %>%
      dplyr::group_by(Stations_id,Stationshoehe,geoBreite,geoLaenge,Stationsname,Bundesland) %>%
      dplyr::summarise(Network = paste(Network,collapse = " "))

    return(SpatialPointsDataFrame(allN2[,c("geoLaenge", "geoBreite")],
                                  allN2,
                                  proj4string = CRS("+init=epsg:4326")))
  }else{
    return(allN)
  }
}




