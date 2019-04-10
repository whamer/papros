##' This functions download multi-annual DWD rasters and crops them if desired
##' @title Download multi-annual DWD rasters
##' @param parameter a character string defining the parameter to be downloaded (e.g.: "air_temperature_mean", "drought_index", "evapo_p", "frost_days", "hot_days", "ice_days", "precipitation", "snowcover_days", "soil_moist", "soil_temperature_5cm", "summer_days", "sunshine_duration", "vegetation_begin", "vegetation_end", "water_balance")
##' @param period years which are combined in the mult annual datasets (e.g.: "1961-1990", "1981-2010","1991-2010", "1992-2015")
##' @param month the month which should be downloaded (e.g.: 1,2,3,...,12 or 13 for spring (March, April, May), 14 for summer (June, July, August), ..., or 17 for the whole year)
##' @param crop Spatial Dataset of which an extent can be created which is used to crop the germany wide DWD dataset
##' @param savepath defalut = FALSE; path to folder where files should be stored
##' @return a raster dataset
##' @import   sp gstat
##' @importFrom R.utils gunzip
##' @importFrom raster raster
##' @importFrom raster stack
##' @importFrom raster mean
##' @importFrom raster calc
##' @importFrom raster extent
##' @importFrom raster projectRaster
##' @importFrom raster writeRaster
##' @importFrom RCurl getURL
##' @export download_dwd_raster
##' @author Wolfgang Hamer
##' @examples
download_dwd_raster <- function(parameter = "air_temperature_mean", period = "", month = "", crop=FALSE, savepath=FALSE){
  
  if(parameter == "windspeed"){
    resli <- download_windspeed_dwd_raster(parameter = parameter, period = period, heigth = month, crop=crop)
  }else{
    path <- paste0("ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/multi_annual/",parameter,"/")
    
    
    alldir <- RCurl::getURL(path, ftp.use.epsv = FALSE, ftplistonly = TRUE, crlf = TRUE)
    alldir2 <- paste0(path, strsplit(alldir, "\r*\n")[[1]])
    
    alldirS <- grep('.asc.gz', alldir2, value=TRUE)
    alldirS <- grep(period, alldirS, value=TRUE)
    alldirS <- grep(month, alldirS, value=TRUE)
    
    allNam <- unlist(Map(function(u){gsub(".asc.gz","",gsub("grids_germany_multi_annual_","",rev(strsplit(alldirS,"/")[[u]])[1]))},u=1:length(alldirS)))
    
    resli <- Map(function(allNami){
      tmp <- tempfile()
      trye <- try(download.file(alldirS[allNami],tmp),silent=TRUE)
      if(class(trye)!="try-error"){
        tmp2 <- tempfile()
        R.utils::gunzip(tmp, tmp2)
        resu <- raster::raster(tmp2)
        proj4string(resu)  <- CRS("+init=epsg:31467")
        unlink(tmp)
      }else{
        message(paste("No download because:",trye))
      }
      
      if(!is.logical(crop)){
        if(class(crop)=="RasterLayer"){
          crop <- raster::projectRaster(from = crop, crs = CRS("+init=epsg:31467"))
        }else if(class(crop)=="SpatialPointsDataFrame"|class(crop)=="SpatialLinesDataFrame"|class(crop)=="SpatialPolygonsDataFrame"){
          crop <- sp::spTransform(crop, CRS("+init=epsg:31467"))
        }
        crot <- try(raster::extent(crop),silent=TRUE)
        if(class(crot)!="try-error"){
          resu <- raster::crop(resu,crot)
        }
      }
      names(resu) <- allNam[allNami]
      return(resu)
    },allNami = 1:length(allNam))
  }
  
  if(!is.logical(savepath)){
    if(!dir.exists(savepath)){dir.create(savepath)}
    raster::writeRaster(raster::stack(resli), paste0(savepath,names(raster::stack(resli))), bylayer=TRUE, format='GTiff')
  }

  return(raster::stack(resli))
}

##' This functions download multi-annual windspeed DWD rasters and crops them if desired
##' @title Download multi-annual windspeed DWD rasters
##' @param parameter a character string defining the parameter to be downloaded ("e.g.: "windspeed")
##' @param period years which are combined in the mult annual datasets (e.g.: "1961-1990", "1981-2010","1991-2010", "1992-2015")
##' @param heigth the heigth of the observation (e.g. "10m")
##' @param crop Spatial Dataset of which an extent can be created which is used to crop the germany wide DWD dataset
##' @return a raster dataset
##' @import   sp gstat
##' @importFrom R.utils gunzip
##' @importFrom raster raster
##' @importFrom raster stack
##' @importFrom raster mean
##' @importFrom raster calc
##' @importFrom raster extent
##' @importFrom raster projectRaster
##' @importFrom raster writeRaster
##' @importFrom RCurl getURL
##' @author Wolfgang Hamer
##' @examples
download_windspeed_dwd_raster <- function(parameter = "windspeed", period = "", heigth = "", crop=FALSE){
  path <- paste0("ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/multi_annual//wind_parameters/resol_1000x1000/")
  
  
  alldir <- RCurl::getURL(path, ftp.use.epsv = FALSE, ftplistonly = TRUE, crlf = TRUE)
  alldir2 <- paste0(path, strsplit(alldir, "\r*\n")[[1]])
  
  alldirS <- grep('_BRD.asc', alldir2, value=TRUE)
  alldirS <- grep(period, alldirS, value=TRUE)
  alldirS <- grep(heigth, alldirS, value=TRUE)
  
  allNam <- unlist(Map(function(u){gsub(".asc","",gsub("grids_germany_multi_annual_","",rev(strsplit(alldirS,"/")[[u]])[1]))},u=1:length(alldirS)))
  
  resli <- Map(function(allNami){
    tmp <- tempfile()
    trye <- try(download.file(alldirS[allNami],tmp),silent=TRUE)
    if(class(trye)!="try-error"){
      resu <- raster::raster(tmp)
      proj4string(resu)  <- CRS("+init=epsg:31467")
    }else{
      message(paste("No download because:",trye))
    }
    if(!is.logical(crop)){
      if(class(crop)=="RasterLayer"){
        crop <- raster::projectRaster(from = crop, crs = CRS("+init=epsg:31467"))
      }else if(class(crop)=="SpatialPointsDataFrame"|class(crop)=="SpatialLinesDataFrame"|class(crop)=="SpatialPolygonsDataFrame"){
        crop <- sp::spTransform(crop, CRS("+init=epsg:31467"))
      }
      crot <- try(raster::extent(crop),silent=TRUE)
      if(class(crot)!="try-error"){
        resu <- raster::crop(resu,crot)
      }
    }
    names(resu) <- allNam[allNami]
    return(resu)
  },allNami = 1:length(allNam))
  
  return(raster::stack(resli))
}
