##' This function tries to automatically interpolate points
##' @title Automatically interpolates point values
##' @param sp_points SpatialPointsDataframe containing the aim variable and if "ked" should be applied the covariables
##' @param aim_variable Character string with the name of the aim variable
##' @param outputfile SpatialPointsDataframe, SpatialGridDataFrame or raster which should be filled with predictions; requires covariables for "ked" (if two values are given in a vector a raster is created with the resolution given by the values)
##' @param co_variables default = FALSE, vector of covariables if needed
##' @param co_variables default = FALSE, vector of covariables if needed
##' @param procedure default = c("ked","rfk,"ok","idw"); vector containing the interpolation technic to be used; all methods are tested and the method with the lowest RMSE is used for the interpolation; KED = Kriging With external Drift; RFK = Random Forest Kriging; OK = Ordinary Kriging; IDW = Inverse distance Weighting
##' @param show_rmse default = TRUE, should the RMSE values derived by loocv_interpolate_points be displayed?
##' @return interpolated point values as sp or raster file depending on outputfile
##' @import dplyr sp gstat
##' @importFrom raster raster
##' @importFrom raster stack
##' @importFrom raster mean
##' @importFrom raster calc
##' @importFrom raster extent
##' @importFrom raster projectRaster
##' @importFrom raster writeRaster
##' @importFrom automap autoKrige
##' @importFrom magrittr %>%
##' @importFrom magrittr %<>%
##' @import sf 
##' @export auto_interpolate_points
##' @author Wolfgang Hamer
##' @examples
##'
##' library(magrittr)
##' library(dplyr)
##' library(sp)
##' library(raster)
##' 
##' # Download example data
##' shdat <- download_statewide_hourly_station_data(state = "Schleswig-Holstein", coord = TRUE)
##' 
##' # Select data of specific Time / Date
##' da_sel <- shdat %>% filter(DateTime == sort(unique(shdat$DateTime))[5])
##' 
##' # Create spatial dataset
##' da_sel_sp <- SpatialPointsDataFrame(da_sel[,c("lon", "lat")],
##'                                     da_sel,
##'                                     proj4string = CRS("+init=epsg:4326"))
##' 
##' # Transform to projected (m based!) system
##' da_sel_sp <- spTransform(da_sel_sp, CRS("+init=epsg:25832"))
##' 
##' air_mean <- download_dwd_raster(parameter = "air_temperature_mean", period = "1961-1990", month = 17, crop=da_sel_sp)
##' 
##' da_sel_sp <- raster::extract(air_mean,da_sel_sp,sp=TRUE)
##' 
##' # Application of function for point data result
##' myintpoints <- auto_interpolate_points(sp_points = da_sel_sp,
##'                                        aim_variable = "Temperature",
##'                                        outputfile = air_mean,
##'                                        co_variables = c("air_temp_mean_1961.1990_17"))
##' plot(myintpoints)
auto_interpolate_points <- function(sp_points, aim_variable, outputfile, co_variables = FALSE, procedure = c("ked","rfk","ok","idw"),show_rmse=TRUE){
  
  locit <- loocv_interpolate_points(sp_points = sp_points, 
                                    aim_variable = aim_variable,
                                    co_variables = co_variables,
                                    procedure = procedure)
  
  rmses <- do.call(cbind,
                   Map(function(x){sqrt(mean(x$residual^2,na.rm=TRUE))},
                       x=locit))
  
  if(show_rmse){
    message(paste(colnames(rmses),":",round(rmses,4)," --- "))
  }
  
  lowestm <- colnames(rmses)[which.min(rmses)]
  lowestv <- round(as.numeric(rmses[,lowestm]),3)
  message(paste("Selected",lowestm,"with RMSE of",lowestv))
  
  myintpoints <- interpolate_points(sp_points = sp_points,
                                    aim_variable = aim_variable,
                                    outputfile = outputfile,
                                    co_variables = co_variables,
                                    procedure = lowestm)
  return(myintpoints)
}


