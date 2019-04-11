##' This function applies a leave-one-out-cross-validation on given spatial data
##' @title LOOCV of given point values
##' @param sp_points SpatialPointsDataframe containing the aim variable and if "ked" should be applied the covariables
##' @param aim_variable Character string with the name of the aim variable
##' @param co_variables default = FALSE, vector of covariables if needed
##' @param procedure default = c("ked","rfk,"ok","idw"); vector containing the interpolation technic to be used; the first method is used and if this does not work out, the second, and so on; KED = Kriging With external Drift; RFK = Random Forest Kriging; OK = Ordinary Kriging; IDW = Inverse distance Weighting
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
##' @export loocv_interpolate_points
##' @author Wolfgang Hamer
loocv_interpolate_points <- function(sp_points, aim_variable, co_variables = FALSE, procedure = c("ked","rfk","ok","idw")){
  sp_points <- sp_points[!is.na(sp_points@data[,aim_variable]),]
  
  for(i in 1:length(procedure)){
    
    if(any(procedure=="ked")){
      ked <- Map(function(xyv){interpolate_points(sp_points = sp_points[-xyv,], 
                                  aim_variable = aim_variable,
                                  outputfile = sp_points[xyv,],
                                  co_variables = co_variables, 
                                  procedure = c("ked"))},
                   xyv = 1:nrow(sp_points))
      
      isn <- is.na(ked)
      if(any(isn)){
        ked <- do.call("rbind",ked[-which(isn)])
        ked@data$observed <- sp_points@data[!(isn),aim_variable]
      }else{
        ked <- do.call("rbind",ked)
        ked@data$observed <- sp_points@data[,aim_variable]
      }
      ked@data$residual <- ked@data$observed - ked@data$var1.pred
    }
    
    if(any(procedure=="rfk")){
      rfk <- Map(function(xyv){interpolate_points(sp_points = sp_points[-xyv,], 
                                                  aim_variable = aim_variable,
                                                  outputfile = sp_points[xyv,],
                                                  co_variables = co_variables, 
                                                  procedure = c("rfk"))},
                 xyv = 1:nrow(sp_points))
      
      isn <- is.na(rfk)
      if(any(isn)){
        rfk <- do.call("rbind",rfk[-which(isn)])
        rfk@data$observed <- sp_points@data[!(isn),aim_variable]
      }else{
        rfk <- do.call("rbind",rfk)
        rfk@data$observed <- sp_points@data[,aim_variable]
      }
      rfk@data$residual <- rfk@data$observed - rfk@data$var1.pred
    }
    
    if(any(procedure=="ok")){
      ok <- Map(function(xyv){interpolate_points(sp_points = sp_points[-xyv,], 
                                                  aim_variable = aim_variable,
                                                  outputfile = sp_points[xyv,],
                                                  co_variables = co_variables, 
                                                  procedure = c("ok"))},
                 xyv = 1:nrow(sp_points))
      
      isn <- is.na(ok)
      if(any(isn)){
        ok <- do.call("rbind",ok[-which(isn)])
        ok@data$observed <- sp_points@data[!(isn),aim_variable]
      }else{
        ok <- do.call("rbind",ok)
        ok@data$observed <- sp_points@data[,aim_variable]
      }
      ok@data$residual <- ok@data$observed - ok@data$var1.pred
    }
    
    if(any(procedure=="idw")){
      idw <- Map(function(xyv){interpolate_points(sp_points = sp_points[-xyv,], 
                                                 aim_variable = aim_variable,
                                                 outputfile = sp_points[xyv,],
                                                 co_variables = co_variables, 
                                                 procedure = c("idw"))},
                xyv = 1:nrow(sp_points))
      
      isn <- is.na(idw)
      if(any(isn)){
        idw <- do.call("rbind",idw[-which(isn)])
        idw@data$observed <- sp_points@data[!(isn),aim_variable]
      }else{
        idw <- do.call("rbind",idw)
        idw@data$observed <- sp_points@data[,aim_variable]
      }
      idw@data$residual <- idw@data$observed - idw@data$var1.pred
    }
  }
  return(mget(procedure))
}
