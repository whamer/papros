##' This function interpolates spatial data by defined spatial covariables
##' @title Random Forest Interpolation
##' @param sp_points SpatialPointsDataframe containing the aim variable 
##' @param aim_variable Character string with the name of the aim variable
##' @param co_variables vector of covariables if needed
##' @param outputfile SpatialPointsDataframe, SpatialGridDataFrame or raster which should be filled with predictions; requires covariables for "ked" (if two values are given in a vector a raster is created with the resolution given by the values)
##' @return a dataframe or a SpatialPointsDataFrame containing information about DWD locations in Germany
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
##' @export random_forest_interpolation
##' @author Wolfgang Hamer
random_forest_interpolation <- function(sp_points, aim_variable, co_variables, outputfile){
  sp_points <- sp_points[!is.na(sp_points@data[,aim_variable]),]
  
  if (any(c('RasterLayer','RasterStack','RasterBrick') %in% class(outputfile))){
    if (proj4string(sp_points)!=proj4string(outputfile)){
      #outputfile <- raster::projectRaster(from = outputfile, crs = CRS(proj4string(sp_points)))
      sp_points <- spTransform(sp_points, proj4string(outputfile))
    }
    outputfile <- as(outputfile, 'SpatialGridDataFrame')
  }
  
  form <- as.formula(paste0(aim_variable,
                            ifelse(is.logical(co_variables[1]),"~ 1",
                                   paste("~",paste0(co_variables,collapse = "+")))))
  sp_points_rfk <- sp_points
  
  # Remove NAs of covariable
  if(!is.logical(co_variables[1])){
    sp_points_rfk <- sf::st_as_sf(sp_points_rfk) %>% 
      dplyr::select(aim_variable,co_variables) %>% 
      na.omit() %>% 
      as(.,"Spatial")
    sp_points_rfk@proj4string <- CRS(proj4string(sp_points))
  }
  
  if(dim(sp_points_rfk)[1] < 3){
    vers <- paste("With",dim(sp_points_rfk)[1],"elements to interpolate, no RFK!")
  }else{
    rad <- randomForest::randomForest(form,sp_points_rfk)
    
    sp_points_rfk$Residual <- sp_points_rfk@data[,aim_variable] - rad$predicted
    
    z = capture.output(vers <- try(automap::autoKrige(formula = Residual ~ 1,
                                                      input_data = sp_points_rfk,
                                                      new_data = outputfile),
                                   silent=TRUE))
  }
  
  if ('autoKrige' %in% class(vers)){
    if("SpatialPointsDataFrame" %in% class(outputfile)){
      returnobj <- vers$krige_output
      returnobj$var1.pred <- returnobj$var1.pred + predict(rad,outputfile)
      returnobj <- returnobj[,names(returnobj)=="var1.pred"]
    }else if("SpatialGridDataFrame" %in% class(outputfile)){
      returnobj <- raster::calc(raster::stack(raster::predict(stack(outputfile),rad),raster(vers$krige_output)),sum)
      names(returnobj) <- "var1.pred"
    }else{
      warning("Unknown class of outputfile!")
    }
    return(returnobj)
  }else{
    message(paste("Method RFK did not success because:",vers))
  }
}

