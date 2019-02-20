##' This functions applies one machine learning model on a given rasterstack
##' @title Predict by raster stack and machine learning model
##' @param rstack list containing an raster stack with covariables for prediction based on the mmodel
##' @param mmodel machine learning model
##' @param additionalRaster rasters that are identical in each time step and should be added to each rasterstack
##' @param type character string containing type of prediction (e.g. "prob" for probability); default to FALSE
##' @param index in case of type = "prob" the index of the parameter of which the probability shold be returned
##' @return stack with one prediction for each element of the input list
##' @importFrom raster raster
##' @importFrom raster stack
##' @importFrom raster mean
##' @importFrom raster calc
##' @importFrom raster extent
##' @importFrom raster predict
##' @export machine_predictor
##' @author Wolfgang Hamer
##' @examples
machine_predictor <- function(rstack, mmodel, additionalRaster=FALSE,type=FALSE,index=FALSE){
  unra <- names(rstack)
  pb <- progress_estimated(length(unra))

  predsta <- Map(function(x){
    pb$tick()$print()
    sta <- rstack[[x]]
    if(!is.logical(additionalRaster)){sta <- raster::stack(sta,additionalRaster)}
    if(!is.logical(type)){
      return(raster::predict(sta,mmodel,type=type, index=index))
    }else{
      return(raster::predict(sta,mmodel))
    }
  }, x = unra)
  return(raster::stack(predsta))
}

