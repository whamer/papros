##' This function tries to interpolate points based on given parameters
##' @title Interpolates point values
##' @param sp_points SpatialPointsDataframe containing the aim variable and if "ked" should be applied the covariables
##' @param aim_variable Character string with the name of the aim variable
##' @param outputfile SpatialPointsDataframe, SpatialGridDataFrame or raster which should be filled with predictions; requires covariables for "ked" (if two values are given in a vector a raster is created with the resolution given by the values)
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
##' @export interpolate_points
##' @author Wolfgang Hamer
##' @examples
##'
##' library(magrittr)
##' library(dplyr)
##' library(sp)
##' library(raster)
##' # Download example data
##' shdat <- download_statewide_hourly_station_data(state = "Schleswig-Holstein", coord = TRUE)
##'
##' # Select data of specific Time / Date
##' da_sel <- shdat %>% dplyr::filter(DateTime == sort(unique(shdat$DateTime))[5])
##'
##' # Create spatial dataset
##' da_sel_sp <- SpatialPointsDataFrame(da_sel[,c("lon", "lat")],
##'                                     da_sel,
##'                                     proj4string = CRS("+init=epsg:4326"))
##'
##' # Transform to projected (m based!) system
##' da_sel_sp <- spTransform(da_sel_sp, CRS("+init=epsg:25832"))
##'
##' # Manually creating points of interest
##' preds <- SpatialPointsDataFrame(data.frame(x=c(9.5,9.0,10.4,10.5,9.9,10.5),
##'                                            y=c(53.5, 54.69,54.44,53.93,53.65, 54.55)),
##'                                 data.frame(lat=c(9.5,9.0,10.4,10.5,9.9,10.5),
##'                                            lon=c(53.5, 54.69,54.44,53.93,53.65, 54.55)),
##'                                 proj4string = CRS("+init=epsg:4326"))
##' outputpoints <- spTransform(preds, CRS("+init=epsg:25832"))
##'
##' # Application of function for point data result
##' myintpoints <- interpolate_points(sp_points = da_sel_sp,
##'                                   aim_variable = "Temperature",
##'                                   outputfile = outputpoints,
##'                                   co_variables = c("lat","lon"),
##'                                   procedure = c("ked","ok","idw"))
##'
##' # Create raster of interest
##' outputraster <- raster(ncol=100, nrow=100)
##' extent(outputraster) <- extent(outputpoints)
##' crs(outputraster) <- CRS("+init=epsg:25832")
##' outputraster[]<- rep(1,length(outputraster$layer[]))
##'
##' # Application of function for raster data result
##' myintraster <- interpolate_points(sp_points = da_sel_sp,
##'                                   aim_variable = "Temperature",
##'                                   outputfile = outputraster,
##'                                   co_variables = c("lat","lon"),
##'                                   procedure = c("ked","ok","idw"))
##'
##' # Application of function for raster data result
##' myintraster2 <- interpolate_points(sp_points = da_sel_sp,
##'                                    aim_variable = "Temperature",
##'                                    outputfile = c(500,500),
##'                                    co_variables = c("lat","lon"),
##'                                    procedure = c("ked","ok","idw"))

interpolate_points <- function(sp_points, aim_variable, outputfile, co_variables = FALSE, procedure = c("ked","rfk","ok","idw")){
  success <- FALSE

  sp_points <- sp_points[c(!is.na(sp_points@data[,aim_variable])),]

  if(length(outputfile)==2){
    outputfile <- raster(ext=extent(sp_points),
                         resolution = outputfile,
                         crs = CRS(proj4string(sp_points)))
    outputfile[]<- rep(1,length(outputfile$layer[]))
  }
  if (any(c('RasterLayer','RasterStack','RasterBrick') %in% class(outputfile))){
    if (proj4string(sp_points)!=proj4string(outputfile)){
      #outputfile <- raster::projectRaster(from = outputfile, crs = CRS(proj4string(sp_points)))
      sp_points <- spTransform(sp_points, proj4string(outputfile))
      sp_points@proj4string <- outputfile@crs
    }
    outputfile <- as(outputfile, 'SpatialGridDataFrame')
  }



  while(success == FALSE){
    if(procedure[1]=="empty" & success == FALSE){
      warning("No interpolation possible!")
      returnobj <- NA
      success <- TRUE
    }else{
      if(procedure[1]=="ked"){
        form <- as.formula(paste0(aim_variable,
                                  ifelse(is.logical(co_variables[1]),"~ 1",
                                         paste("~",paste0(co_variables,collapse = "+")))))
        sp_points_ked <- sp_points

        # Remove NAs of covariable
        if(!is.logical(co_variables[1])){
          sp_points_ked <- sf::st_as_sf(sp_points_ked) %>%
            dplyr::select(aim_variable,co_variables) %>%
            na.omit() %>%
            as(.,"Spatial")
          sp_points_ked@proj4string <- CRS(proj4string(sp_points))
        }

        if(dim(sp_points_ked)[1] < 5){
          vers <- paste("With",dim(sp_points_ked)[1],"elements to interpolate, no KED!")
        }else{
          z = capture.output(vers <- try(automap::autoKrige(formula = form,
                                                            input_data = sp_points_ked,
                                                            new_data = outputfile),
                                         silent=TRUE))
        }

        if ('autoKrige' %in% class(vers)){
          if("SpatialPointsDataFrame" %in% class(outputfile)){
            returnobj <- vers$krige_output
          }else if("SpatialGridDataFrame" %in% class(outputfile)){
            returnobj <- raster(vers$krige_output)
          }else{
            warning("Unknown class of outputfile!")
          }
          success <- TRUE
        }else{
          message(paste("Method ked did not success because:",vers))
          procedure <- procedure[-1]
        }
      }

      if(length(procedure)==0){procedure <- "empty"}

      if(procedure[1]=="rfk"){
        returnobj <- random_forest_interpolation(sp_points = sp_points,
                                                 aim_variable = aim_variable,
                                                 co_variables = co_variables,
                                                 outputfile = outputfile)

        if(any(c("SpatialPointsDataFrame","RasterLayer") %in% class(returnobj))){
          success <- TRUE
        }else{
          procedure <- procedure[-1]
        }
      }

      if(length(procedure)==0){procedure <- "empty"}

      if(procedure[1]=="ok"){
        form <- as.formula(paste0(aim_variable,"~ 1"))

        if(dim(sp_points)[1] < 5){
          vers <- paste("With",dim(sp_points)[1],"elements to interpolate, no OK!")
        }else{
          z = capture.output(vers <- try(automap::autoKrige(formula = form,
                                                            input_data = sp_points,
                                                            new_data = outputfile),
                                         silent=TRUE))
        }

        if ('autoKrige' %in% class(vers)){
          if("SpatialPointsDataFrame" %in% class(outputfile)){
            returnobj <- vers$krige_output
          }else if("SpatialGridDataFrame" %in% class(outputfile)){
            returnobj <- raster(vers$krige_output)
          }else{
            warning("Unknown class of outputfile!")
          }
          success <- TRUE
        }else{
          message(paste("Method ok did not success because:",vers))
          procedure <- procedure[-1]
        }
      }

      if(length(procedure)==0){procedure <- "empty"}

      if(procedure[1]=="idw"){
        form <- as.formula(paste0(aim_variable,"~ 1"))
        z = capture.output(vers <- try(gstat::idw(form,
                                                  sp_points,
                                                  outputfile),
                                       silent=TRUE))

        if ('try-error' %in% class(vers)){
          message(paste("Method idw did not success because:",vers))
          procedure <- procedure[-1]
        }else{
          if("SpatialPointsDataFrame" %in% class(outputfile)){
            returnobj <- vers
          }else if("SpatialGridDataFrame" %in% class(outputfile)){
            returnobj <- raster(vers)
          }else{
            warning("Unknown class of outputfile!")
          }
          success <- TRUE
        }
      }
      if(length(procedure)==0){procedure <- "empty"}
    }
  }
  return(returnobj)
}
