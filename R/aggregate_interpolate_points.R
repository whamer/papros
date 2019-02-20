##' This functions interpolates points based on a dataframe containing coordinates, an aim variable, an aim dataset and a parameter such as a date, by which the interpolations are divided
##' @title Interpolate and temporal aggregate DWD values
##' @param dataframe dataframe containing the aim variable and if "ked" should be applied the covariables
##' @param coords vector containing names of the columns containing x and y coordinate values
##' @param epsg number of the EPSG code of the "coords" information
##' @param DateTime name of the DateTime column
##' @param infection duration of the assumed infection
##' @param incubation duration of the assumed incubation
##' @param aim_variable Character string with the name of the aim variable
##' @param outputfile SpatialPointsDataframe, SpatialGridDataFrame or raster which should be filled with predictions; requires covariables for "ked"
##' @param trans_epsg default = FALSE, number of the EPSG code the "coords" information should be transformed to
##' @param co_variables default = FALSE, vector of covariables if needed
##' @param procedure default = c("ked","ok","idw"); vector containing the interpolation technic to be used; the first method is used and if this does not work out, the second, and so on
##' @param progressbar default = TRUE; should a progressbar be generated?
##' @return a dataframe or a SpatialPointsDataFrame containing information about DWD locations in Germany
##' @import dplyr sp
##' @importFrom raster raster
##' @importFrom raster stack
##' @importFrom raster mean
##' @importFrom raster calc
##' @importFrom raster extent
##' @importFrom magrittr %>%
##' @importFrom magrittr %<>%
##' @export aggregate_interpolate_points
##' @author Wolfgang Hamer
##' @examples
##'
##' # Download example data
##' shdat <- download_statewide_hourly_station_data(state = "Schleswig-Holstein", coord = TRUE)
##'
##' shdat2 <- shdat %>%
##'     filter(DateTime < sort(unique(shdat$DateTime))[50])
##'
##' example <- aggregate_interpolate_points(dataframe = shdat2,
##'                                        coords = c("lon","lat"),
##'                                        DateTime = "DateTime",
##'                                        infection=2,
##'                                        incubation=8,
##'                                        aim_variable ="Temperature",
##'                                        outputfile=c(1000,1000),
##'                                        co_variables = FALSE,
##'                                        procedure = c("ked","ok","idw"),
##'                                        epsg = 4326,
##'                                        trans_epsg = 25832)
##' plot(example[[1]])
aggregate_interpolate_points <- function(dataframe, coords, epsg, DateTime, infection, incubation, aim_variable, outputfile, trans_epsg = FALSE, co_variables = FALSE, procedure = c("ked","ok","idw"), progressbar = TRUE){
  if(length(outputfile)==2){
    # Create spatial dataset
    sp_points <- SpatialPoints(dataframe[,c(coords[1], coords[2])][!duplicated(dataframe[,c(coords[1], coords[2])]),],
                               proj4string = CRS(paste0("+init=epsg:",epsg)))
    # Transform to projected (m based!) system
    if(!is.logical(trans_epsg)){sp_points <- spTransform(sp_points, CRS(paste0("+init=epsg:",trans_epsg)))}

    outputfile <- raster::raster(ext=raster::extent(sp_points),
                                 resolution = outputfile,
                                 crs = CRS(proj4string(sp_points)))
    outputfile[]<- rep(1,length(outputfile$layer[]))
  }

  dataframe <- dwd_add_date_time(dataframe)

  undays <- sort(unique(dataframe$Date))

  if(progressbar){
    pb <- progress_estimated(length(aim_variable))
  }

  for(plura in aim_variable){
    dataframe2 <- dataframe[!is.na(dataframe[,plura]),]

    aggrerast <- Map(function(i){
      itdat <- dataframe2 %>% dplyr::filter(Date %in% seq(from = undays[(i-infection)+1], to = undays[i], by = 1))
      itint <- multiple_interpolate_points(dataframe = itdat,
                                           coords = coords,
                                           epsg = epsg,
                                           splitter = DateTime,
                                           aim_variable = plura,
                                           outputfile = outputfile,
                                           trans_epsg = trans_epsg,
                                           co_variables = co_variables,
                                           procedure = procedure,
                                           progressbar = FALSE)
      if(class(outputfile)=="numeric"|class(outputfile)=="RasterLayer"|class(outputfile)=="SpatialGridsDataFrame"){
        meanit <- raster::mean(raster::stack(itint))
        names(meanit) <- paste0(plura,"_mean")
        minit <- raster::calc(raster::stack(itint),min)
        names(minit) <- paste0(plura,"_min")
        maxit <- raster::calc(raster::stack(itint),max)
        names(maxit) <- paste0(plura,"_max")
        return(raster::stack(meanit,minit,maxit))
      }else if(class(outputfile)=="SpatialPointsDataFrame"){
        ret <- as.data.frame(
          do.call(rbind, Map(function(x){
          cbind(coordinates(itint[[x]]),itint[[x]]@data[,1])
          }, x = 1:length(itint))))
        names(ret) <- c("x","y","Value")

        ret %<>% dplyr::group_by(x, y) %>%
          dplyr::summarise(Mean_Value =mean(Value),
                           Min_Value =min(Value),
                           Max_Value =max(Value)) %>%
          as.data.frame(.)

        names(ret)[c(3,4,5)] <- c(paste0(plura,"_mean"),
                                  paste0(plura,"_min"),
                                  paste0(plura,"_max"))

        ret2 <- merge(outputfile,ret,by=c("x","y"))
        ret2$Date <- gsub("-","",undays[(i-infection)+1]+incubation)
        return(ret2)
      }else{
        message(paste("Class",class(outputfile),"not yet supported!"))
      }

    },
    i = infection:length(undays))

    names(aggrerast) <- unlist(Map(function(i){gsub("-","",undays[(i-infection)+1]+incubation)},
                                   i = infection:length(undays)))
    assign(plura,aggrerast)

    if(progressbar){
      pb$tick()$print()
    }
  }

  if(length(aim_variable) > 1){
    unda <- unique(c(names(get(aim_variable[1])),names(get(aim_variable[2]))))

    if(class(outputfile)=="SpatialPointsDataFrame"){
      combt <- Map(function(x){
        merge(get(aim_variable[1])[[x]],get(aim_variable[2])[[x]],by=c("x","y","Date"))
        },x=unda)
    }else if(class(outputfile)=="numeric"|class(outputfile)=="RasterLayer"|class(outputfile)=="SpatialGridsDataFrame"){
      combt <- Map(function(x){
        raster::stack(get(aim_variable[1])[[x]],get(aim_variable[2])[[x]])
      },x=unda)
    }

    if(length(aim_variable) > 2){
      for(co in 3:length(aim_variable)){
        unda <- unique(c(names(combt),names(get(aim_variable[co]))))

        if(class(outputfile)=="SpatialPointsDataFrame"){
          combt <- Map(function(x){
            merge(combt[[x]],get(aim_variable[co])[[x]],by=c("x","y","Date"))
          },x=unda)
        }else if(class(outputfile)=="numeric"|class(outputfile)=="RasterLayer"|class(outputfile)=="SpatialGridsDataFrame"){
          combt <- Map(function(x){
            raster::stack(combt[[x]],get(aim_variable[co])[[x]])
          },x=unda)
        }
      }
    }
  }else{
    combt <- aggrerast
  }
  return(combt)
}




