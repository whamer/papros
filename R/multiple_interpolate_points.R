##' This functions interpolates points based on a dataframe containing coordinates, an aim variable, an aim dataset and a parameter such as a date, by which the interpolations are divided
##' @title Interpolates DWD values
##' @param dataframe dataframe containing the aim variable and if "ked" should be applied the covariables
##' @param coords vector containing names of the columns containing x and y coordinate values
##' @param epsg number of the EPSG code of the "coords" information
##' @param splitter name of the splitter column
##' @param aim_variable Character string with the name of the aim variable
##' @param outputfile SpatialPointsDataframe, SpatialGridDataFrame or raster which should be filled with predictions; requires covariables for "ked"
##' @param trans_epsg default = FALSE, number of the EPSG code the "coords" information should be transformed to
##' @param co_variables default = FALSE, vector of covariables if needed
##' @param procedure default = c("ked","ok","idw"); vector containing the interpolation technic to be used; the first method is used and if this does not work out, the second, and so on
##' @param progressbar default = TRUE; should a progressbar be generated?
##' @return a dataframe or a SpatialPointsDataFrame containing information about DWD locations in Germany
##' @import dplyr sp gstat
##' @importFrom magrittr %>%
##' @importFrom magrittr %<>%
##' @importFrom raster raster
##' @importFrom raster stack
##' @importFrom raster mean
##' @importFrom raster calc
##' @importFrom raster extent
##' @export multiple_interpolate_points
##' @author Wolfgang Hamer
##' @examples
##'
##' # Download example data
##' shdat <- download_statewide_hourly_station_data(state = "Schleswig-Holstein", coord = TRUE)
##'
##' shdat2 <- shdat %>%
##'     filter(DateTime < sort(unique(shdat$DateTime))[5])
##'
##' example <- multiple_interpolate_points(dataframe = shdat2,
##'                                        coords = c("lon","lat"),
##'                                        splitter = "DateTime",
##'                                        aim_variable ="Temperature",
##'                                        outputfile=c(1000,1000),
##'                                        co_variables = FALSE,
##'                                        procedure = c("ked","ok","idw"),
##'                                        epsg = 4326,
##'                                        trans_epsg = 25832)
##' plot(stack(example))
multiple_interpolate_points <- function(dataframe, coords, epsg, splitter, aim_variable, outputfile, trans_epsg = FALSE, co_variables = FALSE, procedure = c("ked","ok","idw"),progressbar=TRUE){
  dataframe <- dataframe[!is.na(dataframe[,aim_variable]),]

  spli <- unique(dataframe[,splitter])

  if(progressbar){
    pb <- progress_estimated(length(spli))
  }

  int_list <- Map(function(parts){
    da_sel <- dataframe[dataframe[,splitter]==parts,]

    # Create spatial dataset
    da_sel_sp <- SpatialPointsDataFrame(da_sel[,c(coords[1], coords[2])],
                                        da_sel,
                                        proj4string = CRS(paste0("+init=epsg:",epsg)))
    # Transform to projected (m based!) system
    if(!is.logical(trans_epsg)){da_sel_sp <- spTransform(da_sel_sp, CRS(paste0("+init=epsg:",trans_epsg)))}

    myintraster <- interpolate_points(sp_points = da_sel_sp,
                                      aim_variable = aim_variable,
                                      outputfile = outputfile,
                                      co_variables = co_variables,
                                      procedure = procedure)
    names(myintraster) <- paste0(aim_variable,"_",parts)
    if(progressbar){
      pb$tick()$print()
    }
    return(myintraster)},
    parts = spli)

  return(int_list)
}





