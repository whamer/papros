##' This functions applies one machine learning model on a given rasterstack
##' @title Lineplot of predict by raster stack and machine learning model
##' @param rstack list containing an raster stack of predictions as created by ´machine_predictor´. The names of the rasters are expected to be in the format "X20180515" for the date 2018-05-15
##' @param location sp object containing location information
##' @param yname name for the y axis
##' @param ylim default = c(0,100); limits of the y axis
##' @param rollingaverage default = 1; how many points should be averaged for the line
##' @param threshold default = FALSE; numeric which indicates a red threshold line on the y axis
##' @param aggregate_x_ticks default = 5; how many ticks should the x axis have?
##' @return plot
##' @import ggplot2
##' @importFrom raster raster
##' @importFrom raster stack
##' @importFrom raster mean
##' @importFrom raster calc
##' @importFrom raster extent
##' @importFrom raster projectRaster
##' @importFrom raster writeRaster
##' @importFrom zoo rollmean
##' @importFrom raster predict
##' @importFrom magrittr %>%
##' @importFrom magrittr %<>%
##' @export machine_predictor_lineplot
##' @author Wolfgang Hamer
##' @examples
machine_predictor_lineplot <- function(rstack, location,yname,ylim=c(0,100),rollingaverage=1,threshold=FALSE,aggregate_x_ticks=5){
  if(class(location)=="SpatialPointsDataFrame"){
    val <- data.frame(Date = as.Date(paste(substr(names(rstack),2,5),substr(names(rstack),6,7),substr(names(rstack),8,9),sep="-"),"%Y-%m-%d"),
                      Value = c(raster::extract(rstack,location)))

    val$Valueav <- zoo::rollmean(val$Value, rollingaverage, na.pad=TRUE)
    spline_int <- as.data.frame(spline(val$Date, val$Valueav))

    pl <- ggplot(data=val, aes(x=as.numeric(Date), y=Value, group=1)) +
      #geom_line(linetype = "dashed", size=1.3)+
      theme_bw()+
      geom_line(data = spline_int, aes(x = x, y = y),linetype = "dashed", size=1.3)+
      geom_point(size=4,color="blue")+
      labs(x = "Date", y = yname)+
      ylim(ylim)+
      scale_x_continuous(breaks=as.numeric(val$Date),
                       labels=substr(val$Date,6,10))

    if(!is.logical(threshold)){
      pl <- pl + geom_hline(yintercept = threshold, linetype = "dotted",color="red", size=1.3)
    }

    return(pl)
  }else if(class(location)=="SpatialPolygonsDataFrame"){

    preval <- as.data.frame(raster::extract(rstack,location)) %>%
      na.omit()

    val <- data.frame(Date = as.Date(paste(substr(names(rstack),2,5),substr(names(rstack),6,7),substr(names(rstack),8,9),sep="-"),"%Y-%m-%d"),
                      Mean = c(apply(preval,2,mean)),
                      Min = c(apply(preval,2,min)),
                      Max = c(apply(preval,2,max)))

    val$Meanav <- zoo::rollmean(val$Mean, rollingaverage, na.pad=TRUE)
    val$Minnav <- zoo::rollmean(val$Min, rollingaverage, na.pad=TRUE)
    val$Maxav <- zoo::rollmean(val$Max, rollingaverage, na.pad=TRUE)

    spline_int1 <- as.data.frame(spline(val$Date, val$Meanav))
    spline_int2 <- as.data.frame(spline(val$Date, val$Minnav))
    spline_int3 <- as.data.frame(spline(val$Date, val$Maxav))

    spline_int <- cbind(spline_int1,spline_int2[,2],spline_int3[,2])
    names(spline_int) <- c("Date","Mean","Minnav","Maxav")

    if(aggregate_x_ticks>length(val$Date)){
      x_ticks <- seq_along(val$Date)
    }else{
      x_ticks <- round(seq(min(seq_along(val$Date)),max(seq_along(val$Date)),length=aggregate_x_ticks))
    }


    pl <- ggplot(data=val, aes(x=as.numeric(Date), y=Mean)) +
      theme_bw() +
      geom_ribbon(mapping=aes(ymin = Minnav, ymax = Maxav), data = spline_int,  fill = "grey80") +
      geom_line(data = spline_int, aes(x = Date, y = Mean),linetype = "dashed", size=1.3)+
      geom_point(size=4,color="blue")+
      labs(x = "Date", y = yname)+
      ylim(ylim)+
      scale_x_continuous(breaks=as.numeric(val$Date)[x_ticks],
                         labels=substr(val$Date,6,10)[x_ticks])

    if(!is.logical(threshold)){
      pl <- pl + geom_hline(yintercept = threshold, linetype = "dotted",color="red", size=1.3)
    }
    return(pl)
  }
}


