##' This function creates a video of the raster stack
##' @title Videoplot predicted raster stack
##' @param rstack list containing an raster stack of predictions as created by ´machine_predictor´. The names of the rasters are expected to be in the format "X20180515" for the date 2018-05-15
##' @param ffmpeg_path path of the ´ffmpeg.exe´as available by https://www.ffmpeg.org/download.html
##' @param storefile File to which the mp4 file should be stored
##' @param other.opts Further options of the ´saveVideo´ function of the animation library
##' @param main character string containing the main for the raster plot. default = "default" which creates a date of the raster name as mentioned above
##' @param ... Further options of the raster plot, such as col, breaks, sub, cex.axis, ...
##' @return a video file stored at the specified location
##' @import animation
##' @importFrom raster raster
##' @importFrom raster stack
##' @importFrom raster mean
##' @importFrom raster calc
##' @importFrom raster extent
##' @importFrom raster projectRaster
##' @importFrom raster writeRaster
##' @importFrom raster plot
##' @export videoplot_rasterstack
##' @author Wolfgang Hamer
##' @examples
##'
videoplot_rasterstack <- function(rstack,
                                  ffmpeg_path,
                                  storefile,
                                  other.opts = "-pix_fmt yuv420p -b 500k -s:v 720x720",
                                  main ="default",
                                  col=colorRampPalette(c("green","yellow","red"))(8),
                                  breaks=c(0,.125,.25,.375,.5,.625,.75,.875,1),
                                  sub="",
                                  cex.axis=1.3,
                                  cex.main=1.8,
                                  cex.sub=1.6,
                                  cex.lab=1.4,
                                  legend.width=2,
                                  legend.shrink=0.8,
                                  axis.args=list(cex.axis=1.3)){
  ani.options(ffmpeg = ffmpeg_path)

  if(main == "default"){
    main <- as.Date(substr(names(rstack),2,9),"%Y%m%d")
  }

  saveVideo({
    for(i in 1:dim(rstack)[3]){
      plot(rstack[[i]],
           main = main[i],
           col = col,
           breaks = breaks,
           sub=sub,
           cex.axis=cex.axis,
           cex.main=cex.main,
           cex.sub=cex.sub,
           cex.lab=cex.lab,
           legend.width=legend.width,
           legend.shrink=legend.shrink,
           axis.args=axis.args)
      }
    }, video.name = storefile, other.opts = other.opts)
}

