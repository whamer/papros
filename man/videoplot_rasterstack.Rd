% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/videoplot_rasterstack.R
\name{videoplot_rasterstack}
\alias{videoplot_rasterstack}
\title{Videoplot predicted raster stack}
\usage{
videoplot_rasterstack(
  rstack,
  ffmpeg_path,
  storefile,
  other.opts = "-pix_fmt yuv420p -b 500k -s:v 720x720",
  main = "default",
  col = colorRampPalette(c("green", "yellow", "red"))(8),
  breaks = c(0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 1),
  sub = "",
  cex.axis = 1.3,
  cex.main = 1.8,
  cex.sub = 1.6,
  cex.lab = 1.4,
  legend.width = 2,
  legend.shrink = 0.8,
  axis.args = list(cex.axis = 1.3)
)
}
\arguments{
\item{rstack}{list containing an raster stack of predictions as created by ´machine_predictor´. The names of the rasters are expected to be in the format "X20180515" for the date 2018-05-15}

\item{ffmpeg_path}{path of the ´ffmpeg.exe´as available by https://www.ffmpeg.org/download.html}

\item{storefile}{File to which the mp4 file should be stored}

\item{other.opts}{Further options of the ´saveVideo´ function of the animation library}

\item{main}{character string containing the main for the raster plot. default = "default" which creates a date of the raster name as mentioned above}

\item{...}{Further options of the raster plot, such as col, breaks, sub, cex.axis, ...}
}
\value{
a video file stored at the specified location
}
\description{
This function creates a video of the raster stack
}
\examples{

}
\author{
Wolfgang Hamer
}
