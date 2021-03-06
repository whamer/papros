% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/machine_predictor_lineplot.R
\name{machine_predictor_lineplot}
\alias{machine_predictor_lineplot}
\title{Lineplot of predict by raster stack and machine learning model}
\usage{
machine_predictor_lineplot(
  rstack,
  location,
  yname,
  ylim = c(0, 100),
  rollingaverage = 1,
  threshold = FALSE,
  aggregate_x_ticks = 5
)
}
\arguments{
\item{rstack}{list containing an raster stack of predictions as created by ´machine_predictor´. The names of the rasters are expected to be in the format "X20180515" for the date 2018-05-15}

\item{location}{sp object containing location information}

\item{yname}{name for the y axis}

\item{ylim}{default = c(0,100); limits of the y axis}

\item{rollingaverage}{default = 1; how many points should be averaged for the line}

\item{threshold}{default = FALSE; numeric which indicates a red threshold line on the y axis}

\item{aggregate_x_ticks}{default = 5; how many ticks should the x axis have?}
}
\value{
plot
}
\description{
This functions applies one machine learning model on a given rasterstack
}
\author{
Wolfgang Hamer
}
