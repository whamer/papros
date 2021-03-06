% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loocv_interpolate_points.R
\name{loocv_interpolate_points}
\alias{loocv_interpolate_points}
\title{LOOCV of given point values}
\usage{
loocv_interpolate_points(
  sp_points,
  aim_variable,
  co_variables = FALSE,
  procedure = c("ked", "rfk", "ok", "idw")
)
}
\arguments{
\item{sp_points}{SpatialPointsDataframe containing the aim variable and if "ked" should be applied the covariables}

\item{aim_variable}{Character string with the name of the aim variable}

\item{co_variables}{default = FALSE, vector of covariables if needed}

\item{procedure}{default = c("ked","rfk,"ok","idw"); vector containing the interpolation technic to be used; the first method is used and if this does not work out, the second, and so on; KED = Kriging With external Drift; RFK = Random Forest Kriging; OK = Ordinary Kriging; IDW = Inverse distance Weighting}
}
\value{
interpolated point values as sp or raster file depending on outputfile
}
\description{
This function applies a leave-one-out-cross-validation on given spatial data
}
\author{
Wolfgang Hamer
}
