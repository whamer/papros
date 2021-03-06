% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random_forest_interpolation.R
\name{random_forest_interpolation}
\alias{random_forest_interpolation}
\title{Random Forest Interpolation}
\usage{
random_forest_interpolation(sp_points, aim_variable, co_variables, outputfile)
}
\arguments{
\item{sp_points}{SpatialPointsDataframe containing the aim variable}

\item{aim_variable}{Character string with the name of the aim variable}

\item{co_variables}{vector of covariables if needed}

\item{outputfile}{SpatialPointsDataframe, SpatialGridDataFrame or raster which should be filled with predictions; requires covariables for "ked" (if two values are given in a vector a raster is created with the resolution given by the values)}
}
\value{
a dataframe or a SpatialPointsDataFrame containing information about DWD locations in Germany
}
\description{
This function interpolates spatial data by defined spatial covariables
}
\author{
Wolfgang Hamer
}
