% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_dwd_raster.R
\name{download_dwd_raster}
\alias{download_dwd_raster}
\title{Download multi-annual DWD rasters}
\usage{
download_dwd_raster(
  parameter = "air_temperature_mean",
  period = "",
  month = "",
  crop = FALSE,
  savepath = FALSE
)
}
\arguments{
\item{parameter}{a character string defining the parameter to be downloaded (e.g.: "air_temperature_mean", "drought_index", "evapo_p", "frost_days", "hot_days", "ice_days", "precipitation", "snowcover_days", "soil_moist", "soil_temperature_5cm", "summer_days", "sunshine_duration", "vegetation_begin", "vegetation_end", "water_balance")}

\item{period}{years which are combined in the mult annual datasets (e.g.: "1961-1990", "1981-2010","1991-2010", "1992-2015")}

\item{month}{the month which should be downloaded (e.g.: 1,2,3,...,12 or 13 for spring (March, April, May), 14 for summer (June, July, August), ..., or 17 for the whole year)}

\item{crop}{Spatial Dataset of which an extent can be created which is used to crop the germany wide DWD dataset}

\item{savepath}{defalut = FALSE; path to folder where files should be stored}
}
\value{
a raster dataset
}
\description{
This functions download multi-annual DWD rasters and crops them if desired
}
\author{
Wolfgang Hamer
}
