\encoding{UTF-8}
\name{CalGR}
\alias{CalGR}
\title{Calibration algorithm that optimises the error criterion selected as objective function}


\usage{
CalGR(PrepGR, CalCrit = c("NSE", "KGE", "KGE2", "RMSE"),
  WupPer = NULL, CalPer,
  transfo = c("", "sqrt", "log", "inv", "sort"), verbose = TRUE)
}


\arguments{
  \item{PrepGR}{[object of class \code{PrepGR}] see \code{\link{PrepGR}} for details}
  
  \item{CalCrit}{[character] name of the objective function (must be one of \code{"NSE"}, \code{"KGE"}, \code{"KGE2"} or \code{"RMSE"})}
  
  \item{WupPer}{(optional) [character] vector of 2 values to define the beginning and end of the  warm-up period [\code{"YYYY-mm-dd"} or \code{"YYYY-mm-dd HH:MM:SS"}]}
  
  \item{CalPer}{[character] vector of 2 values to define the beginning and end of the  calibration period [\code{"YYYY-mm-dd"} or \code{"YYYY-mm-dd HH:MM:SS"}]}
  
  \item{transfo}{(optional) [character] name of the transformation transformation applied to discharge for calculating the objective function (must be one of \code{""}, \code{"sqrt"}, \code{"log"}, \code{"inv"} or \code{"sort"})}
  
  \item{verbose}{(optional) [boolean] logical value indicating if the function is run in verbose mode or not}
}


\value{
  [list] object of class \code{CalGR} containing:
  \item{OptionsCalib}{[list] object of class \code{RunOptions} (see: \code{\link{CreateRunOptions}})}
  \item{Qobs}{[numeric]  series of observed discharges [mm/time step]}
  \item{OutputsCalib}{[list] object of class \code{OutputsCalib} (see: \code{\link{Calibration}})}
  \item{OutputsModel}{[list] object of class \code{OutputsModel} (see: \code{\link{RunModel}})}
  \item{TypeModel}{[character] name of the function of the hydrological model used}
  \item{CalCrit}{[character] name of the function that computes the error criterion during the calibration step}
  \item{PeriodModel}{[list] \code{$WarmUp}: vector of 2 \code{POSIXct} values defining the beginning and end of the warm-up period, \code{$Run}: vector of 2 \code{POSIXct} values defining the beginning and end of the calibration period}
}


\seealso{
\code{\link{CreateRunOptions}}, \code{\link{CreateInputsCrit}}, \code{\link{CreateCalibOptions}}, \code{\link{ErrorCrit_RMSE}}, \code{\link{ErrorCrit_NSE}}, \code{\link{ErrorCrit_KGE}}, \code{\link{ErrorCrit_KGE2}}, \code{\link{Calibration_Michel}}
}


\description{Calibration algorithm that optimises the error criterion selected as objective function using the Irstea-HBAN procedure described by C. Michel}


\author{Olivier Delaigue}


\examples{
library(airGRteaching)

## data.frame of observed data
data(L0123001, package = "airGR")
BasinObs2 <- BasinObs[, c("DatesR", "P", "E", "Qmm", "T")]

## Preparation of observed data for modelling
PREP <- PrepGR(ObsDF = BasinObs2, HydroModel = "GR4J", CemaNeige = TRUE)

## Calibration step
CAL <- CalGR(PrepGR = PREP, CalCrit = "KGE2",
             WupPer = NULL, CalPer = c("1990-01-01", "1993-12-31"))
str(CAL)
}


