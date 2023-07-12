\encoding{UTF-8}
\name{CalGR}
\alias{CalGR}


\title{Calibration algorithm that optimises the error criterion selected as objective function}


\description{Calibration algorithm that optimises the error criterion selected as objective function using the INRAE-HYCAR procedure described by C. Michel}


\usage{
CalGR(PrepGR, CalCrit = c("NSE", "KGE", "KGE2", "RMSE"),
      WupPer = NULL, CalPer,
      transfo = c("", "sqrt", "log", "inv", "sort"), verbose = TRUE)
}


\arguments{
  \item{PrepGR}{[object of class \code{PrepGR}] see \code{\link{PrepGR}} for details}

  \item{CalCrit}{[character] name of the objective function (must be one of \code{"NSE"}, \code{"KGE"}, \code{"KGE2"} or \code{"RMSE"})}

  \item{WupPer}{(optional) [character] vector of 2 values to define the beginning and end of the warm-up period [\code{"YYYY-mm-dd"} or \code{"YYYY-mm-dd HH:MM:SS"}]; [0L] to disable the warm-up period. See details}

  \item{CalPer}{[character] vector of 2 values to define the beginning and end of the calibration period [\code{"YYYY-mm-dd"} or \code{"YYYY-mm-dd HH:MM:SS"}]}

  \item{transfo}{(optional) [character] name of the transformation transformation applied to discharge for calculating the objective function (must be one of \code{""}, \code{"sqrt"}, \code{"log"}, \code{"inv"} or \code{"sort"})}

  \item{verbose}{(optional) [boolean] logical value indicating if the function is run in verbose mode or not}
}


\value{
  [list] object of class \code{CalGR} containing:
  \item{OptionsCalib}{[list] object of class \code{RunOptions} (see: \code{\link[airGR]{CreateRunOptions}})}
  \item{Qobs}{[numeric]  series of observed discharges [mm/time step]}
  \item{OutputsCalib}{[list] object of class \code{OutputsCalib} (see: \code{\link[airGR]{Calibration}})}
  \item{OutputsModel}{[list] object of class \code{OutputsModel} (see: \code{\link[airGR]{RunModel}})}
  \item{TypeModel}{[character] name of the function of the hydrological model used}
  \item{CalCrit}{[character] name of the function that computes the error criterion during the calibration step}
  \item{PeriodModel}{[list] \code{$WarmUp}: vector of 2 \code{POSIXct} values defining the beginning and end of the warm-up period, \code{$Run}: vector of 2 \code{POSIXct} values defining the beginning and end of the calibration period}
}


\seealso{
airGRteaching
\code{\link{plot}} and  \code{\link{dyplot}}
functions to display static and dynamic plots
\cr\cr
airGR
\code{\link[airGR]{CreateRunOptions}}, \code{\link[airGR]{CreateInputsCrit}}, \code{\link[airGR]{CreateCalibOptions}}, \code{\link[airGR]{ErrorCrit_RMSE}}, \code{\link[airGR]{ErrorCrit_NSE}}, \code{\link[airGR]{ErrorCrit_KGE}}, \code{\link[airGR]{ErrorCrit_KGE2}}, \code{\link[airGR]{Calibration_Michel}}
functions
}


\details{
\code{WupPer = NULL} indicates that, if available, a period of one year immediately present before the CalPer period is used. \code{WupPer = 0L} allows to disable the warm up of the model.
}


\author{Olivier Delaigue, Guillaume Thirel}


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


## Structure of CalGR object
str(CAL)

## Parameter and criterion evolution during
## the steepest descent step of the calibration algorithm
plot(CAL, which = "iter")

## Plot diagnostics
plot(CAL)

## Static plot of observed and simulated time series
plot(CAL, which = "ts")
plot(CAL, which = c("Precip", "Flows"))

## Dynamic plot of observed and simulated time series
dyplot(CAL)
}


