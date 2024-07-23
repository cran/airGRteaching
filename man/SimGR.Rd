\encoding{UTF-8}
\name{SimGR}
\alias{SimGR}


\title{Running one of the GR hydrological models}


\description{Function for running the GR hydrological models}


\usage{
SimGR(PrepGR, CalGR = NULL, Param, EffCrit = c("NSE", "KGE", "KGE2", "RMSE"),
  WupPer = NULL, SimPer,
  transfo = c("", "sqrt", "log", "inv", "sort"), verbose = TRUE)
}


\arguments{
  \item{PrepGR}{[object of class \emph{PrepGR}] see \code{\link{PrepGR}} for details}

  \item{CalGR}{(deprecated) use the \code{Param} argument instead}

  \item{Param}{[object of class \emph{CalGR} or numeric] see \code{\link{CalGR}}. The length of the vector of parameters depends on the model used, see below for details}

  \item{EffCrit}{[character] name of the efficiency criterion (must be one of \code{"NSE"}, \code{"KGE"}, \code{"KGE2"} or \code{"RMSE"})}

  \item{WupPer}{(optional) [character] vector of 2 values to define the beginning and end of the warm-up period [\code{"YYYY-mm-dd"} or \code{"YYYY-mm-dd HH:MM:SS"}]; [0L] to disable the warm-up period. See details}

  \item{SimPer}{[character] vector of 2 values to define the beginning and end of the simulation period  [\code{"YYYY-mm-dd"} or \code{"YYYY-mm-dd HH:MM:SS"}]}

  \item{transfo}{(optional) [character] name of the transformation applied to discharge for calculating the error criterion (must be one of \code{""}, \code{"sqrt"}, \code{"log"}, \code{"inv"} or \code{"sort"})}

  \item{verbose}{(optional) [boolean] logical value indicating if the function is run in verbose mode or not}
}


\value{
  [list] object of class \code{SimGR} containing:
  \item{OptionsSimul}{[list] object of class \code{RunOptions} (see: \code{\link[airGR]{CreateRunOptions}})}
  \item{OptionsCrit}{[list] object of class \code{InputsCrit} (see: \code{\link[airGR]{CreateInputsCrit}})}
  \item{OutputsModel}{[list] object of class \code{OutputsModel} (see: \code{\link[airGR]{RunModel}})}
  \item{Qobs}{[numeric]  series of observed discharges [mm/time step]}
  \item{TypeModel}{[character] name of the function of the hydrological model used}
  \item{CalCrit}{[character] name of the function that computes the error criterion during the calibration step}
  \item{EffCrit}{[list] name of the function that computes the error criterion during the simulation step}
  \item{PeriodModel}{[list] \code{$WarmUp}: vector of 2 \code{POSIXct} values defining the beginning and end of the warm-up period; \code{$Run}: vector of 2 \code{POSIXct} values defining the beginning and end of the calibration period}
}



\seealso{
airGRteaching
\code{\link{plot}} and  \code{\link{dyplot}}
functions to display static and dynamic plots
\cr\cr
airGR
\code{\link[airGR]{CreateRunOptions}}, \code{\link[airGR]{CreateInputsCrit}}, \code{\link[airGR]{RunModel}}, \code{\link[airGR]{ErrorCrit_RMSE}}, \code{\link[airGR]{ErrorCrit_NSE}}, \code{\link[airGR]{ErrorCrit_KGE}}, \code{\link[airGR]{ErrorCrit_KGE2}}
functions
}


\details{
The user can customize the parameters with the \code{Param} argument.
The user can also use the parameters resulting from a calibration. In this case, it is necessary to use the \code{\link{CalGR}} function.
\cr\cr
\code{WupPer = NULL} indicates that, if available, a period of one year immediately present before the CalPer period is used. \code{WupPer = 0L} allows to disable the warm up of the model.
}


\author{Olivier Delaigue, Guillaume Thirel}


\examples{
library(airGRteaching)

## data.frame of observed data
data(L0123001, package = "airGR")
BasinObs2 <- BasinObs[, c("DatesR", "P", "E", "Qmm", "T")]

## Preparation of observed data for modelling
PREP <- PrepGR(ObsDF = BasinObs2, HydroModel = "GR4J", CemaNeige = FALSE)

## Calibration step
CAL <- CalGR(PrepGR = PREP, CalCrit = "KGE2",
             WupPer = NULL, CalPer = c("1990-01-01", "1993-12-31"))

## Simulation step using the result of the automatic calibration method to set the model parameters
SIM <- SimGR(PrepGR = PREP, Param = CAL, EffCrit = "KGE2",
             WupPer = NULL, SimPer = c("1994-01-01", "1998-12-31"))

## Simulation step using model parameter set by the user
SIM <- SimGR(PrepGR = PREP, Param = c(270.426, 0.984, 108.853, 2.149), EffCrit = "KGE2",
             WupPer = NULL, SimPer = c("1994-01-01", "1998-12-31"))


## Structure of SimGR object
str(SIM)

## Plot diagnostics
plot(SIM)

## Static plot of observed and simulated time series
plot(SIM, which = "ts")
plot(SIM, which = c("Precip", "Flows"))

## Dynamic plot of observed and simulated time series
dyplot(SIM)
}

