\encoding{UTF-8}
\name{as.data.frame}
\alias{as.data.frame.PrepGR}
\alias{as.data.frame.CalGR}
\alias{as.data.frame.SimGR}
\alias{as.data.frame}


\title{Function to coerce the outputs of PrepGR, CalGR and SimGR to a data.frame}


\description{Function to coerce the outputs of PrepGR, CalGR and SimGR to a data.frame}


\usage{
\method{as.data.frame}{PrepGR}(x, row.names = NULL, ...)

\method{as.data.frame}{CalGR}(x, row.names = NULL, ...)

\method{as.data.frame}{SimGR}(x, row.names = NULL, ...)
}


\arguments{
  \item{x}{[\code{\link{PrepGR}}], [\code{\link{CalGR}}] or [\code{\link{SimGR}}] objects}
  \item{row.names}{\code{NULL} or a character vector giving the row names for the data.frame. Missing values are not allowed}
  \item{...}{additional arguments to be passed to or from methods}
}


\value{
  [data.frame] containing:
  \item{Dates}{[POSIXct] vector of dates}
  \item{PotEvap}{[numeric] time series of potential evapotranspiration (catchment average) [mm/time step]}
  \item{PrecipObs}{[numeric] time series of total precipitation (catchment average) [mm/time step]}
  \item{PrecipFracSolid_CemaNeige}{[numeric] time series of solid precipitation fraction (layer average) [-],
must be defined if CemaNeige is used}
  \item{TempMeanSim_CemaNeige}{[numeric] time series of mean air temperature (layer average) [°C],
must be defined if CemaNeige is used}
  \item{Qobs}{[numeric] time series of observed flow (for the same time steps than simulated) [mm/time step]}
  \item{Qsim}{[numeric] time series of simulated flow (for the same time steps than simulated) [mm/time step] (only for objects of class \code{\link{CalGR}} or \code{\link{SimGR}})}
}



\seealso{
\code{\link{PrepGR}}, \code{\link{CalGR}}, \code{\link{SimGR}}
}


\author{Olivier Delaigue}


\examples{
library(airGRteaching)

## data.frame of observed data
data(L0123001, package = "airGR")
BasinObs2 <- BasinObs[, c("DatesR", "P", "E", "Qmm", "T")]

## Preparation of observed data for modelling
PREP <- PrepGR(ObsDF = BasinObs2, HydroModel = "GR4J", CemaNeige = FALSE)
head(as.data.frame(PREP))

## Calibration step
CAL <- CalGR(PrepGR = PREP, CalCrit = "KGE2",
             WupPer = NULL, CalPer = c("1990-01-01", "1991-12-31"))
head(as.data.frame(CAL))

## Simulation step using the result of the automatic calibration method to set the model parameters
SIM <- SimGR(PrepGR = PREP, CalGR = CAL, EffCrit = "KGE2",
             WupPer = NULL, SimPer = c("1992-01-01", "1992-12-31"))
head(as.data.frame(SIM))
}

