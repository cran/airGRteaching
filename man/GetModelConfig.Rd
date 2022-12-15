\encoding{UTF-8}
\name{GetModelConfig}
\alias{GetModelConfig}
\alias{GetCrit}
\alias{GetCrit.CalGR}
\alias{GetCrit.SimGR}
\alias{GetParam}
\alias{GetParam.CalGR}
\alias{GetParam.SimGR}


\title{
Get model configuration
}


\description{
Get the error criterion value or the model parameter set from \emph{CalGR} and \emph{SimGR} objects.
}


\usage{
\method{GetCrit}{CalGR}(x, ...)

\method{GetCrit}{SimGR}(x, ...)

\method{GetParam}{CalGR}(x, ...)

\method{GetParam}{SimGR}(x, ...)
}


\arguments{
  \item{x}{[\code{\link{CalGR}}] or [\code{\link{SimGR}}] objects}
  \item{...}{additional arguments to be passed to or from methods}
}


\author{
Olivier Delaigue
}


\seealso{
\code{\link{CalGR}}, \code{\link{SimGR}}
}


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

## Get the error criterion selected as objective function obtained at the end of the calibration
GetCrit(CAL)

## Get the parameter set obtained at the end of the calibration
GetParam(CAL)

## Simulation step using the result of the automatic calibration method to set the model parameters
SIM <- SimGR(PrepGR = PREP, Param = CAL, EffCrit = "KGE2",
             WupPer = NULL, SimPer = c("1994-01-01", "1998-12-31"))

## Get the error criterion computed during the simulation step
GetCrit(SIM)

## Get the parameter set used during the simulation step
GetParam(SIM)
}
