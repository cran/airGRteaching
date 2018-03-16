\encoding{UTF-8}
\name{PrepGR}
\alias{PrepGR}
\title{Creation of the inputs required to run the CalGR and SimGR functions}


\usage{
PrepGR(ObsDF = NULL, DatesR = NULL, Precip = NULL, PotEvap = NULL,
       Qobs = NULL, TempMean = NULL,
       ZInputs = NULL, HypsoData = NULL, NLayers = 5,
       HydroModel, CemaNeige = FALSE)
}


\arguments{
  \item{ObsDF}{(optional) [data.frame] data.frame of dates, total precipitation, potential evapotranspiration, observed discharges and mean air temperature (only if CemaNeige is used) (variables must be in this order; see below for the units)}
  
  \item{DatesR}{(optional) [POSIXt] vector of dates required to create the GR and CemaNeige (if used) models inputs. Time zone must be defined as "UTC"}
  
  \item{Precip}{(optional) [numeric] time series of total precipitation (catchment average) [mm/time step], required to create the GR and CemaNeige (if used) models inputs}
  
  \item{PotEvap}{(optional) [numeric] time series of potential evapotranspiration (catchment average) [mm/time step], required to create the GR model inputs}
  
  \item{Qobs}{(optional) [numeric] time series of observed discharges [mm/time step]}
  
  \item{TempMean}{(optional) [numeric] time series of mean air temperature [°C], required to create the CemaNeige model inputs}
  
  \item{ZInputs}{(optional) [numeric] real giving the mean elevation of the Precip and TempMean series (before extrapolation) [m], possibly used to create the CemaNeige (if used) model inputs}
  
  \item{HypsoData}{(optional) [numeric] vector of 101 reals: min, q01 to q99 and max of catchment elevation distribution [m]; if not defined a single elevation is used for CemaNeige (if used)}
  
  \item{NLayers}{(optional) [numeric] integer giving the number of elevation layers requested [-], required to create CemaNeige (if used) model inputs}
  
  \item{HydroModel}{[character] name of the hydrological model (must be one of \code{"GR1A"}, \code{"GR2M"}, \code{"GR4J"}, \code{"GR5J"}, \code{"GR6J"} or \code{"GR4H"})}
  
  \item{CemaNeige}{[boolean] option indicating whether CemaNeige should be activated (only available when \code{HydroModel} is equal to any of \code{"GR4J"}, \code{"GR5J"} or \code{"GR6J"})}
}


\value{
  [list] object of class \code{PrepGR} containing the data required to evaluate the model outputs:
  \item{InputsModel}{[list] object of class \emph{InputsModel} containing the data required to evaluate the model outputs (see: \code{\link{CreateInputsModel}} outputs)}
  \item{Qobs}{[numeric] time series of observed discharges [mm/time step]}
  \item{HydroModel}{[character] name of the function of the hydrological model used}
}


\seealso{
\code{\link{CreateInputsModel}}
}


\description{Creation of the inputs required to run the CalGR and SimGR functions}

\details{
If the \code{ObsDF} argument is provided, \code{DatesR}, \code{Precip}, \code{PotEvap}, \code{Qobs} and \code{TempMean} are not necessary, and vice-versa. If one variable is provided in \code{ObsDF} and also separately, then only the data included in \code{ObsDF} are used. 
}

\author{Olivier Delaigue}


\examples{
library(airGR)

## data.frame of observed data
data(L0123001)
BasinObs2 <- BasinObs[, c("DatesR", "P", "E", "Qmm", "T")]

## Preparation of observed data for modelling
PREP <- PrepGR(ObsDF = BasinObs2, HydroModel = "GR4J", CemaNeige = FALSE)
str(PREP)
}

