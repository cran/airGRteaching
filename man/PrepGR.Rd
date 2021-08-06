\encoding{UTF-8}
\name{PrepGR}
\alias{PrepGR}


\title{Creation of the inputs required to run the CalGR and SimGR functions}


\description{Creation of the inputs required to run the CalGR and SimGR functions}


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

  \item{HydroModel}{[character] name of the hydrological model (must be one of \code{"GR1A"}, \code{"GR2M"}, \code{"GR4J"}, \code{"GR5J"}, \code{"GR6J"}, \code{"GR4H"} or \code{"GR5H"})}

  \item{CemaNeige}{[boolean] option indicating whether CemaNeige should be activated (only available for hourly or daily models, when \code{HydroModel} is equal to any of \code{"GR4J"}, \code{"GR5J"}, \code{"GR6J"}, \code{"GR4H"} or \code{"GR5H"}). See details}
}


\value{
  [list] object of class \code{PrepGR} containing the data required to evaluate the model outputs:
  \item{InputsModel}{[list] object of class \emph{InputsModel} containing the data required to evaluate the model outputs (see: \code{\link[airGR]{CreateInputsModel}} outputs)}
  \item{Qobs}{[numeric] time series of observed discharges [mm/time step]}
  \item{HydroModel}{[character] name of the function of the hydrological model used}
}


\seealso{
airGRteaching
\code{\link{plot}} and  \code{\link{dyplot}}
functions to display static and dynamic plots
\cr\cr
airGR
\code{\link[airGR]{CreateInputsModel}}
function
}


\details{
If the \code{ObsDF} argument is provided, \code{DatesR}, \code{Precip}, \code{PotEvap}, \code{Qobs} and \code{TempMean} are not necessary, and vice-versa. If one variable is provided in \code{ObsDF} and also separately, then only the data included in \code{ObsDF} are used.
\cr\cr
If \code{HydroModel = "GR5H"}, by default, this model is used without the interception store (i.e. without specifying \code{Imax}; see \code{\link[airGR]{RunModel_GR5H}}).
\cr\cr
If the \code{CemaNeige} argument is set to \code{TRUE}, the default version of CemaNeige is used (i.e. without the Linear Hysteresis, see the details part in \code{\link[airGR]{CreateRunOptions}}).
\cr\cr
The \code{PrepGR} function can be used even if no observed discharges are available. In this case, it is necessary to provide observed discharges time series equal to NA: this means that either the \code{ObsDF} observed discharges column or the \code{Qobs} arguments, depending on the format of data you provide, must be provided and filled with NAs.
}

\author{Olivier Delaigue}


\examples{
library(airGRteaching)

## data.frame of observed data
data(L0123001, package = "airGR")
BasinObs2 <- BasinObs[, c("DatesR", "P", "E", "Qmm", "T")]

## Preparation of observed data for modelling when inputs set by using a data.frame
PREP <- PrepGR(ObsDF = BasinObs2, HydroModel = "GR4J", CemaNeige = FALSE)


## Structure of PrepGR object
str(PREP)

## Static plot of observed time series
plot(PREP)

## Dynamic plot of observed time series
dyplot(PREP)


## Preparation of observed data for modelling when inputs set by using independant vectors
PREP <- PrepGR(DatesR = BasinObs2$DatesR, Precip = BasinObs2$P,
               PotEvap = BasinObs2$E, Qobs = BasinObs2$Qmm,
               HydroModel = "GR4J", CemaNeige = FALSE)


## Preparation of observed data for an ungauged catchment (i.e. no observed discharge available)
## Observed discharge set to NA in the Qobs argument
##   or in the 4th column of the data.frame if the ObsDF argument is used
PREP <- PrepGR(DatesR = BasinObs2$DatesR, Precip = BasinObs2$P,
               PotEvap = BasinObs2$E, Qobs = NA,
               HydroModel = "GR4J", CemaNeige = FALSE)
}
