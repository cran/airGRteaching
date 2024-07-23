\encoding{UTF-8}
\name{ShinyGR}
\alias{ShinyGR}


\title{Interactive Web application to run manually the GR2M, GR4J, GR5J and GR6J hydrological models with or without CemaNeige}


\description{Shiny application to understand and to display in a interactive way the impact of each parameter of the GR models on the simulated flows}


\usage{
ShinyGR(ObsDF = NULL,
        DatesR = NULL, Precip = NULL, PotEvap = NULL, Qobs = NULL, TempMean = NULL,
        ZInputs = NULL, HypsoData = NULL, NLayers = 5,
        SimPer, NamesObsBV = NULL, theme = "RStudio")
}


\arguments{
  \item{ObsDF}{(optional) [data.frame or list of data.frame] \code{data.frame} of dates, total precipitation, potential evapotranspiration, observed discharge and mean air temperature (only if CemaNeige is used) (variables must be in this order; see below for the units)}

  \item{DatesR}{(optional) [POSIXt] vector of daily or monthly dates required to create the GR and CemaNeige models inputs. Time zone must be defined as "UTC"}

  \item{Precip}{(optional) [numeric] time series of total precipitation (catchment average) [mm/time step], required to create the GR and CemaNeige models inputs}

  \item{PotEvap}{(optional) [numeric] time series of potential evapotranspiration (catchment average) [mm/time step], required to create the GR model inputs}

  \item{Qobs}{(optional) [numeric] time series of observed discharge [mm/time step]}

  \item{TempMean}{(optional) [numeric] time series of mean air temperature [°C], required to create the CemaNeige model inputs (if used)}

  \item{ZInputs}{(optional) [numeric or list of numerics] real giving the mean elevation of the Precip and TempMean series (before extrapolation) [m], used to create the CemaNeige model inputs (if used)}

  \item{HypsoData}{(optional) [numeric or list of numerics] vector of 101 reals: min, q01 to q99 and max of catchment elevation distribution [m]; if not defined a single elevation is used for CemaNeige (if used)}

  \item{NLayers}{(optional) [numeric or list of numerics] integer giving the number of elevation layers requested [-], required to create CemaNeige model inputs (if used)}

  \item{SimPer}{[character or list of characters] vector of 2 values to define the beginning and the end of the simulation period [\code{"YYYY-mm-dd"} or \code{"YYYY-mm-dd HH:MM:SS"}], see below for details}

  \item{NamesObsBV}{(optional) [character] vector of values to define the data inputs name(s) (if the ObsDF list is not already named)}

  \item{theme}{(optional) [character] alternative stylesheet [\code{"RStudio"} (default), \code{"Cerulean"}, \code{"Cyborg"}, \code{"Flatly"}, \code{"Inrae"}, \code{"Saclay"}, \code{"United"} or \code{"Yeti"}]}
}


\seealso{
\code{\link{CalGR}, \link{SimGR}, \link[airGRteaching]{plot}}
}


\details{
The warm-up period always starts from the first date of the dataset to the time step just before the beginning of the simulation period (\code{SimPer}). \cr
The \code{ShinyGR} function can be used even if no observed discharges are available. In this case, it is necessary to provide observed discharges time series equal to NA: this means that either the \code{ObsDF} observed discharges column or the \code{Qobs} arguments, depending on the format of data you provide, must be provided and filled with NAs. \cr
Several datasets can be proposed at the same time in the interface (see the code example below). A dataset with a daily time step can be proposed at the same time as a dataset at the monthly time step. \cr
CemaNeige can only be used with the daily models at the moment.
}

\note{
A demo version of the GUI is available on the \href{https://sunshine.inrae.fr/}{Sunshine} platform.
}


\author{
Olivier Delaigue,
Laurent Coron,
Pierre Brigode}

\examples{
library(airGRteaching)

## data.frame of daily observed data of a low-land basin
data(L0123001, package = "airGR")
BV_L0123001 <- BasinObs[0001:6000, c("DatesR", "P", "E", "Qmm", "T")]
BI_L0123001 <- BasinInfo

## data.frame of daily observed data of a mountainous basin
data(L0123002, package = "airGR")
BV_L0123002 <- BasinObs[5000:9999, c("DatesR", "P", "E", "Qmm", "T")]
BI_L0123002 <- BasinInfo

## data.frame of monthly aggregated time series from daily observed data of a low-land basin
BV_L0123001m <- SeriesAggreg(BV_L0123001[BV_L0123001$DatesR < "2000-06-01", ],
                             Format = "\%Y\%m", ConvertFun = c("sum", "sum", "sum", "mean"))


## Interactive simulation when inputs set by using a data.frame
\donttest{if (interactive()) {
  ShinyGR(ObsDF = list("Low-land basin" = BV_L0123001, "Mountainous basin" = BV_L0123002),
          ZInputs = list(NULL, median(BI_L0123002$HypsoData)),
          HypsoData = list(NULL, BI_L0123002$HypsoData),
          NLayers = list(5, 5),
          SimPer = list(c("1994-01-01", "1998-12-31"), c("2004-01-01", "2006-12-31")),
          theme = "United")
}}

## Interactive simulation using when inputs set by using independant vectors
\donttest{if (interactive()) {
  ShinyGR(DatesR = BV_L0123002$DatesR,
          Precip = BV_L0123002$P,
          PotEvap = BV_L0123002$E,
          Qobs = BV_L0123002$Qmm,
          Temp = BV_L0123002$T,
          ZInputs = median(BI_L0123002$HypsoData),
          HypsoData = BI_L0123002$HypsoData,
          NLayers = 5,
          SimPer = c("2004-01-01", "2006-12-31"),
          NamesObsBV = "Mountainous basin",
          theme = "Saclay")
}}

## Interactive simulation for an ungauged catchment (i.e. no observed discharge available)
## Observed discharge set to NA in the Qobs argument
##   or in the 4th column of the data.frame if the ObsDF argument is used
\donttest{if (interactive()) {
  ShinyGR(DatesR = BV_L0123001$DatesR,
          Precip = BV_L0123001$P,
          PotEvap = BV_L0123001$E,
          Qobs = NA,
          SimPer = c("1994-01-01", "1998-12-31"),
          NamesObsBV = "Low-land basin",
          theme = "Cyborg")
}}

## Interactive simulation when inputs are at different time steps
\donttest{if (interactive()) {
  ShinyGR(ObsDF = list("Low-land basin [daily]" = BV_L0123001,
                       "Low-land basin [monthly]" = BV_L0123001m),
        SimPer = list(c("1994-01-01", "1998-12-01"),
                      c("1994-01-01", "1998-12-01")),
        theme = "Flatly")
}}
}

