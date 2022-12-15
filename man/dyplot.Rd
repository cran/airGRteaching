\encoding{UTF-8}
\name{dyplot}
\alias{dyplot}
\alias{dyplot.PrepGR}
\alias{dyplot.CalGR}
\alias{dyplot.SimGR}

\title{Interactive plots for time series of PrepGR, CalGR and SimGR objects}


\description{Interactive plots for time series of \emph{PrepGR}, \emph{CalGR} and \emph{SimGR} objects.}


\usage{
\method{dyplot}{PrepGR}(x, Qsup = NULL, Qsup.name = "Qsup",
       col.Precip = c("royalblue", "lightblue"),
       col.Q = c("black", "orangered", "grey"), col.na = "lightgrey",
       ylab = NULL, main = NULL,
       plot.na = TRUE, RangeSelector = TRUE, Roller = FALSE,
       LegendShow = c("follow", "auto", "always", "onmouseover", "never"), ...)

\method{dyplot}{CalGR}(x, Qsup = NULL, Qsup.name = "Qsup",
       col.Precip = c("royalblue", "lightblue"),
       col.Q = c("black", "orangered", "grey"), col.na = "lightgrey",
       ylab = NULL, main = NULL,
       plot.na = TRUE, RangeSelector = TRUE, Roller = FALSE,
       LegendShow = c("follow", "auto", "always", "onmouseover", "never"), ...)

\method{dyplot}{SimGR}(x, Qsup = NULL, Qsup.name = "Qsup",
       col.Precip = c("royalblue", "lightblue"),
       col.Q = c("black", "orangered", "grey"), col.na = "lightgrey",
       ylab = NULL, main = NULL,
       plot.na = TRUE, RangeSelector = TRUE, Roller = FALSE,
       LegendShow = c("follow", "auto", "always", "onmouseover", "never"), ...)
}


\arguments{
  \item{x}{[\code{\link{PrepGR}}], [\code{\link{CalGR}}] or [\code{\link{SimGR}}] objects}

  \item{Qsup}{(optional) [numeric] additional time series of flows (at the same time step than argument \code{x}) [mm/time step]}

  \item{Qsup.name}{(optional) [character] a label for the legend of Qsup}

  \item{col.Precip}{(optional) [character] vector of 1 (total precip.) or 2 (liquid and solid precip. with CemaNeige) color codes or names for precipitation (these can be of the form \code{"#RRGGBB"} or \code{"rgb(255, 100, 200)"} or \code{"yellow"}), see \code{\link[graphics]{par}} and \code{\link[grDevices]{rgb}}}

  \item{col.Q}{(optional) [character] vector of up to 3 color codes or names for observed (first value), simulated (second value, if provided) and additional (last value, if provided) flows, respectively (these can be of the form \code{"#RRGGBB"} or \code{"rgb(255, 100, 200)"} or \code{"yellow"}), see \code{\link[graphics]{par}} and \code{\link[grDevices]{rgb}}}

  \item{col.na}{(optional) [character] color code or name for missing values(these can be of the form \code{"#RRGGBB"} or \code{"rgb(255, 100, 200)"} or \code{"yellow"}), see \code{\link[graphics]{par}} and \code{\link[grDevices]{rgb}}}

  \item{ylab}{(optional) [character] a label for the y-axis (flow and precipitation)}

  \item{main}{(optional) [character] a main title for the plot}

  \item{plot.na}{[boolean] indicating if the missing values are plotted on the x-axis}

  \item{RangeSelector}{(optional) [boolean] add a range selector to the bottom of the chart that allows users to pan and zoom to various date ranges (see \code{\link[dygraphs]{dyRangeSelector}})}

  \item{Roller}{(optional) [numeric] number of time scale units (e.g. days, months, years) to average values over (see \code{\link[dygraphs]{dyRoller}})}

  \item{LegendShow}{(optional) [character] when to display the legend. Specify \code{"always"} to always show the legend. Specify \code{"onmouseover"} to only display it when a user mouses over the chart. Specify \code{"follow"} (default) to have the legend show as overlay to the chart which follows the mouse. See \code{\link[dygraphs]{dyLegend}}}

  \item{...}{other parameters to be passed through to plotting functions}
}


\seealso{
airGRteaching static \code{\link{plot}} functions
\cr\cr
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
\donttest{dyplot(PREP, main = "Observation")}

## Calibration step
CAL <- CalGR(PrepGR = PREP, CalCrit = "KGE2",
             WupPer = NULL, CalPer = c("1990-01-01", "1993-12-31"))
\donttest{dyplot(CAL, main = "Calibration")}

## Simulation
SIM <- SimGR(PrepGR = PREP, CalGR = CAL, EffCrit = "KGE2",
             WupPer = NULL, SimPer = c("1994-01-01", "1998-12-31"))
\donttest{dyplot(SIM, main = "Simulation")}
}
