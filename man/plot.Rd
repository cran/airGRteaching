\encoding{UTF-8}
\name{plot}
\alias{plot}
\alias{plot.PrepGR}
\alias{plot.CalGR}
\alias{plot.SimGR}


\title{Static plots for time series of PrepGR, CalGR and SimGR objects}


\description{
Static plots for time series of \emph{PrepGR}, \emph{CalGR} and \emph{SimGR} objects.
Also plot of the evolution  of parameters and objective function during the calibration step for CalGR object.}


\usage{
\method{plot}{PrepGR}(x, type = "l",
     col.Precip = "royalblue", col.Q = "black", col.na = "grey",
     xlab = NULL, ylab = NULL, main = NULL,
     plot.na = TRUE, ...)

\method{plot}{CalGR}(x, xlab = NULL, ylab = NULL, main = NULL,
     which = "synth", log_scale = FALSE, ...)

\method{plot}{SimGR}(x,
     which = "synth", log_scale = FALSE, ...)
}


\arguments{
  \item{x}{[PrepGR], [CalGR] or [SimGR] objects (see \code{\link{PrepGR}}, \code{\link{CalGR}} and \code{\link{SimGR}})}

  \item{type}{[character] the type of plot that should be drawn (see \code{\link[graphics]{plot}} for details)}

  \item{col.Precip}{(optional) [character]color code or name for precipitation, see \code{\link[graphics]{par}}}

  \item{col.Q}{(optional) [character] color code or name for observed flow, see \code{\link[graphics]{par}}}

  \item{col.na}{(optional) [character] color code or name for missing values, see \code{\link[graphics]{par}}}

  \item{xlab}{(optional) [character] a label for the x-axis (see \code{\link[graphics]{title}})}

  \item{ylab}{(optional) [character] a label for the y-axis (vector of 1 or 2 values for rainfall and flow respectively; see \code{\link[graphics]{title}})}

  \item{main}{(optional) [character] a main title for the plot (see \code{\link[graphics]{title}})}

  \item{plot.na}{[boolean] boolean indicating if the missing values are plotted on the x-axis}

  \item{which}{[character] choice of the plot type (\code{"synth"} (default): plot diagnostics; \code{"iter"}: parameter and calibration criterion values during the iterations of the steepest descent step of the airGR calibration algorithm; \code{"ts"}: time series of observed precipitation and observed and simulated flows; \code{"perf"}: error time series and other graphic to compare simulated to observed flows; for other graphical outputs, see \code{\link[airGR]{plot.OutputsModel}})}

\item{log_scale}{(optional) [boolean] indicating if the flow and the error time series axis and the flow error time series axis are to be logarithmic, default = \code{FALSE}}

  \item{...}{other parameters to be passed through to plotting functions}
}


\seealso{
airGR
\code{\link[airGR]{plot.OutputsModel}}
function
\cr\cr
airGRteaching
\code{\link{dyplot}}
function to display dynamic plots
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

## Observed data plotting
plot(PREP)
}

