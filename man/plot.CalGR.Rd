\encoding{UTF-8}
\name{plot.CalGR}
\alias{plot.CalGR}
\title{Time series plotting of GR calibration objects}


\usage{
\method{plot}{CalGR}(x, xlab = NULL, ylab = NULL, main = NULL, which = c("perf", "iter", "ts"), ...)
}


\arguments{
  \item{x}{[object of class \emph{CalGR}] see \code{\link{CalGR}} for details}
  
  \item{xlab}{(optional) [character] a label for the x-axis when \code{which = "iter"} (see \code{\link{title}})}
  
  \item{ylab}{(optional) [character] a label for the y-axis when \code{which = "iter"} (vector of 1 or 2 values for flow and rainfall respectively; see \code{\link{title}})}
  
  \item{main}{(optional) [character] a main title for the plot (see \code{\link{title}})}
  
  \item{which}{[character] choice of the plot type (\code{"perf"} (default): plot diagnostics; \code{"iter"}: parameter and calibration criterion values during the iterations of the steepest descent step of the airGR calibration algorithm; \code{"ts"}: time series of observed precipitation and observed and simulated flows)}
  
  \item{...}{other parameters to be passed through to plotting functions}
}


\description{Time series plotting of GR calibration objects}


\seealso{
\code{\link{CalGR}}, \code{\link{plot.OutputsModel}}
}


\author{Olivier Delaigue}


\examples{
library(airGR)

## data.frame of observed data
data(L0123001)
BasinObs2 <- BasinObs[, c("DatesR", "P", "E", "Qmm", "T")]

## Preparation of observed data for modelling
PREP <- PrepGR(ObsDF = BasinObs2, HydroModel = "GR5J", CemaNeige = TRUE)

## Calibration step
CAL <- CalGR(PrepGR = PREP, CalCrit = "KGE2",
             WupPer = NULL, CalPer = c("1990-01-01", "1993-12-31"))
plot(CAL, which = "perf")
plot(CAL, which = "ts")
plot(CAL, which = "iter")
}

