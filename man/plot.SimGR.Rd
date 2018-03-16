\encoding{UTF-8}
\name{plot.SimGR}
\alias{plot.SimGR}
\title{Synthetic plotting of of GR model outputs}


\usage{
\method{plot}{SimGR}(x, ...)
}


\arguments{
\item{x}{[object of class \emph{SimGR}] see \code{\link{SimGR}} for details}

\item{...}{other parameters to be passed through to plotting functions}
}


\value{
Screen plot window
}


\description{
Function that creates a screen plot giving an overview of the GR model outputs
}


\details{
Dashboard of results including various graphs (depending on the model): \cr
(1) time series of total precipitation and simulated flows (and observed flows if provided) \cr
(2) interannual median monthly simulated flow (and observed flows if provided) \cr
(3) correlation plot between simulated and observed flows (if observed flows provided) \cr
(4) cumulative frequency plot for simulated flows (and observed flows if provided)
}


\seealso{
\code{\link{SimGR}}, \code{\link{plot.OutputsModel}}
}


\examples{
library(airGR)

## data.frame of observed data
data(L0123001)
BasinObs2 <- BasinObs[, c("DatesR", "P", "E", "Qmm", "T")]

## Preparation of observed data for modelling
PREP <- PrepGR(ObsDF = BasinObs2, HydroModel = "GR4J", CemaNeige = FALSE)

## Calibration step
CAL <- CalGR(PrepGR = PREP, CalCrit = "KGE2",
             WupPer = NULL, CalPer = c("1990-01-01", "1993-12-31"))
       
## Simulation step using the result of the automatic calibration method to set the model parameters
SIM <- SimGR(PrepGR = PREP, CalGR = CAL, EffCrit = "KGE2",
             WupPer = NULL, SimPer = c("1994-01-01", "1998-12-31"))
plot(SIM)                        
}


\author{
Olivier Delaigue, Laurent Coron
}

