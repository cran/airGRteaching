\encoding{UTF-8}
\name{plot.PrepGR}
\alias{plot.PrepGR}
\title{Time series plotting GR observation objects}


\usage{
\method{plot}{PrepGR}(x, type = "l", col.Precip = "royalblue", col.Q = "black", col.na = "grey",
  xlab = NULL, ylab = NULL, main = NULL,
  plot.na = TRUE, ...)
}


\arguments{
  \item{x}{[object of class \emph{PrepGR}] see \code{\link{PrepGR}} for details}
  
  \item{type}{[character] the type of plot that should be drawn (see \code{\link{plot}} for details)}
  
  \item{col.Precip}{(optional) [character]color code or name for precipitation, see \code{\link{par}}}
  
  \item{col.Q}{(optional) [character] color code or name for observed flow, see \code{\link{par}}}
  
  \item{col.na}{(optional) [character] color code or name for missing values, see \code{\link{par}}}
  
  \item{xlab}{(optional) [character] a label for the x-axis (see \code{\link{title}})}
  
  \item{ylab}{(optional) [character] a label for the y-axis (vector of 1 or 2 values for rainfall and flow respectively; see \code{\link{title}})}
  
  \item{main}{(optional) [character] a main title for the plot (see \code{\link{title}})}
  
  \item{plot.na}{[boolean] boolean indicating if the missing values are plotted on the x-axis}
  
  \item{...}{other parameters to be passed through to plotting functions}
}


\seealso{
\code{\link{PrepGR}}
}


\description{Time series plotting GR observation objects}


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

