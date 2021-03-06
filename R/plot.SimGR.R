plot.SimGR <- function(x, ...) {

  if (! any(class(x) %in% "SimGR")) {
    stop("Non convenient data for x argument. Must be of class \"SimGR\"")
  }

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  plot(x$OutputsModel, Qobs = x$Qobs, ...)

}
