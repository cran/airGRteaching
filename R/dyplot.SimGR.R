dyplot.SimGR <- function(x, ...) {

  if (!inherits(x, "SimGR")) {
    stop("Non convenient data for x argument. Must be of class \"SimGR\"")
  }

  dyplot.default(x, ...)

}
