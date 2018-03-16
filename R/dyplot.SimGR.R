dyplot.SimGR <- function(x, ...) {

  if (! any(class(x) %in% "SimGR")) {
    stop("Non convenient data for x argument. Must be of class \"SimGR\"")
  }
  
  dyplot.default(x, ...)

}
