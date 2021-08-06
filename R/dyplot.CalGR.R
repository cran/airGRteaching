dyplot.CalGR <- function(x, ...) {

  if (!inherits(x, "CalGR")) {
    stop("Non convenient data for x argument. Must be of class \"CalGR\"")
  }

  dyplot.default(x, ...)

}
