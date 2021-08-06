dyplot.PrepGR <- function(x, ...) {

  if (!inherits(x, "PrepGR")) {
    stop("Non convenient data for x argument. Must be of class \"PrepGR\"")
  }

  dyplot.default(x, ...)

}
