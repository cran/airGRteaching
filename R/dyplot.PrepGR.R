dyplot.PrepGR <- function(x, ...) {

  if (! any(class(x) %in% "PrepGR")) {
    stop("Non convenient data for x argument. Must be of class \"PrepGR\"")
  }
  
  dyplot.default(x, ...)
  
}
