dyplot.CalGR <- function(x, ...) {

  if (! any(class(x) %in% "CalGR")) {
    stop("Non convenient data for x argument. Must be of class \"CalGR\"")
  }
  
  dyplot.default(x, ...)
  
}
