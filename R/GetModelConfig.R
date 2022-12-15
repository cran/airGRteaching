
GetCrit <- function(x, ...) {
  UseMethod("GetCrit")
}

# -----

GetParam <- function(x, ...) {
  UseMethod("GetParam")
}

# -----

GetCrit.CalGR <- function(x, ...) {
  if (!inherits(x, "CalGR")) {
    stop("Non convenient data for x argument. Must be of class \"CalGR\"")
  }
  res <- x$OutputsCalib$CritFinal
  names(res) <- x$OutputsCalib$CritName
  res
}

# -----

GetCrit.SimGR <- function(x, ...) {
  if (!inherits(x, "SimGR")) {
    stop("Non convenient data for x argument. Must be of class \"SimGR\"")
  }
  res <- x$EffCrit$CritValue
  names(res) <- x$EffCrit$CritName
  res
}

# -----

GetParam.CalGR <- function(x, ...) {
  if (!inherits(x, "CalGR")) {
    stop("Non convenient data for x argument. Must be of class \"CalGR\"")
  }
  x$OutputsCalib$ParamFinalR
}

# -----

GetParam.SimGR <- function(x, ...) {
  if (!inherits(x, "SimGR")) {
    stop("Non convenient data for x argument. Must be of class \"SimGR\"")
  }
  x$OutputsModel$RunOptions$Param
}

