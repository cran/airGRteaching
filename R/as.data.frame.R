as.data.frame.PrepGR <- function(x, row.names = NULL, ...) {

  # if (!(inherits(x, "PrepGR"))) {
  #   stop("'InputsCrit' must be of class 'PrepGR'")
  # }

  TMGR <- .TypeModelGR(x)
  myGR <- list()

  myGR$FracSolid <- NA
  myGR$TempMean  <- NA

  if (TMGR$CemaNeige) {
    PrecipSol <- rowMeans(as.data.frame(x$InputsModel$LayerPrecip) * as.data.frame(x$InputsModel$LayerFracSolidPrecip), na.rm = TRUE)
    PrecipSim <- rowMeans(as.data.frame(x$InputsModel$LayerPrecip), na.rm = TRUE)
    FracSolid <- PrecipSol / PrecipSim
    FracSolid <- ifelse(is.na(FracSolid) & PrecipSol == 0 & PrecipSim == 0, 0, FracSolid)
    myGR$FracSolid <- FracSolid
    TempMean  <- rowMeans(as.data.frame(x$InputsModel$LayerTempMean), na.rm = TRUE)
    myGR$TempMean  <- TempMean
  }

  myGR$DatesR  <- x$InputsModel$DatesR
  myGR$PotEvap <- x$InputsModel$PotEvap
  myGR$Precip  <- x$InputsModel$Precip
  myGR$Qobs    <- x$Qobs
  myGR$Qsim    <- NA

  TabSim <- data.frame(Dates                     = myGR$DatesR,
                       PotEvap                   = myGR$PotEvap,
                       PrecipObs                 = myGR$Precip,
                       PrecipFracSolid_CemaNeige = myGR$FracSolid,
                       TempMeanSim_CemaNeige     = myGR$TempMean,
                       Qobs                      = myGR$Qobs,
                       Qsim                      = myGR$Qsim)
  as.data.frame(x = TabSim, row.names = row.names, ...)
}


as.data.frame.CalGR <- function(x, row.names = NULL, ...) {

  # if (inherits(x, "CalGR") | inherits(x, "SimGR"))) {
  #   stop("'InputsCrit' must be of class 'CalGR', 'SimGR'")
  # }

  TMGR <- .TypeModelGR(x)
  myGR <- list()

  myGR$FracSolid <- NA
  myGR$TempMean  <- NA

  if (TMGR$CemaNeige) {
    PrecipSol <- rowMeans(sapply(x$OutputsModel$CemaNeigeLayers, "[[", "Psol"), na.rm = TRUE)
    PrecipSim <- rowMeans(sapply(x$OutputsModel$CemaNeigeLayers, "[[", "Pliq"), na.rm = TRUE) + PrecipSol
    FracSolid <- PrecipSol / PrecipSim
    FracSolid <- ifelse(is.na(FracSolid) & PrecipSol == 0 & PrecipSim == 0, 0, FracSolid)
    myGR$FracSolid <- FracSolid
    TempMean  <- rowMeans(sapply(x$OutputsModel$CemaNeigeLayers, "[[", "Temp"), na.rm = TRUE)
    myGR$TempMean  <- TempMean
  }

  myGR$DatesR  <- x$OutputsModel$DatesR
  myGR$PotEvap <- x$OutputsModel$PotEvap
  myGR$Precip  <- x$OutputsModel$Precip
  myGR$Qobs    <- x$Qobs
  myGR$Qsim    <- x$OutputsModel$Qsim

  TabSim <- data.frame(Dates                     = myGR$DatesR,
                       PotEvap                   = myGR$PotEvap,
                       PrecipObs                 = myGR$Precip,
                       PrecipFracSolid_CemaNeige = myGR$FracSolid,
                       TempMeanSim_CemaNeige     = myGR$TempMean,
                       Qobs                      = myGR$Qobs,
                       Qsim                      = myGR$Qsim)
  as.data.frame(x = TabSim, row.names = row.names, ...)
}


as.data.frame.SimGR  <- as.data.frame.CalGR


