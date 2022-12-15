CalGR <- function(PrepGR, CalCrit = c("NSE", "KGE", "KGE2", "RMSE"),
                  WupPer = NULL, CalPer, transfo = c("", "sqrt", "log", "inv", "sort"), verbose = TRUE) {

  CalCrit <- match.arg(arg = CalCrit)
  CalCrit <- sprintf("ErrorCrit_%s", CalCrit)
  FUN_CRIT <- get(CalCrit)

  if (!any(transfo %in% c("", "sqrt", "log", "inv", "sort"))) {
    stop("Non convenient transformation \"transfo\"")
  } else {
    transfo <- transfo[1L]
  }


  if (!inherits(PrepGR, "PrepGR")) {
    stop("Non convenient data for argument \"PrepGR\". Must be of class \"PrepGR\"")
  }

  isQobs <- !all(is.na(PrepGR$Qobs))
  if (!isQobs) {
    stop("\"PrepGR\" does not contain any Qobs values. It is not possible to calibrate the model")
  }

  WupInd <- NULL
  if (!is.null(WupPer)) {
    if (!identical(WupPer, 0L)) {
      WupPer <- as.POSIXct(WupPer, tz = "UTC")
      if (length(WupPer) != 2) {
        stop("Warm-up period \"WupPer\" must be of length 2")
      }
      if (any(is.na(WupPer))) {
        stop("Non convenient date format for the warm-up period \"WupPer\"")
      } else {
        if (!any(PrepGR$InputsModel$DatesR == WupPer[1]) | !any(PrepGR$InputsModel$DatesR == WupPer[2])) {
          stop("Non convenient date for the warm-up period \"WupPer\"")
        } else {
          WupInd <- which(PrepGR$InputsModel$DatesR == WupPer[1]):which(PrepGR$InputsModel$DatesR == WupPer[2])
        }
      }
    } else {
      WupInd <- 0L
    }
  }

  CalPer <- as.POSIXct(CalPer, tz = "UTC")
  if (length(CalPer) != 2) {
    stop("Calibration period \"CalPer\" must be of length 2")
  }
  if (any(is.na(CalPer))) {
    stop("Non convenient date format for the calibration period \"CalPer\"")
  } else {
    if (!any(PrepGR$InputsModel$DatesR == CalPer[1]) | !any(PrepGR$InputsModel$DatesR == CalPer[2])) {
      stop("Non convenient date for the calibration period \"CalPer\"")
    } else {
      CalInd <- which(PrepGR$InputsModel$DatesR == CalPer[1]):which(PrepGR$InputsModel$DatesR == CalPer[2])
    }
  }



  MOD_opt <- CreateRunOptions(FUN_MOD = get(PrepGR$TypeModel), InputsModel = PrepGR$InputsModel,
                              IndPeriod_WarmUp = WupInd, IndPeriod_Run = CalInd, verbose = TRUE)


  MOD_crt <- CreateInputsCrit(FUN_CRIT = FUN_CRIT, InputsModel = PrepGR$InputsModel,
                              RunOptions = MOD_opt, Obs = PrepGR$Qobs[CalInd], transfo = transfo)


  CAL_opt <- CreateCalibOptions(FUN_MOD = get(PrepGR$TypeModel), FUN_CALIB = Calibration_Michel)


  CAL <- Calibration(InputsModel = PrepGR$InputsModel, RunOptions = MOD_opt,
                     InputsCrit = MOD_crt,  CalibOptions = CAL_opt,
                     FUN_MOD = get(PrepGR$TypeModel),
                     FUN_CALIB = Calibration_Michel, verbose = verbose)


  SIM <- RunModel(InputsModel = PrepGR$InputsModel, RunOptions = MOD_opt,
                  Param = CAL$ParamFinalR, FUN_MOD = get(PrepGR$TypeModel))


  CalGR <- list(OptionsCalib = MOD_opt, Qobs = PrepGR$Qobs[CalInd],
                OutputsCalib = CAL, OutputsModel = SIM,
                TypeModel = PrepGR$TypeModel, CalCrit = CalCrit,
                PeriodModel = list(WarmUp = as.POSIXct(PrepGR$InputsModel$DatesR[range(MOD_opt$IndPeriod_WarmUp)], tz = "UTC"),
                                   Run    = CalPer))
  class(CalGR) <- c("CalGR", "GR")
  return(CalGR)

}
