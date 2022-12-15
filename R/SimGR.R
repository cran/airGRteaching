SimGR <- function(PrepGR, CalGR = NULL, Param, EffCrit = c("NSE", "KGE", "KGE2", "RMSE"),
                  WupPer = NULL, SimPer, transfo = c("", "sqrt", "log", "inv", "sort"), verbose = TRUE) {

  EffCrit <- match.arg(arg = EffCrit)
  EffCrit <- sprintf("ErrorCrit_%s", EffCrit)
  FUN_CRIT <- get(EffCrit)

  if (!any(transfo %in% c("", "sqrt", "log", "inv", "sort"))) {
    stop("Non convenient transformation \"transfo\"")
  } else {
    transfo <- transfo[1L]
  }


  if (!inherits(PrepGR, "PrepGR")) {
    stop("Non convenient data for argument \"PrepGR\". Must be of class \"PrepGR\"")
  }

  if (!missing(CalGR)) {
    warning("Deprecated \"CalGR\" argument. Use \"Param\" instead")
  }
  ### to remove when the CalGR will be removed
  if (missing(Param)) {
    Param <- NULL
  }
  if (!inherits(CalGR, "CalGR") & !is.null(CalGR)) {
    stop("Non convenient data  for argument \"CalGR\". Must be of class \"CalGR\"")
  }
  if (is.null(CalGR) & is.null(Param)) {
    stop("Arguments \"CalGR\" and \"Param\" are missing, with no default. You must fill in one of these two arguments")
  }
  if (is.null(Param)) {
    Param <- CalGR$OutputsCalib$ParamFinalR
  }
  ###
  if (inherits(Param, "CalGR")) {
    Param <- Param$OutputsCalib$ParamFinalR
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

  SimPer <- as.POSIXct(SimPer, tz = "UTC")
  if (length(SimPer) != 2) {
    stop("Simulation period \"SimPer\" must be of length 2")
  }
  if (any(is.na(SimPer))) {
    stop("Non convenient date format for the simulation period \"SimPer\"")
  } else {
    if (!any(PrepGR$InputsModel$DatesR == SimPer[1]) | !any(PrepGR$InputsModel$DatesR == SimPer[2])) {
      stop("Non convenient date for the simulation period \"SimPer\"")
    } else {
      SimInd <- which(PrepGR$InputsModel$DatesR == SimPer[1]):which(PrepGR$InputsModel$DatesR == SimPer[2])
    }
  }


  MOD_opt <- CreateRunOptions(FUN_MOD = get(PrepGR$TypeModel), InputsModel = PrepGR$InputsModel,
                              IndPeriod_WarmUp = WupInd, IndPeriod_Run = SimInd, verbose = verbose)


  # NA in Qobs
  isQobs <- !all(is.na(PrepGR$Qobs))
  isQobsSimPer <- !all(is.na(PrepGR$Qobs[SimInd]))
  if (!isQobs) {
    warning("\"PrepGR\" does not contain any Qobs values. The efficiency criterion is not computed")
  } else if (!isQobsSimPer) {
    message("\"PrepGR\" does not contain any Qobs values on \"SimPer\". The efficiency criterion is not computed")
  }


  SIM <- RunModel(InputsModel = PrepGR$InputsModel, RunOptions = MOD_opt,
                  Param = Param, FUN_MOD = get(PrepGR$TypeModel))


  if (isQobsSimPer) {
    MOD_crt <- CreateInputsCrit(FUN_CRIT = FUN_CRIT, InputsModel = PrepGR$InputsModel,
                                RunOptions = MOD_opt, Obs = PrepGR$Qobs[SimInd], transfo = transfo)
    CRT <- ErrorCrit(InputsCrit = MOD_crt, OutputsModel = SIM, verbose = verbose)
  } else {
    MOD_crt <- NULL
    CRT <- NULL
  }


  SimGR <- list(OptionsSimul = MOD_opt, OptionsCrit = MOD_crt, OutputsModel = SIM, Qobs = PrepGR$Qobs[SimInd],
                TypeModel = PrepGR$TypeModel,
                CalCrit = CalGR$CalCrit, EffCrit = CRT,
                PeriodModel = list(WarmUp = as.POSIXct(PrepGR$InputsModel$DatesR[range(MOD_opt$IndPeriod_WarmUp)], tz = "UTC"),
                                   Run    = SimPer))
  class(SimGR) <- c("SimGR", "GR")
  return(SimGR)

}
