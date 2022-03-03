PrepGR <- function(ObsDF = NULL, DatesR = NULL, Precip = NULL, PotEvap = NULL, Qobs = NULL, TempMean = NULL,
                   ZInputs = NULL, HypsoData = NULL, NLayers = 5,
                   HydroModel, CemaNeige = FALSE) {


  SuiteGR <- paste0("GR", c("1A", "2M", "4J", "5J", "6J", "4H", "5H"))
  HydroModel <- match.arg(arg = HydroModel, choices = SuiteGR)

  if (is.null(ObsDF) && (is.null(DatesR) | is.null(Precip) | is.null(PotEvap))) {
    stop("Missing input data")
  }

  if (!is.null(ObsDF)) {
    if (ncol(ObsDF) >= 5) {
      TempMean <- ObsDF[, 5L, drop = TRUE]
    }
  }

  if (!is.null(Qobs)) {
    Qobs <- Qobs
  } else {
    Qobs <- NA
  }

  if (!is.null(TempMean)) {
    TempMean <- TempMean
  } else {
    TempMean <- NA
  }

  if (is.null(ObsDF)) {
    ObsDF <- data.frame(DatesR   = DatesR,
                        Precip   = Precip,
                        PotEvap  = PotEvap,
                        Qobs     = Qobs,
                        TempMean = TempMean)
  }

  if (!is.null(ObsDF)) {
    ObsDF <- data.frame(DatesR   = ObsDF[, 1L, drop = TRUE],
                        Precip   = ObsDF[, 2L, drop = TRUE],
                        PotEvap  = ObsDF[, 3L, drop = TRUE],
                        Qobs     = ObsDF[, 4L, drop = TRUE],
                        TempMean = TempMean)
  }

  if (!any(attributes(ObsDF$DatesR[1])$tzone %in%  "UTC")) {
    stop("Non convenient date format. Time zone must be defined as \"UTC\"")
  }

  if (! CemaNeige) {
    TypeModel <- sprintf("RunModel_%s", HydroModel)
  }
  if (CemaNeige && grepl("J|H", HydroModel)) {
    TypeModel <- sprintf("RunModel_CemaNeige%s", HydroModel)
  }
  if (CemaNeige && !grepl("J|H", HydroModel)) {
    warning("CemaNeige can not be used with ", HydroModel)
    TypeModel <- sprintf("RunModel_%s", HydroModel)
  }
  FUN_MOD <- get(TypeModel)


  MOD_obs <- CreateInputsModel(FUN_MOD = FUN_MOD, DatesR = ObsDF$DatesR,
                      Precip = ObsDF$Precip, PotEvap = ObsDF$PotEvap, TempMean = ObsDF$TempMean,
                      ZInputs = ZInputs,  HypsoData = HypsoData, NLayers = NLayers, verbose = FALSE)


  PrepGR <- list(InputsModel = MOD_obs, Qobs = ObsDF$Qobs, TypeModel = TypeModel)
  class(PrepGR) <- c("PrepGR", "GR")
  return(PrepGR)

}
