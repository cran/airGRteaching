ShinyGR <- function(ObsDF = NULL, DatesR = NULL, Precip = NULL, PotEvap = NULL, Qobs = NULL, TempMean = NULL,
                    ZInputs = NULL, HypsoData = NULL, NLayers = 5, SimPer, NamesObsBV = NULL,
                    theme = "RStudio") {

  .onAttach()

  message("\n-----------------------------------------------------------------------")
  message("\n   Press the 'escape' key to disconnect the graphical user interface \n")
  message("-----------------------------------------------------------------------\n")

  theme <- match.arg(arg = theme,
                     choices = c("RStudio", "Cerulean", "Cyborg", "Flatly", "Inrae", "Saclay", "United", "Yeti"))

  if ((is.null(ObsDF) | any(sapply(ObsDF, is.null))) && (is.null(DatesR) | is.null(Precip) | is.null(PotEvap) | is.null(Qobs))) {
    stop("Missing input data")
  }

  if (is.null(SimPer) | any(sapply(SimPer, is.null))) {
    stop("Null values non suitable for 'SimPer'.")
  }

  if (!is.null(ObsDF)) {
    if (!is.list(ObsDF) | inherits(ObsDF, "PrepGR")) {
      stop("'ObsDF' must be a (list of) 'data.frame'.")
    }
  }
  if (is.data.frame(ObsDF)) {
    ObsDF <- list(ObsDF)
  }

  if (!is.list(HypsoData)) {
    HypsoData <- list(HypsoData)
  }

  if (!is.list(SimPer)) {
    SimPer <- list(SimPer)
  }

  if (is.null(ObsDF)) {
    lenObsDF <- 1L
  } else {
    lenObsDF <- length(ObsDF)
  }

  if (is.null(names(ObsDF)) & !is.null(ObsDF)) {
    if (is.null(NamesObsBV)) {
      NamesObsBV <- paste0("%s %0", nchar(lenObsDF), "d")
      NamesObsBV <- sprintf(NamesObsBV, rep("Unnamed watershed", times = lenObsDF), seq_along(ObsDF))
    } else if (lenObsDF > length(NamesObsBV)) {
      warning("Not enough 'NamesObsBV' elements. Elements recycled.")
      NamesObsBV <- paste0("%s %0", nchar(lenObsDF), "d")
      NamesObsBV <- sprintf(NamesObsBV, rep("Unnamed watershed", times = lenObsDF), seq_along(ObsDF))
    } else if (lenObsDF < length(NamesObsBV)) {
      warning("Too long 'NamesObsBV'. Only the first element(s) of 'NamesObsBV' argument used.")
    }
    NamesObsBV <- NamesObsBV[seq_along(ObsDF)]
    names(ObsDF) <- NamesObsBV
  } else if (!is.null(names(ObsDF)) & !is.null(ObsDF)) {
    NamesObsBV <- names(ObsDF)
  } else if (is.null(ObsDF)) {
    NamesObsBV <- ifelse(is.null(NamesObsBV), "Unnamed watershed", NamesObsBV[1L])
  }
  if (!is.null(NamesObsBV)) {
    if (any(nchar(NamesObsBV) == 0)) {
      stop("NamesObsBV must be a string vector of at least one character.")
    }
  }

  if (is.null(ObsDF)) {
    if (length(ZInputs) > 1) {
      warning("Too long 'ZInputs'. Only the first element(s) of 'ZInputs' argument used.")
      ZInputs <- list(ZInputs[[1L]])
    }
    if (length(HypsoData) > 1) {
      warning("Too long 'HypsoData'. Only the first element(s) of 'HypsoData' argument used.")
      HypsoData <- list(HypsoData[[1L]])
    }
    if (length(NLayers) > 1) {
      warning("Too long 'NLayers'. Only the first element(s) of 'NLayers' argument used.")
      NLayers <- list(NLayers[[1L]])
    }
    if (length(SimPer) > 1 & !is.list(SimPer)) {
      warning("Too long 'SimPer'. Only the first element(s) of 'SimPer' argument used.")
      SimPer <- SimPer[[1L]]
    }
  }

  if (is.null(ZInputs)) {
    ZInputs <- vector(mode = "list", length = lenObsDF)
  } else {
    ZInputs <- as.list(ZInputs)
    if (length(ZInputs) == lenObsDF) {
      ZInputs <- as.list(ZInputs)
    } else if (length(ZInputs) > lenObsDF) {
      ZInputs <- as.list(ZInputs)[seq_along(ObsDF)]
      warning("Too long 'ZInputs'. Only the first element(s) of 'ZInputs' argument used.")
    } else if (length(ZInputs) < lenObsDF) {
      ZInputs <- as.list(rep(ZInputs, lenObsDF))[seq_along(ObsDF)]
      if (lenObsDF > 1) {
        warning("Not enough 'ZInputs' elements. Elements of the list recycled.")
      }
    }
  }
  names(ZInputs) <- NamesObsBV

  if (is.null(HypsoData)) {
    HypsoData <- vector(mode = "list", length = lenObsDF)
  } else {
    if (!is.list(HypsoData)) {
      HypsoData <- list(HypsoData)
    }
    if (length(HypsoData) == lenObsDF) {
      HypsoData <- as.list(HypsoData)
    } else if (length(HypsoData) > lenObsDF) {
      HypsoData <- as.list(HypsoData)[seq_along(ObsDF)]
      warning("Too long 'HypsoData'. Only the first element(s) of 'HypsoData' argument used.")
    } else if (length(HypsoData) < lenObsDF) {
      HypsoData <- as.list(rep(HypsoData, lenObsDF))[seq_along(ObsDF)]
      if (lenObsDF > 1) {
        warning("Not enough 'HypsoData' elements. Elements of the list recycled.")
      }
    }
  }
  names(HypsoData) <- NamesObsBV

  if (is.null(NLayers)) {
    NLayers <- vector(mode = "list", length = lenObsDF)
  } else {
    if (length(NLayers) == lenObsDF) {
      NLayers <- as.list(NLayers)
    } else if (length(NLayers) > lenObsDF) {
      NLayers <- as.list(NLayers)[seq_along(ObsDF)]
      warning("Too long 'NLayers'. Only the first element(s) of 'NLayers' argument used.")
    } else if (length(NLayers) < lenObsDF) {
      NLayers <- as.list(rep(NLayers, lenObsDF))[seq_along(ObsDF)]
      if (lenObsDF > 1) {
        warning("Not enough 'NLayers' elements. Elements of the list recycled.")
      }
    }
  }
  names(NLayers) <- NamesObsBV

  if (length(SimPer) > lenObsDF) {
    SimPer <- as.list(SimPer)[seq_along(ObsDF)]
    warning("Too long 'SimPer'. Only the first element(s) of 'SimPer' argument used.")
  } else if (length(SimPer) < lenObsDF) {
    SimPer <- as.list(rep(SimPer, lenObsDF))[seq_along(ObsDF)]
    if (lenObsDF > 1) {
      warning("Not enough 'SimPer' elements. Elements of the list recycled.")
    }
  }
  names(SimPer) <- NamesObsBV


  .GlobalEnv$.ShinyGR.hist <- list(list())#list(Param = list(), TypeModel = lsit(), Crit = list(), Qsim = list())
  .GlobalEnv$.ShinyGR.args <- list(ObsDF = ObsDF, NamesObsBV = NamesObsBV,
                                   DatesR = DatesR, Precip = Precip, PotEvap = PotEvap, Qobs = Qobs, TempMean = TempMean,
                                   ZInputs = ZInputs, HypsoData = HypsoData, NLayers = NLayers, SimPer = SimPer,
                                   theme = theme)

  ## timezone used
  # oTZ <- Sys.timezone()
  Sys.setenv(TZ = "UTC")

  on.exit({rm(.ShinyGR.args, .ShinyGR.hist, envir = .GlobalEnv) ; Sys.unsetenv("TZ")})

  shiny::runApp(system.file("ShinyGR", package = "airGRteaching"), launch.browser = TRUE)
  return(NULL)

}
