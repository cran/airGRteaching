# server.R

shinyServer(function(input, output, session) {



  ## --------------- List of input names

  getInputs <- reactive({
    inputList <- sort(names(reactiveValuesToList(input)))
    inputList <- inputList[!grepl("^dy", inputList)]
    inputList <- inputList[!grepl("^CalButton", inputList)]
    inputList <- c(inputList, "DownloadTab", "DownloadPlot")
    return(inputList)
  })




  ## Models available considering the plot type
  observeEvent(input$Dataset, {
    if (is.null(.ShinyGR.args$ObsDF[[input$Dataset]])) {
      datesDataset <- .ShinyGR.args$DatesR[1:2]
    } else{
      datesDataset <- .ShinyGR.args$ObsDF[[input$Dataset]][[1]][1:2]
    }
    nbDaysDataset <- as.numeric(diff(datesDataset), units = "days")
    if (nbDaysDataset %in% 29:31) {
      updateSelectInput(session, inputId = "HydroModel", choice = c("GR2M"), selected = "GR2M")
      updateSelectInput(session, inputId = "SnowModel" , choice = c("None"))
    } else {
      if (input$HydroModel == "GR2M") {
      HydroModel <- "GR4J"
      } else {
        HydroModel <- input$HydroModel
      }
      updateSelectInput(session, inputId = "HydroModel", choice = c("GR4J", "GR5J", "GR6J"), selected = HydroModel)
      updateSelectInput(session, inputId = "SnowModel" , choice = c("None", "CemaNeige")   , selected = input$SnowModel)
    }
  }, priority = -100)




  ## --------------- Data preparation

  getPrep <- reactive({

    if (input$Dataset == "Unnamed watershed") {
      ObsDF <- NULL
    } else {
      # ObsDF <- get(input$Dataset)
      ObsDF <- .ShinyGR.args$ObsDF[[input$Dataset]]
    }
    if (!all(is.na(ObsDF[[4]])) | !all(is.na(.ShinyGR.args$Qobs))) {
      isUngauged <- FALSE
    } else {
      isUngauged <- TRUE
    }
    if (is.null(ObsDF)) {
      datesDataset <- .ShinyGR.args$DatesR[1:2]
    } else{
      datesDataset <- .ShinyGR.args$ObsDF[[input$Dataset]][[1]][1:2]
    }
    nbDaysDataset <- as.numeric(diff(datesDataset), units = "days")
    if (nbDaysDataset %in% 29:31) {
      HydroModel <- "GR2M"
    } else if (input$HydroModel == "GR2M") {
      updateSelectInput(session, inputId = "HydroModel", choice = c("GR4J", "GR5J", "GR6J"), selected = input$HydroModel)
      HydroModel <- "GR4J"
    } else {
      HydroModel <- input$HydroModel
    }
    TMGR  <- .TypeModelGR(HydroModel)
    X2 <- ifelse(input$HydroModel == "GR2M", input$X2GR2M, input$X2)
    PARAM <- c(input$X1, X2, input$X3, input$X4, input$X5, input$X6)[seq_len(TMGR$NbParam)]
    if (input$SnowModel == "CemaNeige") {
      PARAM <- c(PARAM, input$C1, input$C2)
    }
    PREP <- PrepGR(ObsDF = ObsDF,
                   DatesR = .ShinyGR.args$DatesR,
                   Precip = .ShinyGR.args$Precip, PotEvap = .ShinyGR.args$PotEvap,
                   Qobs = .ShinyGR.args$Qobs, TempMean = .ShinyGR.args$TempMean,
                   ZInputs = .ShinyGR.args$ZInputs[[input$Dataset]],
                   HypsoData = .ShinyGR.args$HypsoData[[input$Dataset]],
                   NLayers = .ShinyGR.args$NLayers[[input$Dataset]],
                   HydroModel = HydroModel,
                   CemaNeige = input$SnowModel == "CemaNeige")

    ## old value: bad time zone management
    #WUPPER <- c(PREP$InputsModel$DatesR[1L], input$Period[1L]-.TypeModelGR(PREP)$TimeLag)
    ## patch from Juan Camilo Peña <juancamilopec@gmail.com>
    #WUPPER <- c(format(PREP$InputsModel$DatesR[1L], format = "%Y-%m-%d", tz = "UTC"), format(input$Period[1L]-.TypeModelGR(PREP)$TimeLag, format = "%Y-%m-%d", tz = "UTC"))
    ## new value
    WUPPER <- as.POSIXlt(c(as.character(PREP$InputsModel$DatesR[1L]), as.character(input$Period[1L]-.TypeModelGR(PREP)$TimeLag)), tz = "UTC")
    if (HydroModel == "GR2M") {
      WUPPER <- trunc(WUPPER, units = "months")
    } else {
      WUPPER <- trunc(WUPPER, units = "days")
    }
    if (WUPPER[2L] < WUPPER[1L]) {
      WUPPER[2L] <- WUPPER[1L]
    }

    ## Enable or disable automatic calibration (if there is Qobs or not)
    isQobs <- !all(is.na(PREP$Qobs[PREP$InputsModel$Dates >= input$Period[1L] & PREP$InputsModel$Dates <= input$Period[2L]]))
    # if ( isQobs | input$Period[1L] != input$Period[2L]) {
    #   shinyjs::enable("CalButton")
    # }
    if (!isQobs | input$Period[1L] == input$Period[2L]) {
      shinyjs::disable("CalButton")
    }

    return(list(TMGR = TMGR, PREP = PREP, WUPPER = WUPPER, isUngauged = isUngauged))

  })



  ## --------------- Calibration

  ## If the user calibrate the model
  CAL_click <- reactiveValues(valueButton = 0)


  ## Automatic calibration
  observeEvent(input$CalButton, {

    ## Desable all inputs during automatic calibration
    lapply(getInputs(), shinyjs::disable)
    shinyjs::disable("CalButton")

    ## Model calibration
    CAL_opt <- list(Crit    = gsub(" .*", "", input$TypeCrit),
                    Transfo = gsub("1", "inv", gsub("(\\D{3} \\[)(\\w{0,4})(\\W*Q\\W*\\])", "\\2", input$TypeCrit)))
    CAL     <- CalGR(PrepGR = getPrep()$PREP, CalCrit = CAL_opt$Crit, transfo = CAL_opt$Transfo,
                     WupPer = substr(getPrep()$WUPPER, 1, 10),
                     CalPer = c(substr(input$Period[1L], 1, 10), substr(input$Period[2L], 1, 10)),
                     verbose = FALSE)
    PARAM   <- CAL$OutputsCalib$ParamFinalR

    updateSliderInput(session, inputId = "X1", value = PARAM[1L])
    if (input$HydroModel == "GR2M") {
      updateSliderInput(session, inputId = "X2GR2M", value = PARAM[2L])
    }
    if (input$HydroModel %in% c("GR4J", "GR5J", "GR6J")) {
      updateSliderInput(session, inputId = "X2", value = PARAM[2L])
      updateSliderInput(session, inputId = "X3", value = PARAM[3L])
      updateSliderInput(session, inputId = "X4", value = PARAM[4L])
    }
    if (input$HydroModel %in% c("GR5J", "GR6J")) {
      updateSliderInput(session, inputId = "X5", value = PARAM[5L])
    }
    if (input$HydroModel %in% "GR6J") {
      updateSliderInput(session, inputId = "X6", value = PARAM[6L])
    }
    if (input$SnowModel == "CemaNeige") {
      updateSliderInput(session, inputId = "C1", value = PARAM[length(PARAM)-1])
      updateSliderInput(session, inputId = "C2", value = PARAM[length(PARAM)])
    }
    updateActionButton(session, inputId = "CalButton", label = "Model calibrated", icon = icon("check"))
    CAL_click$valueButton <- 1
    shinyjs::disable("CalButton")
    ## Enable calibration
    # if (input$Period[1L] != input$Period[2L]) {
    #   lapply(getInputs(), shinyjs::enable)
    #   shinyjs::enable("CalButton")
    # }
  }, priority = +20)


  ## Manual calibration
  observeEvent({input$Dataset ; input$HydroModel ; input$SnowModel ;
    input$X1 ; input$X2 ; input$X2GR2M ; input$X3 ; input$X4 ; input$X5 ; input$X6 ; input$C1 ; input$C2 ;
    input$TypeCrit ; input$Period}, {
      CAL_click$valueButton <- CAL_click$valueButton - 1
      CAL_click$valueButton <- ifelse(CAL_click$valueButton < -1, -1, CAL_click$valueButton)
      if (CAL_click$valueButton < 0) {
        updateActionButton(session, inputId = "CalButton", label = "Run", icon = icon("arrows-rotate"))
        if (!getPrep()$isUngauged) {
          shinyjs::enable("CalButton")
        }
      }

      ## Enable all inputs except automatic calibration
      if (input$Period[1L] != input$Period[2L]) {
        lapply(getInputs(), shinyjs::enable)
        if (getPrep()$isUngauged) {
          shinyjs::disable("CalButton")
          shinyjs::disable("TypeCrit")
        }
      }

      ## Disable the use of CemaNeige is there is no temperature
      if (!is.null(.ShinyGR.args$ObsDF[[input$Dataset]])) {
        if (ncol(.ShinyGR.args$ObsDF[[input$Dataset]]) < 5) {
          shinyjs::disable("SnowModel")
        }
      } else {
        if (is.null(.ShinyGR.args$TempMean)) {
          shinyjs::disable("SnowModel")
        }
      }
    })



  ## --------------- Simulation

  getSim <- reactive({
    X2 <- ifelse(input$HydroModel == "GR2M", input$X2GR2M, input$X2)
    PARAM <- c(input$X1, X2, input$X3, input$X4, input$X5, input$X6)[seq_len(getPrep()$TMGR$NbParam)]
    if (input$SnowModel == "CemaNeige") {
      PARAM <- c(PARAM, input$C1, input$C2)
    }

    ## Simulated flows computation
    PREP <- getPrep()$PREP
    if (input$HydroModel == "GR2M") {
      SimPer <- trunc(input$Period, units = "months")
    } else {
      SimPer <- trunc(input$Period, units = "days")
    }
    SIM <- SimGR(PrepGR = PREP, Param = PARAM,
                 WupPer = substr(getPrep()$WUPPER, 1, 10),
                 SimPer = c(substr(SimPer[1L], 1, 10), substr(SimPer[2L], 1, 10)),
                 verbose = FALSE)

    ## Criteria computation
    CRIT_opt <- list(Crit    = c(rep("ErrorCrit_NSE", 3), rep("ErrorCrit_KGE", 3)),
                     Transfo = rep(c("", "sqrt", "inv"), times = 2))
    nCRIT_opt <- length(CRIT_opt$Crit)
    if (!getPrep()$isUngauged) {
      InputsCritMulti <- CreateInputsCrit(FUN_CRIT = CRIT_opt$Crit,
                                          InputsModel = getPrep()$PREP$InputsModel,
                                          RunOptions = SIM$OptionsSimul,
                                          Obs = replicate(n = nCRIT_opt, expr = SIM$Qobs, simplify = FALSE),
                                          VarObs = rep("Q", times = nCRIT_opt),
                                          transfo = CRIT_opt$Transfo,
                                          Weights = NULL)
      iCRIT <- ErrorCrit(InputsCrit = InputsCritMulti, OutputsModel = SIM$OutputsModel, verbose = FALSE)
      CRIT <- do.call("rbind", lapply(iCRIT, function(i) data.frame(CritName = i$CritName, CritValue = i$CritValue)))
      CRIT <- rbind(CRIT, data.frame(CritName = "BIAS[Qsim/Qobs]",
                                     CritValue = ifelse(is.na(iCRIT[[which(CRIT$CritName == "KGE[Q]")]]$CritValue),
                                                        NA,
                                                        iCRIT[[which(CRIT$CritName == "KGE[Q]")]]$SubCritValues[3])))
      colnames(CRIT) <- c("Criterion", "Value")
    } else {
      CRIT <- data.frame(Criterion = NA, Value = NA)
    }

    ## Recording past simulations
    .GlobalEnv$.ShinyGR.hist[[length(.GlobalEnv$.ShinyGR.hist)+1]] <- list(Qsim      = SIM$OutputsModel$Qsim,
                                                                           Param     = PARAM,
                                                                           TypeModel = SIM$TypeModel,
                                                                           Crit      = CRIT,
                                                                           Dataset   = input$Dataset,
                                                                           Period    = SIM$PeriodModel$Run)

    .GlobalEnv$.ShinyGR.hist <- .GlobalEnv$.ShinyGR.hist[!(duplicated(sapply(.GlobalEnv$.ShinyGR.hist, function(x) sum(x$Param)), fromLast = TRUE) &
                                                           duplicated(sapply(.GlobalEnv$.ShinyGR.hist, function(x) x$TypeModel), fromLast = TRUE))]
    .GlobalEnv$.ShinyGR.hist <- tail(.GlobalEnv$.ShinyGR.hist, n = 2)

    if (length(.GlobalEnv$.ShinyGR.hist) == 2 &  is.null(names(.GlobalEnv$.ShinyGR.hist[[1L]]))) {
      .GlobalEnv$.ShinyGR.hist[[1L]] <- NULL
    }
    if (length(.GlobalEnv$.ShinyGR.hist) == 2) {
      if (.GlobalEnv$.ShinyGR.hist[[1L]]$Dataset != .GlobalEnv$.ShinyGR.hist[[2L]]$Dataset) { # reset Qold when new dataset
        .GlobalEnv$.ShinyGR.hist[[1L]] <- NULL
      }
    }
    if (length(.GlobalEnv$.ShinyGR.hist) == 2 & !is.null(names(.GlobalEnv$.ShinyGR.hist[[1L]]))) {
      isEqualSumQsim   <- !identical(sum(.GlobalEnv$.ShinyGR.hist[[1L]]$Crit$Value), sum(.GlobalEnv$.ShinyGR.hist[[2L]]$Crit$Value))
      isEqualTypeModel <- .GlobalEnv$.ShinyGR.hist[[1L]]$TypeModel == .GlobalEnv$.ShinyGR.hist[[2L]]$TypeModel
      isEqualPeriod    <- !identical(.GlobalEnv$.ShinyGR.hist[[1L]]$Period, .GlobalEnv$.ShinyGR.hist[[2L]]$Period)
      if (length(.GlobalEnv$.ShinyGR.hist[[1L]]$Qsim) != length(.GlobalEnv$.ShinyGR.hist[[2L]]$Qsim) |
          (isEqualSumQsim & isEqualTypeModel) | isEqualPeriod) {
        OBSold <- getPrep()$PREP
        OBSold$TypeModel <- .GlobalEnv$.ShinyGR.hist[[1L]]$TypeModel
        if (.TypeModelGR(OBSold)$CemaNeige & !.TypeModelGR(getPrep()$PREP)$CemaNeige | # present: No CemaNeige ; old: CemaNeige
            isEqualSumQsim & isEqualTypeModel) {
          if (input$Dataset == "Unnamed watershed") {
            ObsDF <- NULL
          } else {
            # ObsDF <- get(input$Dataset)
            ObsDF <- .ShinyGR.args$ObsDF[[input$Dataset]]
          }
          OBSold <- PrepGR(ObsDF = ObsDF,
                           DatesR = .ShinyGR.args$DatesR,
                           Precip = .ShinyGR.args$Precip, PotEvap = .ShinyGR.args$PotEvap,
                           Qobs = .ShinyGR.args$Qobs, TempMean = .ShinyGR.args$TempMean,
                           ZInputs = .ShinyGR.args$ZInputs[[input$Dataset]],
                           HypsoData = .ShinyGR.args$HypsoData[[input$Dataset]],
                           NLayers = .ShinyGR.args$NLayers[[input$Dataset]],
                           HydroModel = input$HydroModel,
                           CemaNeige = input$SnowModel == "CemaNeige")
        }
        if (input$HydroModel == "GR2M") {
          SimPer <- trunc(input$Period, units = "months")
        } else {
          SimPer <- trunc(input$Period, units = "days")
        }
        SIMold <- SimGR(PrepGR = OBSold,
                        Param = .GlobalEnv$.ShinyGR.hist[[1L]]$Param,
                        WupPer = substr(getPrep()$WUPPER, 1, 10),
                        SimPer = substr(c(SimPer[1L], SimPer[2L]), 1, 10),
                        verbose = FALSE)
        if (!getPrep()$isUngauged) {
          InputsCritMultiold <- CreateInputsCrit(FUN_CRIT = CRIT_opt$Crit,
                                                 InputsModel = OBSold$InputsModel,
                                                 RunOptions = SIMold$OptionsSimul,
                                                 Obs = replicate(n = nCRIT_opt, expr = SIMold$Qobs, simplify = FALSE),
                                                 VarObs = rep("Q", times = nCRIT_opt),
                                                 transfo = CRIT_opt$Transfo,
                                                 Weights = NULL)
          iCRITold <- ErrorCrit(InputsCrit = InputsCritMultiold, OutputsModel = SIMold$OutputsModel, verbose = FALSE)
          CRITold <- do.call("rbind", lapply(iCRITold, function(i) data.frame(CritName = i$CritName, CritValue = i$CritValue)))
          CRITold <- rbind(CRITold, data.frame(CritName = "BIAS[Qsim/Qobs]",
                                               CritValue = ifelse(is.na(iCRITold[[which(CRITold$CritName == "KGE[Q]")]]$CritValue),
                                                                  NA,
                                                                  iCRITold[[which(CRITold$CritName == "KGE[Q]")]]$SubCritValues[3])))
          colnames(CRITold) <- c("Criterion", "Value")
        } else {
          CRITold <- data.frame(Criterion = NA, Value = NA)
        }
        .GlobalEnv$.ShinyGR.hist[[1L]]$Crit <- CRITold
        .GlobalEnv$.ShinyGR.hist[[1L]]$Qsim <- SIMold$OutputsModel$Qsim
      }
    }

    return(list(PARAM = PARAM, SIM = SIM, SIMold = .GlobalEnv$.ShinyGR.hist, Crit = CRIT))

  })



  ## --------------- Plot

  ## Choice
  getPlotType <- reactive({
    switch(input$PlotType,
           "Model performance" = 1,
           "Flow time series"  = 2,
           "State variables"   = 3,
           "Model diagram"     = 4)
  })


  ## Models available considering the plot type
  # observe({
  #   if (getPlotType() == 4) {
  #     updateSelectInput(session, inputId = "HydroModel", choice = c("GR4J", "GR5J", "GR6J"), selected = input$HydroModel)
  #     updateSelectInput(session, inputId = "SnowModel" , choice = c("None"))
  #   } else {
  #     updateSelectInput(session, inputId = "HydroModel", choice = c("GR4J", "GR5J", "GR6J"), selected = input$HydroModel)
  #     updateSelectInput(session, inputId = "SnowModel" , choice = c("None", "CemaNeige")   , selected = input$SnowModel)
  #   }
  # })


  ## Plots available considering the model type
  # observe({
  #   if (input$HydroModel == "GR6J") {
  #     updateSelectInput(session, inputId = "PlotType",
  #                       choice = c("Flow time series", "Model performance", "State variables"),
  #                       selected = input$PlotType)
  #   } else {
  #     updateSelectInput(session, inputId = "PlotType",
  #                       choice = c("Flow time series", "Model performance", "State variables", "Model diagram"),
  #                       selected = input$PlotType)
  #   }
  # })


  # Formated simulation results
  getData <- reactive({
    OutputsModel <- getSim()$SIM$OutputsModel
    IndPlot <- which(OutputsModel$DatesR >= input$Period[1L] & OutputsModel$DatesR <= input$Period[2L])
    namesOutputsModel2Selec <- names(OutputsModel)[seq_len(which(names(OutputsModel) == "Qsim"))]
    OutputsModel2 <- sapply(OutputsModel[namesOutputsModel2Selec], function(x) x[IndPlot])
    # OutputsModel2 <- OutputsModel[IndPlot] ### using S3method('[', OutputsModel)
    OutputsModel2 <- c(OutputsModel2, Qobs = list(getSim()$SIM$Qobs[IndPlot]))

    if (length(OutputsModel2$DatesR) != 0) {
      data <- data.frame(DatesR  = OutputsModel2$DatesR,
                         precip. = OutputsModel2$Precip,
                         PET     = OutputsModel2$PotEvap,
                         prod.   = OutputsModel2$Prod,
                         rout.   = OutputsModel2$Rout,
                         # exp.    = rep(NA, length(OutputsModel2$DatesR)),
                         # 'exp. (+)'= rep(NA, length(OutputsModel2$DatesR)),
                         # 'exp. (-)'= rep(NA, length(OutputsModel2$DatesR)),
                         # Qr      = OutputsModel2$QR,
                         # Qd      = OutputsModel2$QD,
                         Qsim    = OutputsModel2$Qsim,
                         Qobs    = OutputsModel2$Qobs,
                         QsimOld = rep(NA, length(OutputsModel2$DatesR)))
                         # QrExp   = rep(NA, length(OutputsModel2$DatesR)))

      if (length(.GlobalEnv$.ShinyGR.hist) == 2 & input$ShowOldQsim == "Yes") {
        data$QsimOld <- .GlobalEnv$.ShinyGR.hist[[1L]]$Qsim[seq_len(nrow(data))]
      }
      if (input$HydroModel == "GR6J") {
        data$'exp.'    <- NULL
        data$'exp. (+)'<- ifelse(OutputsModel2$Exp >= 0, OutputsModel2$Exp, NA)
        data$'exp. (-)'<- ifelse(OutputsModel2$Exp <  0, OutputsModel2$Exp, NA)
        data$'QrExp'   <- OutputsModel2$QRExp
      }
      if (input$HydroModel != "GR2M") {
        data$'Qr'    <- OutputsModel2$QR
        data$'Qd'    <- OutputsModel2$QD
      }

      return(list(OutputsModel = OutputsModel2, Tab = data))
    }
  })


  ## Period slider responds to changes in the selected/zoomed dateWindow
  observeEvent({input$dyPlotTSq_date_window ; input$dyPlotSVq_date_window ; input$dyPlotMDp_date_window}, {
    if (!is.null(input$dyPlotTSq_date_window)  && getPlotType() == 2) {
      dateWindow <- as.POSIXct(strftime(input$dyPlotTSq_date_window, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
    }
    if (!is.null(input$dyPlotSVq_date_window) && getPlotType() == 3) {
      dateWindow <- as.POSIXct(strftime(input$dyPlotSVq_date_window, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
    }
    if (!is.null(input$dyPlotMDp_date_window) && getPlotType() == 4) {
      dateWindow <- as.POSIXct(strftime(input$dyPlotMDp_date_window, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
    }
    if (exists("dateWindow")) {
      # if (dateWindow[1L] == dateWindow[2L]) {
      #   if (dateWindow[1L] == as.POSIXct(.ShinyGR.args$SimPer[2L], tz = "UTC")) {
      #     updateSliderInput(session, inputId = "Period",
      #                       value = dateWindow - c(1, 0) * .TypeModelGR(input$HydroModel)$TimeLag)
      #   } else {
      #     updateSliderInput(session, inputId = "Period",
      #                       value = dateWindow + c(0, 1) * .TypeModelGR(input$HydroModel)$TimeLag)
      #   }
      # } else {
      if (dateWindow[1L] != dateWindow[2L]) {
        timeFormat <- ifelse(input$HydroModel == "GR2M", "%Y-%m", "%F")
        updateSliderInput(session, inputId = "Period",
                          value = dateWindow, ### + .TypeModelGR(input$HydroModel)$TimeLag,
                          timeFormat = timeFormat, timezone = "+0000")
      }
      # }
    }
  }, priority = +100)


  # observe({
  #   if (getPlotType() == 1) {
  #     if (input$Period[1L] == input$Period[2L]) {
  #       if (input$Period[1L] == as.POSIXct(.ShinyGR.args$SimPer[2L], tz = "UTC")) {
  #         updateSliderInput(session, inputId = "Period",
  #                           value = input$Period - c(1, 0) * .TypeModelGR(input$HydroModel)$TimeLag)
  #       } else {
  #         updateSliderInput(session, inputId = "Period",
  #                           value = input$Period + c(0, 1) * .TypeModelGR(input$HydroModel)$TimeLag)
  #       }
  #     }
  #   }
  # }, priority = +100)

  ## Disable all inputs if there is no data
  observe({
    if (input$Period[1L] == input$Period[2L]) {
      inputs <- gsub("Period", "CalButton", getInputs())
      lapply(inputs, shinyjs::disable)
    }
  }, priority = -100)


  ## Reset period slider responds to dygraphs to mouse clicks
  observeEvent({input$dyPlotTSq_click}, {
    timeFormat <- ifelse(input$HydroModel == "GR2M", "%Y-%m", "%F")
    updateSliderInput(session, inputId = "Period",
                      value = as.POSIXct(.ShinyGR.args$SimPer[[input$Dataset]], tz = "UTC"),
                      timeFormat = timeFormat, timezone = "+0000")
  }, priority = +10)
  observeEvent({input$dyPlotTSe_click}, {
    timeFormat <- ifelse(input$HydroModel == "GR2M", "%Y-%m", "%F")
    updateSliderInput(session, inputId = "Period",
                      value = as.POSIXct(.ShinyGR.args$SimPer[[input$Dataset]], tz = "UTC"),
                      timeFormat = timeFormat, timezone = "+0000")
  }, priority = +10)
  observeEvent({input$dyPlotSVs_click}, {
    timeFormat <- ifelse(input$HydroModel == "GR2M", "%Y-%m", "%F")
    updateSliderInput(session, inputId = "Period",
                      value = as.POSIXct(.ShinyGR.args$SimPer[[input$Dataset]], tz = "UTC"),
                      timeFormat = timeFormat, timezone = "+0000")
  }, priority = +10)
  observeEvent({input$dyPlotSVq_click}, {
    timeFormat <- ifelse(input$HydroModel == "GR2M", "%Y-%m", "%F")
    updateSliderInput(session, inputId = "Period",
                      value = as.POSIXct(.ShinyGR.args$SimPer[[input$Dataset]], tz = "UTC"),
                      timeFormat = timeFormat, timezone = "+0000")
  }, priority = +10)
  observeEvent({input$dyPlotMDp_click}, {
    timeFormat <- ifelse(input$HydroModel == "GR2M", "%Y-%m", "%F")
    updateSliderInput(session, inputId = "Period",
                      value = as.POSIXct(.ShinyGR.args$SimPer[[input$Dataset]], tz = "UTC"),
                      timeFormat = timeFormat, timezone = "+0000")
  }, priority = +10)
  observeEvent({input$dyPlotMDe_click}, {
    timeFormat <- ifelse(input$HydroModel == "GR2M", "%Y-%m", "%F")
    updateSliderInput(session, inputId = "Period",
                      value = as.POSIXct(.ShinyGR.args$SimPer[[input$Dataset]], tz = "UTC"),
                      timeFormat = timeFormat, timezone = "+0000")
  }, priority = +10)
  observeEvent({input$dyPlotMDq_click}, {
    timeFormat <- ifelse(input$HydroModel == "GR2M", "%Y-%m", "%F")
    updateSliderInput(session, inputId = "Period",
                      value = as.POSIXct(.ShinyGR.args$SimPer[[input$Dataset]], tz = "UTC"),
                      timeFormat = timeFormat, timezone = "+0000")
  }, priority = +10)


  ## Time window slider and dataset choosen on the Summary sheet panel
  observeEvent({input$Dataset}, {
    timeFormat <- ifelse(input$HydroModel == "GR2M", "%Y-%m", "%F")
    updateSliderInput(session, inputId = "Period",
                      min = as.POSIXct(.ShinyGR.args$SimPer[[input$Dataset]][1L], tz = "UTC"),
                      max = as.POSIXct(.ShinyGR.args$SimPer[[input$Dataset]][2L], tz = "UTC"),
                      value = as.POSIXct(.ShinyGR.args$SimPer[[input$Dataset]], tz = "UTC"),
                      timeFormat = timeFormat, timezone = "+0000")
    updateSelectInput(session, inputId = "DatasetSheet",
                      choices = .ShinyGR.args$NamesObsBV,
                      selected = input$Dataset)
  })


  ## Dataset choosen on the SInterface panel
  observeEvent({input$DatasetSheet}, {
    updateSelectInput(session, inputId = "Dataset",
                      choices = .ShinyGR.args$NamesObsBV,
                      selected = input$DatasetSheet)
  })


  ## Target date slider
  eventReactive({input$Dataset}, {
    EventId <- ifelse(input$HydroModel == "GR2M", "EventGR2M", "Event")
    timeFormat <- ifelse(input$HydroModel == "GR2M", "%Y-%m", "%F")
    updateSliderInput(session, inputId = EventId, label = "Select the target date:",
                      min = as.POSIXct(.ShinyGR.args$SimPer[[input$Dataset]][1L], tz = "UTC"),## + .TypeModelGR(input$HydroModel)$TimeLag,
                      max = as.POSIXct(.ShinyGR.args$SimPer[[input$Dataset]][2L], tz = "UTC"),
                      value = as.POSIXct(.ShinyGR.args$SimPer[[input$Dataset]][1L], tz = "UTC"), + .TypeModelGR(input$HydroModel)$TimeLag,
                      timeFormat = timeFormat, timezone = "+0000")
  })
  observe({
    EventId <- ifelse(input$HydroModel == "GR2M", "EventGR2M", "Event")
    timeFormat <- ifelse(input$HydroModel == "GR2M", "%Y-%m", "%F")
    updateSliderInput(session, inputId = EventId, label = "Select the target date:",
                      min = input$Period[1L],## + .TypeModelGR(input$HydroModel)$TimeLag,
                      max = input$Period[2L],
                      timeFormat = timeFormat, timezone = "+0000")
  }, priority = -100)


  ## Graphical parameters
  getPlotPar <- reactive({
    if (.GlobalEnv$.ShinyGR.args$theme == "Cyborg") {
      col_bg <- "black"
      col_fg <- "white"
      par(bg = col_bg, fg = col_fg, col.axis = col_fg, col.lab = col_fg)
    # } else if (.GlobalEnv$.ShinyGR.args$theme == "Flatly") {
    #   col_bg <- "#2C3E50"
    #   col_fg <- "black"
    #   par(bg = col_bg, fg = col_fg, col.axis = col_bg, col.lab = col_bg)
    } else {
      col_bg <- "white"
      col_fg <- "black"
      par(bg = col_bg , fg = col_fg)
    }
    return(list(col_bg = col_bg, col_fg = col_fg, par = par(no.readonly = TRUE)))
  })


  ## Plot model performance
  output$stPlotMP <- renderPlot({
    if (length(getSim()$SIM$OutputsModel$DatesR) < 2) {
      return(NULL)
    }
    OutputsModel <- getSim()$SIM$OutputsModel
    IndPlot <- which(OutputsModel$DatesR >= input$Period[1L] & OutputsModel$DatesR <= input$Period[2L])
    par(getPlotPar()$par)
    par(cex.axis = 1.2)
    if (input$SnowModel != "CemaNeige") {
      par(oma = c(20, 0, 0, 0))
    }
    plot(OutputsModel, Qobs = getSim()$SIM$Qobs, IndPeriod_Plot = IndPlot, cex.lab = 1.2, cex.axis = 1.4, cex.leg = 1.4)
  }, bg = "transparent")


  ## Plot flow time series
  output$dyPlotTSq <- dygraphs::renderDygraph({
    if (length(getSim()$SIM$OutputsModel$DatesR) < 2) {
      return(NULL)
    }
    if (length(getSim()$SIMold) == 2 & input$ShowOldQsim == "Yes") {
      QsimOld <- getSim()$SIMold[[1L]]$Qsim
    } else {
      QsimOld <- NULL
    }
    op <- getPlotPar()$par
    dgTSq <- dyplot(getSim()$SIM, Qsup = QsimOld, Qsup.name = "Qold",
                    RangeSelector = FALSE, LegendShow = "auto",
                    col.Q = c(op$fg, "orangered", "grey"),
                    col.Precip = c("#428BCA", "lightblue"),
                    col.na = rgb(0.5, 0.5, 0.5, alpha = 0.4),
                    group = "ts")
    dgTSq <- dygraphs::dyOptions(dgTSq, axisLineColor = op$fg, axisLabelColor = op$fg,
                                 retainDateWindow = FALSE, useDataTimezone = TRUE)
    dgTSq <- dygraphs::dyLegend(dgTSq, show = "follow", width = 325)
    dgTSq <- dygraphs::dyCrosshair(dgTSq, direction = "vertical")
  })
  output$dyPlotTSe <- dygraphs::renderDygraph({
    if (length(getSim()$SIM$OutputsModel$DatesR) < 2) {
      return(NULL)
    }
    if (getPrep()$isUngauged) {
      return(NULL)
    }
    if (length(getSim()$SIMold) == 2 & input$ShowOldQsim == "Yes") {
      ErrorOld <- getSim()$SIMold[[1L]]$Qsim - getSim()$SIM$Qobs
    } else {
      ErrorOld <- NA
    }
    data <- data.frame(DatesR = getSim()$SIM$OutputsModel$DatesR,
                       Error = getSim()$SIM$OutputsModel$Qsim - getSim()$SIM$Qobs,
                       ErrorOld = ErrorOld,
                       naCol = NA)
    data.xts <- xts::xts(data[, -1L, drop = FALSE], order.by = data$DatesR, tz = "UTC")
    op <- getPlotPar()$par
    ylabDgTSe <- sprintf("flow error [mm/%s]", .TypeModelGR(input$HydroModel)$TimeUnit)
    dgTSe <- dygraphs::dygraph(data.xts, group = "ts", ylab = ylabDgTSe, main = "   ")
    dgTSe <- dygraphs::dySeries(dgTSe, "Error"   , axis = "y" , color = "orangered")
    dgTSe <- dygraphs::dySeries(dgTSe, "ErrorOld", axis = "y" , color = "grey", strokePattern = "dashed")
    dgTSe <- dygraphs::dySeries(dgTSe, "naCol", axis = "y2", color = NA)
    dgTSe <- dygraphs::dyAxis(dgTSe, name = "y", axisLabelWidth = 60)
    dgTSe <- dygraphs::dyAxis(dgTSe, name = "y2", drawGrid = FALSE,
                              axisLabelFormatter = "function(d) {return d.toString().replace(/./g,'');}",
                              axisLabelWidth = 60)
    dgTSe <- dygraphs::dyOptions(dgTSe, titleHeight = 50,
                                 axisLineColor = op$fg, axisLabelColor = op$fg,
                                 retainDateWindow = FALSE, useDataTimezone = TRUE)
    dgTSe <- dygraphs::dyLegend(dgTSe, show = "onmouseover", width = 225)
    dgTSe <- dygraphs::dyCrosshair(dgTSe, direction = "vertical")
    dgTSe <- dygraphs::dyLimit(dgTSe, limit = 0, color = "blue")
    idNA <- .StartStop(data$Error, FUN = is.na)
    dgTSe <- .DyShadingMulti(dygraph = dgTSe, color = rgb(0.5, 0.5, 0.5, alpha = 0.4),
                             ts = data$DatesR, idStart = idNA$start, IdStop = idNA$stop)
  })


  ## Plot state variables stores
  output$dyPlotSVs <- dygraphs::renderDygraph({
    if (length(getSim()$SIM$OutputsModel$DatesR) < 2) {
      return(NULL)
    }
    # OutputsModel <- getSim()$SIM$OutputsModel
    # data <- data.frame(DatesR = OutputsModel$DatesR,
    #                    prod.  = OutputsModel$Prod,
    #                    rout.  = OutputsModel$Rout)
    data <- getData()$Tab[, c("DatesR", "prod.", "rout.", grep("^exp", colnames(getData()$Tab), value = TRUE))]
    data.xts <- xts::xts(data[, -1L, drop = FALSE], order.by = data$DatesR, tzone = "UTC")

    colors <- c("#00008B", "#008B8B", "#10B510", "#FF0303")[seq_len(ncol(data.xts))]

    op <- getPlotPar()$par
    dgSVs <- dygraphs::dygraph(data.xts, group = "state_var", ylab = "store level [mm]")
    dgSVs <- dygraphs::dyOptions(dgSVs, colors = colors,
                                 fillGraph = TRUE, fillAlpha = 0.3,
                                 drawXAxis = FALSE, axisLineColor = op$fg, axisLabelColor = op$fg,
                                 retainDateWindow = FALSE, useDataTimezone = TRUE)
    dgSVs <- dygraphs::dyLegend(dgSVs, show = "always", width = 325)
    dgSVs <- dygraphs::dyCrosshair(dgSVs, direction = "vertical")
  })


  ## Plot state variables Q
  output$dyPlotSVq <- dygraphs::renderDygraph({
    if (length(getSim()$SIM$OutputsModel$DatesR) < 2) {
      return(NULL)
    }
    # OutputsModel <- getSim()$SIM$OutputsModel
    # IndPlot <- which(OutputsModel$DatesR >= input$Period[1L] & OutputsModel$DatesR <= input$Period[2L])
    # OutputsModel2 <- sapply(OutputsModel[seq_len(which(names(OutputsModel) == "Qsim"))], function(x) x[IndPlot])
    # OutputsModel2 <- c(OutputsModel2, Qobs = list(getSim()$SIM$Qobs[IndPlot]))
    #
    # data <- data.frame(DatesR = OutputsModel2$DatesR,
    #                    Qr     = OutputsModel2$QR,
    #                    Qd     = OutputsModel2$QD,
    #                    Qsim   = OutputsModel2$Qsim,
    #                    Qobs   = OutputsModel2$Qobs)
    # if (input$HydroModel == "GR6J") {
    #   data$QrExp <- OutputsModel2$QRExp
    # } else {
    #   data$QrExp <- NA
    # }

    colSelec <- c("DatesR",
                  grep("^Qr$"  , colnames(getData()$Tab), value = TRUE),
                  grep("^Qd$"  , colnames(getData()$Tab), value = TRUE),
                  grep("^QrExp", colnames(getData()$Tab), value = TRUE),
                  "Qsim",
                  "Qobs")
    if (length(getSim()$SIMold) == 2 & input$ShowOldQsim == "Yes") {
      colSelec <- c(colSelec, "QsimOld")
    }

    data <- getData()$Tab[, colSelec]
    data.xts <- xts::xts(data[, -1L], order.by = data$DatesR, tzone = "UTC")

    if (input$HydroModel == "GR6J") {
      names  <- c("Qd", "Qr", "QrExp")
      colors <- c("#FFD700", "#EE6300", "brown")
    } else {
      names  <- c("Qd", "Qr")
      colors <- c("#FFD700", "#EE6300")
    }

    op <- getPlotPar()$par
    dgSVq <- dygraphs::dygraph(data.xts, group = "state_var", ylab = paste0("flow [mm/", getPrep()$TMGR$TimeUnit, "]"), main = " ")
    dgSVq <- dygraphs::dyOptions(dgSVq, fillAlpha = 1.0,
                                 axisLineColor = op$fg, axisLabelColor = op$fg, titleHeight = 10,
                                 retainDateWindow = FALSE, useDataTimezone = TRUE)
    if (input$HydroModel != "GR2M") {
      dgSVq <- dygraphs::dyStackedRibbonGroup(dgSVq, name = names,
                                              color = colors, strokeBorderColor = "black")
    }
    dgSVq <- dygraphs::dySeries(dgSVq, name = "Qobs", fillGraph = FALSE, drawPoints = TRUE, color = op$fg)
    dgSVq <- dygraphs::dySeries(dgSVq, name = "Qsim", fillGraph = FALSE, color = "orangered")
    if (length(getSim()$SIMold) == 2 & input$ShowOldQsim == "Yes") {
      dgSVq <- dygraphs::dySeries(dgSVq, name = "QsimOld", label = "Qold", fillGraph = FALSE, color = "grey", strokePattern = "dashed")
    }
    dgSVq <- dygraphs::dyCrosshair(dgSVq, direction = "vertical")
    dgSVq <- dygraphs::dyLegend(dgSVq, show = "always", width = 325)
    idNA <- .StartStop(getData()$Tab$Qobs, FUN = is.na)
    dgSVq <- .DyShadingMulti(dygraph = dgSVq, color = rgb(0.5, 0.5, 0.5, alpha = 0.4),
                             ts = data$DatesR, idStart = idNA$start, IdStop = idNA$stop)
  })


  ## Plot model diagram precipitation
  output$dyPlotMDp <- dygraphs::renderDygraph({
    if (length(getSim()$SIM$OutputsModel$DatesR) < 2) {
      return(NULL)
    }

    data <- data.frame(DatesR  = getSim()$SIM$OutputsModel$DatesR)
    if (grepl("CemaNeige", getSim()$SIM$TypeModel)) {
      data$Psol <- rowMeans(sapply(getSim()$SIM$OutputsModel$CemaNeigeLayers, function(x) x$Psol))
      data$Pliq <- rowMeans(sapply(getSim()$SIM$OutputsModel$CemaNeigeLayers, function(x) x$Pliq))
      Plim <- c(-1e-3, max(data$Psol+data$Pliq, na.rm = TRUE))
      col.Precip = c("#428BCA", "lightblue")
    } else {
      data$Precip <- getSim()$SIM$OutputsModel$Precip
      Plim <- c(-1e-3, max(data$Precip, na.rm = TRUE))
      col.Precip <- c("#428BCA")
    }
    data.xts <- xts::xts(data[, -1L, drop = FALSE], order.by = data$DatesR, tzone = "UTC")
    # dateEvent <- trunc(input$Event, units = ifelse(input$HydroModel == "GR2M", "months", "days"))
    if (input$HydroModel == "GR2M") {
      dateEvent <- trunc(input$EventGR2M, units = "months")
    } else {
      dateEvent <- trunc(input$Event, units = "days")
    }

    op <- getPlotPar()$par
    dgMDp <- dygraphs::dygraph(data.xts, group = "mod_diag", ylab = paste0("precip. [mm/", getPrep()$TMGR$TimeUnit, "]"))
    dgMDp <- dygraphs::dyOptions(dgMDp, colors = col.Precip, drawXAxis = FALSE,
                                 axisLabelColor = op$fg,
                                 retainDateWindow = FALSE, useDataTimezone = TRUE)
    dgMDp <- dygraphs::dyStackedBarGroup(dgMDp, name = rev(grep("^P", colnames(data.xts), value = TRUE)),
                                         axis = "y", color = (col.Precip))
    dgMDp <- dygraphs::dyAxis(dgMDp, name = "y", valueRange = rev(Plim))
    dgMDp <- dygraphs::dyEvent(dgMDp, dateEvent, color = "orangered")
    dgMDp <- dygraphs::dyLegend(dgMDp, show = "onmouseover", width = 225)
    dgMDp <- dygraphs::dyCrosshair(dgMDp, direction = "vertical")
  })


  ## Plot model diagram ETP
  output$dyPlotMDe <- dygraphs::renderDygraph({
    if (length(getSim()$SIM$OutputsModel$DatesR) < 2) {
      return(NULL)
    }
    # data <- data.frame(DatesR = getSim()$SIM$OutputsModel$DatesR,
    #                    PET    = getSim()$SIM$OutputsModel$PotEvap)
    data <- getData()$Tab[, c("DatesR", "PET")]
    data.xts <- xts::xts(data[, -1L, drop = FALSE], order.by = data$DatesR, tzone = "UTC")
    # dateEvent <- trunc(input$Event, units = ifelse(input$HydroModel == "GR2M", "months", "days"))
    if (input$HydroModel == "GR2M") {
      dateEvent <- trunc(input$EventGR2M, units = "months")
    } else {
      dateEvent <- trunc(input$Event, units = "days")
    }

    op <- getPlotPar()$par
    dgMDe <- dygraphs::dygraph(data.xts, group = "mod_diag", ylab = paste0("PET [mm/", getPrep()$TMGR$TimeUnit, "]"), main = " ")
    dgMDe <- dygraphs::dyOptions(dgMDe, colors = "#A4C400", drawPoints = TRUE,
                                 strokeWidth = 0, pointSize = 2, drawXAxis = FALSE,
                                 axisLineColor = op$fg, axisLabelColor = op$fg, titleHeight = 10,
                                 retainDateWindow = FALSE, useDataTimezone = TRUE)
    dgMDe <- dygraphs::dyEvent(dgMDe, dateEvent, color = "orangered")
    dgMDe <- dygraphs::dyLegend(dgMDe, show = "onmouseover", width = 225)
    dgMDe <- dygraphs::dyCrosshair(dgMDe, direction = "vertical")
  })


  ## Plot model diagram flow
  output$dyPlotMDq <- dygraphs::renderDygraph({
    if (length(getSim()$SIM$OutputsModel$DatesR) < 2) {
      return(NULL)
    }
    # if (length(getSim()$SIMold) == 2 & input$ShowOldQsim == "Yes") {
    #   QsimOld <- getSim()$SIMold[[1L]]$Qsim
    # } else {
    #   QsimOld <- NA
    # }
    # OutputsModel <- getSim()$SIM$OutputsModel
    # IndPlot <- which(OutputsModel$DatesR >= input$Period[1L] & OutputsModel$DatesR <= input$Period[2L])
    # OutputsModel2 <- sapply(OutputsModel[seq_len(which(names(OutputsModel) == "Qsim"))], function(x) x[IndPlot])
    # OutputsModel2 <- c(OutputsModel2, Qobs = list(getSim()$SIM$Qobs[IndPlot]))
    # OutputsModel2$Qsim <- ifelse(format(OutputsModel2$DatesR, "%Y%m%d") > format(input$Event, "%Y%m%d"), NA, OutputsModel2$Qsim)
    # OutputsModel2$Qold <- ifelse(format(OutputsModel2$DatesR, "%Y%m%d") > format(input$Event, "%Y%m%d"), NA, QsimOld[IndPlot])
    #
    # data <- data.frame(DatesR  = OutputsModel2$DatesR,
    #                    Qobs    = OutputsModel2$Qobs,
    #                    Qsim    = OutputsModel2$Qsim,
    #                    QsimOld = OutputsModel2$Qold)

    # dateEvent <- trunc(input$Event, units = ifelse(input$HydroModel == "GR2M", "months", "days"))
    if (input$HydroModel == "GR2M") {
      dateEvent <- trunc(input$EventGR2M, units = "months")
    } else {
      dateEvent <- trunc(input$Event, units = "days")
    }
    data <- getData()$Tab[, c("DatesR", "Qobs", "Qsim", "QsimOld")]
    data$Qsim    <- ifelse(format(data$DatesR, "%Y%m%d") > format(dateEvent, "%Y%m%d"), NA, data$Qsim)
    data$QsimOld <- ifelse(format(data$DatesR, "%Y%m%d") > format(dateEvent, "%Y%m%d"), NA, data$QsimOld)
    data.xts <- xts::xts(data[, -1L, drop = FALSE], order.by = data$DatesR, tzone = "UTC")

    op <- getPlotPar()$par
    dgMDq <- dygraphs::dygraph(data.xts, group = "mod_diag", ylab = paste0("flow [mm/", getPrep()$TMGR$TimeUnit, "]"), main = " ")
    dgMDq <- dygraphs::dyOptions(dgMDq, colors = c(op$fg, "orangered", "grey"), drawPoints = TRUE,
                                 axisLineColor = op$fg, axisLabelColor = op$fg, titleHeight = 10,
                                 retainDateWindow = FALSE, useDataTimezone = TRUE)
    dgMDq <- dygraphs::dySeries(dgMDq, name = "Qsim"   , drawPoints = FALSE)
    dgMDq <- dygraphs::dyEvent(dgMDq, dateEvent, color = "orangered")
    dgMDq <- dygraphs::dySeries(dgMDq, name = "QsimOld", label = "Qold", drawPoints = FALSE, strokePattern = "dashed")
    dgMDq <- dygraphs::dyLegend(dgMDq, show = "onmouseover", width = 225)
    dgMDq <- dygraphs::dyCrosshair(dgMDq, direction = "vertical")
    idNA <- .StartStop(data$Qobs, FUN = is.na)
    dgMDq <- .DyShadingMulti(dygraph = dgMDq, color = rgb(0.5, 0.5, 0.5, alpha = 0.4),
                             ts = data$DatesR, idStart = idNA$start, IdStop = idNA$stop)
  })


  ## Plot model diagram chart
  output$stPlotMD <- renderPlot({
    if (length(getSim()$SIM$OutputsModel$DatesR) < 2) {
      return(NULL)
    }
    # OutputsModel <- getSim()$SIM$OutputsModel
    # IndPlot <- which(OutputsModel$DatesR >= input$Period[1L] & OutputsModel$DatesR <= input$Period[2L])
    # OutputsModel2 <- sapply(OutputsModel[seq_len(which(names(OutputsModel) == "Qsim"))], function(x) x[IndPlot])
    # OutputsModel2 <- c(OutputsModel2, Qobs = list(getSim()$SIM$Qobs[IndPlot]))

    # OutputsModel2 <- getData()$OutputsModel
    # dateEvent <- trunc(input$Event, units = ifelse(input$HydroModel == "GR2M", "months", "days"))
    if (input$HydroModel == "GR2M") {
      SimPer <- trunc(input$Period, units = "months")
      dateEvent <- trunc(input$EventGR2M, units = "months")
    } else {
      SimPer <- trunc(input$Period, units = "days")
      dateEvent <- trunc(input$Event, units = "days")
    }

    par(getPlotPar()$par)
    try(.DiagramGR(OutputsModel = getData()$OutputsModel, Param = getSim()$PARAM,
               SimPer = SimPer, EventDate = dateEvent,
               HydroModel = input$HydroModel, CemaNeige = input$SnowModel == "CemaNeige",
               Theme = .GlobalEnv$.ShinyGR.args$theme),
        silent = TRUE)
  }, bg = "transparent")





  ## --------------- Criteria table

  output$Criteria <- renderTable({

    ## Table created in order to choose order the criteria in the table output
    tabCrit_gauge <- data.frame(Criterion = c("NSE[Q]", "NSE[sqrt(Q)]", "NSE[1/Q]",
                                              "KGE[Q]", "KGE[sqrt(Q)]", "KGE[1/Q]",
                                              "BIAS[Qsim/Qobs]"),
                                ID        = 1:7, stringsAsFactors = FALSE)

    if (length(getSim()$SIMold) == 2 & input$ShowOldQsim == "Yes") {
      tabCrit_old <- getSim()$SIMold[[1L]]$Crit$Value
      tabCrit_val <- cbind(getSim()$Crit, tabCrit_old)
      colnames(tabCrit_val) <- c(colnames(getSim()$Crit), "Qold")
      CellColHisto <- '<div style="color: #808080;"><span>9999</span></div>'
    } else {
      tabCrit_val <- getSim()$Crit
    }
    tabCrit_out <- merge(tabCrit_gauge, tabCrit_val, by = "Criterion", all.x = TRUE)
    tabCrit_out <- tabCrit_out[order(tabCrit_out$ID), ]
    tabCrit_out <- tabCrit_out[, !colnames(tabCrit_out) %in% "ID"]
    tabCrit_out[tabCrit_out <= -99.99] <- -99.99
    tabCrit_out[, seq_len(ncol(tabCrit_out))[-1]] <- sapply(seq_len(ncol(tabCrit_out))[-1], function(x) sprintf("%7.2f", tabCrit_out[, x]))
    tabCrit_out <- as.data.frame(tabCrit_out)
    tabCrit_out[tabCrit_out == " -99.99"] <- "< -99.99"
    colnames(tabCrit_out) <- gsub("Value", "Qsim", colnames(tabCrit_out))
    tabCrit_out$Criterion <- gsub("\\[", " [", tabCrit_out$Criterion)

    ## Color the cell of the crietaia uses during the calibration
    if (CAL_click$valueButton >= 0) {
      CellColCalib <- '<div style="color: #FFFFFF; background-color: #A4C400; border: 5px solid #A4C400; position:relative; top: 0px; left: 5px; padding: 0px; margin: -5px -0px -8px -10px;">
<span>9999</span></div>'
      CellColCalib_id  <- which(tabCrit_out$Criterion == input$TypeCrit)
      tabCrit_out[CellColCalib_id, 2] <- gsub("9999", tabCrit_out[CellColCalib_id, 2], CellColCalib)
    }
    if (input$ShowOldQsim == "Yes" & length(getSim()$SIMold) > 1) {
      tabCrit_out[, "Qold"] <- apply(tabCrit_out[, "Qold", drop = FALSE], 1, function(x) gsub("9999", x, CellColHisto))
    }

    return(tabCrit_out)
  }, align = c("r"), sanitize.text.function = function(x) x)



  ## --------------- Download buttons

  ## Download simulation table
  output$DownloadTab <- downloadHandler(
    filename = function() {
      filename <- "TabSim"
      filename <- sprintf("airGR_%s_%s.csv", filename, gsub("(.*)( )(\\d{2})(:)(\\d{2})(:)(\\d{2})", "\\1_\\3h\\5m\\7s", Sys.time()))
    },
    content = function(file) {
      TabSim <- as.data.frame(getSim()$SIM)
      if (getPrep()$isUngauged) {
        TabSim$Qobs <- NA
      }
      colnames(TabSim) <- sprintf("%s [%s]", colnames(TabSim), c("-", rep("mm", 2), "-", "°C", rep("mm", 2)))
      colnames(TabSim) <- ifelse(grepl("mm", colnames(TabSim)),
                                 gsub("mm", paste0("mm/", .TypeModelGR(getSim()$SIM)$TimeUnit), colnames(TabSim)),
                                 colnames(TabSim))
      write.table(TabSim, file = file, row.names = FALSE, sep = ";")
    }
  )


  ## Download plots
  output$DownloadPlot <- downloadHandler(
    filename = function() {
      filename <- switch(input$PlotType,
                         "Model performance" = "PlotModelPerf",
                         "Flow time series"  = "PlotFlowTimeSeries",
                         "State variables"   = "PlotStateVar",
                         "Model diagram"     = "PlotModelDiag")
      filename <- sprintf("airGR_%s_%s.png", filename, gsub("(.*)( )(\\d{2})(:)(\\d{2})(:)(\\d{2})", "\\1_\\3h\\5m\\7s", Sys.time()))
    },
    content = function(file) {
      k <- 1.75
      ParamTitle <- c("X1", "X2"   , "X3", "X4", "X5", "X6")[seq_len(getPrep()$TMGR$NbParam)]
      ParamUnits <- c("mm", "mm/%s", "mm", "%s",   "", "mm")
      if (input$HydroModel == "GR2M") {
        ParamUnits[2L] <- "[-]%s"
      }
      ParamUnits <- ParamUnits[seq_len(getPrep()$TMGR$NbParam)]
      if (input$SnowModel == "CemaNeige") {
        ParamTitle <- c(ParamTitle, "C1", "C2")
        ParamUnits <- c(ParamUnits,  "", "mm/°C/%s")
      }
      if (input$HydroModel == "GR2M") {
        ParamUnits <- gsub("\\[-\\].*", "", ParamUnits)
      }
      ParamUnits <- sprintf("[%s]", ParamUnits)
      ParamUnits <- gsub("\\[\\]", "[-]", ParamUnits)
      ParamTitle <- paste(ParamTitle, paste(getSim()$PARAM, sprintf(ParamUnits, getPrep()$TMGR$TimeUnit)), sep = " = ", collapse = ", ")
      ParamTitle <- gsub(" ,", ",", ParamTitle)
      PngTitle <- sprintf("%s - %s/%s\n%s\n%s", input$Dataset,
                          input$HydroModel, ifelse(input$SnowModel == "CemaNeige", "CemaNeige", "No snow model"),
                          paste0(input$Period, collapse = " - "),
                          ParamTitle)
      if (getPlotType() == 1) {
        png(filename = file, width = 1000*k, height = ifelse(input$SnowModel != "CemaNeige", 700*k, 1100*k), pointsize = 14, res = 150)
        par(oma = c(0, 0, 4, 0))
        plot(getSim()$SIM)
        mtext(text = PngTitle, side = 3, outer = TRUE, cex = 0.8, line = 1.2)
        dev.off()
      }
      if (getPlotType() == 2) {
        png(filename = file, width = 1000*k, height = 600*k, pointsize = 14, res = 150)
        par(oma = c(0, 0, 4, 0))
        plot(getSim()$SIM, which = c("Precip", "Flows", "Error"))
        mtext(text = PngTitle, side = 3, outer = TRUE, cex = 0.8, line = 1.2)
        dev.off()
      }
      if (getPlotType() == 3) {
        png(filename = file, width = 1000*k, height = 600*k, pointsize = 14, res = 150)
        # OutputsModel <- getSim()$SIM$OutputsModel
        # IndPlot <- which(OutputsModel$DatesR >= input$Period[1L] & OutputsModel$DatesR <= input$Period[2L])
        # OutputsModel2 <- sapply(OutputsModel[seq_len(which(names(OutputsModel) == "Qsim"))], function(x) x[IndPlot])
        # OutputsModel2 <- c(OutputsModel2, Qobs = list(getSim()$SIM$Qobs[IndPlot]))
        #
        # data <- data.frame(DatesR = OutputsModel2$DatesR,
        #                    prod.  = OutputsModel2$Prod,
        #                    rout.  = OutputsModel2$Rout,
        #                    Qr     = OutputsModel2$QR,
        #                    Qd     = OutputsModel2$QD,
        #                    Qsim   = OutputsModel2$Qsim,
        #                    Qobs   = OutputsModel2$Qobs)
        # if (input$HydroModel == "GR6J") {
        #   data$QrExp <- OutputsModel2$QRExp
        # } else {
        #   data$QrExp <- 0
        # }
        colSelec <- c("DatesR",
                      "prod.",
                      "rout.",
                      grep("^Qr$", colnames(getData()$Tab), value = TRUE),
                      grep("^Qd$", colnames(getData()$Tab), value = TRUE),
                      grep("^QrExp|exp", colnames(getData()$Tab), value = TRUE),
                      "Qsim",
                      "Qobs")
        data <- getData()$Tab[, colSelec]
        par(mfrow = c(2, 1), oma = c(3, 0, 4, 0))
        par(mar = c(0.6, 4.0, 0.0, 2.0), xaxt = "n", cex = 0.8)
        if (input$HydroModel != "GR6J") {
          plot(range(data$Dates), range(data$prod., data$rout., na.rm = TRUE),
               type = "n", xlab = "", ylab = "store [mm]")
        }
        if (input$HydroModel == "GR6J") {
          data$exp. <- rowSums(data[, c("exp. (+)", "exp. (-)")], na.rm = TRUE)
          plot(range(data$Dates), range(data$prod., data$rout., data$rout., data$exp.),
               type = "n", xlab = "", ylab = "store [mm]")
        }
        polygon(c(data$Dates, rev(range(data$Dates))), c(data$prod., rep(0, 2)), border = "darkblue", col = adjustcolor("darkblue", alpha.f = 0.30))
        polygon(c(data$Dates, rev(range(data$Dates))), c(data$rout., rep(0, 2)), border = "cyan4"   , col = adjustcolor("cyan4"   , alpha.f = 0.30))
        if (input$HydroModel == "GR6J") {
          minQrExp <- min(data$prod., data$rout., data$exp., 0)
          colQrExp <- ifelse(minQrExp > 0, "#10B510", "#FF0303")
          polygon(c(data$Dates, rev(range(data$Dates))), c(data$exp., rep(0, 2)), border = colQrExp, col = adjustcolor(colQrExp, alpha.f = 0.30))
        }
        if (input$HydroModel != "GR6J") {
          legend("topright", bty = "n", legend = c("prod.", "rout."), cex = 0.8,
                 pt.bg = adjustcolor(c("darkblue", "cyan4"), alpha.f = 0.30),
                 col = c("darkblue", "cyan4"),
                 pch = 22)
        }
        if (input$HydroModel == "GR6J") {
          legend("topright", bty = "n", legend = c("prod.", "rout.", "exp. (+)", "exp. (-)"), cex = 0.8,
                 pt.bg = adjustcolor(c("darkblue", "cyan4", "#10B510", "#FF0303"), alpha.f = 0.30),
                 col = c("darkblue", "cyan4", "#10B510", "#FF0303"),
                 pch = 22)
        }
        par(mar = c(0.0, 4.0, 0.6, 2.0), xaxt = "s")
        plot(data$DatesR, data$Qobs, type = "l", xlab = "", ylab = paste0("flow [mm/", getPrep()$TMGR$TimeUnit, "]"))
        if (input$HydroModel != "GR6J" & input$HydroModel != "GR2M") {
          polygon(c(data$Dates, rev(range(data$Dates))), c(data$Qr+data$Qd, rep(0, 2)), col = "#FFD700", border = NA)
          polygon(c(data$Dates, rev(range(data$Dates))), c(data$Qr        , rep(0, 2)), col = "#EE6300", border = NA)
          legend("topright", bty = "n", legend = c("Qobs", "Qsim", "Qr", "Qd"), cex = 0.8,
                 col = c(par("fg"), "orangered", "#FFD700", "#EE6300"),
                 lwd = c(1, 1, NA, NA), pch = c(20, NA, 15, 15))
        }
        if (input$HydroModel == "GR2M") {
          legend("topright", bty = "n", legend = c("Qobs", "Qsim"), cex = 0.8,
                 col = c(par("fg"), "orangered"),
                 lwd = c(1, 1), pch = c(20, NA))
        }
        if (input$HydroModel == "GR6J") {
          polygon(c(data$Dates, rev(range(data$Dates))), c(data$QrExp+data$Qr+data$Qd, rep(0, 2)), col = "#FFD700", border = NA)
          polygon(c(data$Dates, rev(range(data$Dates))), c(data$QrExp+data$Qr        , rep(0, 2)), col = "#EE6300", border = NA)
          polygon(c(data$Dates, rev(range(data$Dates))), c(data$QrExp                , rep(0, 2)), col = "brown"  , border = NA)
          legend("topright", bty = "n", legend = c("Qobs", "Qsim", "Qd", "Qr", "QrExp"), cex = 0.8,
                 col = c(par("fg"), "orangered", "#FFD700", "#EE6300", "brown"),
                 lwd = c(1, 1, NA, NA, NA), pch = c(20, NA, 15, 15, 15))
        }
        lines(data$DatesR, data$Qsim, lwd = 1, col = "orangered")
        lines(data$DatesR, data$Qobs, lwd = 1, col = par("fg"), type = "o", pch = 20, cex = 0.5)
        mtext(text = PngTitle, side = 3, outer = TRUE, cex = 0.8, line = 0.7)
        box()
        dev.off()
      }
      if (getPlotType() == 4) {
        if (input$HydroModel == "GR2M") {
          dateEvent <- trunc(input$EventGR2M, units = "months")
        } else {
          dateEvent <- input$Event
        }
        isBigTitle <- any(input$SnowModel == "CemaNeige" | input$HydroModel %in% c("GR5J", "GR6J"))
        plotHeight <- ifelse(isBigTitle, 1000, 900)*k
        marginTop <- ifelse(isBigTitle, 7, 6)
        png(filename = file, width = 550*k, height = plotHeight, pointsize = 12, res = 150)
        PngTitleMD <- sprintf("%s - %s/%s\n%s\n%s", input$Dataset,
                            input$HydroModel, ifelse(input$SnowModel == "CemaNeige", "CemaNeige", "No snow model"),
                            dateEvent,
                            ParamTitle)
        if (grepl("X5", PngTitleMD)) {
          PngTitleMD <- gsub(", X5", "\nX5", PngTitleMD)
        } else {
          PngTitleMD <- gsub(", C1", "\nC1", PngTitleMD)
        }

        par(oma = c(0, 1, marginTop, 1))
        .DiagramGR(OutputsModel = getData()$OutputsModel, Param = getSim()$PARAM,
                   SimPer = input$Period, EventDate = dateEvent,
                   HydroModel = input$HydroModel, CemaNeige = input$SnowModel == "CemaNeige")
        mtext(text = PngTitleMD, side = 3, outer = TRUE, cex = 1.2, line = ifelse(isBigTitle, -0.15, 0.6))
        dev.off()
      }
    }
  )



  ## --------------- Summary sheet

  output$Sheet <- renderUI({
    codeRegex <- "\\D{1}\\d{7}"
    codeBH <- gsub(sprintf("(.*)(%s)(.*)", codeRegex), "\\2", input$DatasetSheet)
    urlRegex <- "https://webgr.inrae.fr/wp-content/uploads/fiches/%s_fiche.png"
    urlSheet <- sprintf(urlRegex, codeBH)
    if (.CheckUrl(urlSheet)) {
      tags$p(tags$h6("Click on the image to open it in a new window and to enlarge it."),
             tags$a(href = urlSheet, target = "_blank", rel = "noopener noreferrer",
                    tags$img(src = urlSheet, height = "770px",
                             alt = "If the image does not appear, click on this link.",
                             title = "Click to open in a new window")))
    } else {
      urlSheet <- "fig/sheet_W1110010_thumbnail.png"
      urlWebGR <- "https://webgr.inrae.fr"
      txtWebGR <- "webgr.inrae.fr"
      urlFraDb <- file.path(urlWebGR, "activites/base-de-donnees/")
      txtFraDb <- "All the summary sheets are available on"
      tags$p(tags$h1("Sorry, the summary sheet is not available for this dataset."),
             tags$br(),
             tags$h5("Only sheets of stations of the Banque Hydro French database are available."),
             tags$h5("To display a summary sheet, the name of the chosen dataset has to contain the  Banque Hydro station code (8 characters : 1 letter and 7 numbers)."),
             txtFraDb, tags$a(href = urlFraDb, target = "_blank", rel = "noopener noreferrer", txtWebGR), ".",
             tags$br(),
             tags$br(),
             tags$a(href = urlFraDb, target = "_blank", rel = "noopener noreferrer",
                    tags$img(src = urlSheet, width = "30%", height = "30%",
                             alt = txtWebGR,
                             title = paste("Visit", txtWebGR))))
    }
  })



})

