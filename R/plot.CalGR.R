plot.CalGR <- function(x, xlab = NULL, ylab =  NULL, main = NULL, which = "synth", log_scale = FALSE, ...) {

  if (!inherits(x, "CalGR")) {
    stop("Non convenient data for x argument. Must be of class \"CalGR\"")
  }

  # check 'which' argument
  whichDashboard <- c("all", "synth", "ts", "perf")
  whichUnique    <- c("Precip", "PotEvap", "ActuEvap", "Temp", "SnowPack", "Flows", "Error", "Regime", "CumFreq", "CorQQ")
  whichTeaching  <- c("iter")
  whichAll       <- c(whichDashboard, whichUnique, whichTeaching)
  which <- match.arg(arg = which, choices = whichAll, several.ok = TRUE)

  # dave and reset graphical parameters
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  if (any(which[1L] %in% c("iter"))) {

    # model features
    nbParamX <- .TypeModelGR(x)$NbParam
    nbParamC <- ifelse(.TypeModelGR(x)$CemaNeige, 2, 0)
    nbParam <- nbParamX + nbParamC
    nmParam <- c(sprintf("X%i", 1:nbParamX), sprintf("C%i", seq_len(nbParamC)))

    # plot arrangements
    layout.list <- list(matrix(c(1:2), ncol = 2),
                        matrix(c(1:3, 3), ncol = 2),
                        matrix(NA),
                        matrix(c(1:5, 5), ncol = 3),
                        matrix(c(1:5, 0, 6, 6), ncol = 4),
                        matrix(c(1:7, 7), ncol = 4),
                        matrix(c(1:5, 0, 6:8, 8), ncol = 5),
                        matrix(c(1:9, 9), ncol = 5))
    layout.mat <- layout.list[[nbParam]]
    layout.w <- c(rep(1, ncol(layout.mat)-1), 2)
    layout.h <- rep(1, nrow(layout.mat))
    layout(mat = layout.mat, widths = layout.w, heights = layout.h)

    # graphic labels
    ParamLab <- data.frame(Name  = c(sprintf("X%i", 1:6), sprintf("C%i", 1:2)),
                           Label = c("prod. store capacity [mm]",
                                     "intercatchment exch. coef. [mm/TimeUnit]",
                                     "routing store capacity [mm]",
                                     "UH time constant [TimeUnit]",
                                     "intercatchment exch. threshold [-]",
                                     "coef. for emptying exp. store [mm]",
                                     "weight for snowpack thermal state [-]",
                                     "degree-day melt coef. [mm/degC/TimeUnit]"))
    ParamLab$Label <- gsub("TimeUnit", substr(.TypeModelGR(x)$TimeUnit, 1, 1), ParamLab$Label)

    # graphic display
    par(mar = c(2.5, 3.5, 2.0, 1.0), mgp = c(2.0, 0.6, 0.0), oma = c(0, 0, 4, 0))
    for (i in seq_len(ncol(x$OutputsCalib$HistParamR))) {
      plot(x$OutputsCalib$HistParamR[, i],
           col = ifelse(grepl("X", nmParam[i]), "darkorchid3", "darkolivegreen3"),
           type = "b", pch = 19, cex = 0.8,
           xlab = "", ylab = ParamLab[ParamLab$Name == nmParam[i], "Label"],
           main = nmParam[i])
    }
    plot(x$OutputsCalib$HistCrit,
         col = "tomato",
         type = "b", pch = 19, cex = 1.0,
         xlab = "", ylab = "",
         main = gsub(".*_", "", x$CalCrit))
    mainIter <- "Evolution of parameters and efficiency criterion\nduring the iterations of the steepest-descent step"
    title(main = ifelse(is.null(main), mainIter, main),
          outer = TRUE, line = 0.6, cex.main = 1.6)

  } else {

    # remove not allowed airGR graphic (if not in 1st position in the combined vector)
    which <- setdiff(which, "iter")

    # graphic display
    plot(x$OutputsModel, Qobs = x$Qobs, which = which, log_scale = log_scale, ...)

  }

}
