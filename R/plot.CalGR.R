plot.CalGR <- function(x, xlab = NULL, ylab =  NULL, main = NULL, which = c("perf", "iter", "ts"), ...) {

  if (! any(class(x) %in% "CalGR")) {
    stop("Non convenient data for x argument. Must be of class \"CalGR\"")
  }
  if (! any(which %in% c("perf", "iter", "ts"))) {
    stop("Non convenient data for which argument. Must be of class \"perf\", \"iter\" or \"ts\"")
  }

  nbParamX <- .TypeModelGR(x)$NbParam #as.numeric(gsub("\\D", "", x$TypeModel))
  nbParamC <- ifelse(.TypeModelGR(x)$CemaNeige, 2, 0)
  nbParam <- nbParamX + nbParamC
  nmParam <- c(sprintf("X%i", 1:nbParamX), sprintf("C%i", seq_len(nbParamC)))

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  if (any(which[1L] %in% c("perf"))) {
    plot(x$OutputsModel, Qobs = x$Qobs, ...)
  }

  if (any(which[1L] %in% c("iter"))) {
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
  }
  # if (which[1L] %in%  c("both")) {
  #   layout.mat <- rbind(layout.mat, max(layout.mat)+1, max(layout.mat)+2)
  #   layout(mat = layout.mat, widths = layout.w, heights = layout.h)
  # }
  if (which[1L] %in% c("ts")) {
    layout(mat = matrix(1:2), widths = c(1, 2), heights = c(1, 2))
  }

  if (any(which[1L] %in% c("iter"))) {
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
  }

  if (any(which[1L] %in%  c("ts"))) {
    if (is.null(xlab)) {
      xlab <- "Time"
    }
    if (is.null(ylab)) {
      ylab  <- paste0(c("flow [mm/", "precip. [mm/"), .TypeModelGR(x)$TimeUnit, "]")
    } else {
      if (length(ylab) < 2) {
        ylab <- c(ylab, "")
      }
    }
    data <- data.frame(DatesR = x$OutputsModel$DatesR,
                       Precip = x$OutputsModel$Precip,
                       Qobs   = x$Qobs,
                       Qsim   = x$OutputsModel$Qsim)

    par(mar = c(0.1, 4, 4, 1), xaxt = "n")
    plot(Precip ~ DatesR, data = data, type = "h", col = "royalblue",
         xlab = "", ylab = ylab[2L], main = main, ylim = rev(range(data$Precip)))

    par(mar = c(5, 4, 0.1, 1), xaxt = "s")
    matplot(data$DatesR, data[, c("Qobs", "Qsim")], type = "l", col = c("black", "orangered"), lty = 1,
         xlab = xlab, ylab = ylab[1L], main = "", xaxt = "n")
    axis.POSIXct(side = 1, x = data$DatesR)

    # if (plot.na) {
    #   axis(side = 1, at = as.POSIXct(data$DatesR[is.na(data$Qobs)]),
    #        labels = FALSE, lwd.ticks = 3, col.ticks = col.na, tck = 0.025, lend = "butt")
    #
    #   legend("topright", legend = "NA", pch = 15, col = col.na, bty = "n", cex = 0.8)
    # }
  }
  # box()

}
