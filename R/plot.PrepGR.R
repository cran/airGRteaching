plot.PrepGR <- function(x, type = "l", col.Precip = "royalblue", col.Q = "black", col.na = "grey",
                       xlab = NULL, ylab =  NULL, main = NULL, plot.na = TRUE, ...) {

  if (!inherits(x, "PrepGR")) {
    stop("Non convenient data for x argument. Must be of class \"PrepGR\"")
  }

  if (is.null(xlab)) {
    xlab <- "Time"
  }
  if (is.null(ylab)) {
    yunit <- .TypeModelGR(x)$TimeUnit
    ylab  <- paste0(c("precip. [mm/", "flow [mm/"), yunit, "]")
  } else {
    if (length(ylab) < 2) {
      ylab <- c(ylab, "")
    }
  }

  data <- data.frame(DatesR = x$InputsModel$DatesR,
                     Precip = x$InputsModel$Precip,
                     Qobs   = x$Qobs)

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  layout(mat = matrix(1:2), widths = c(1, 2), heights = c(1, 2))

  par(mar = c(0.1, 4, 4, 2), xaxt = "n")
  plot(Precip ~ DatesR, data = data, type = "h", col = col.Precip,
       xlab = "", ylab = ylab[1L], main = main, ylim = rev(range(data$Precip)))

  par(mar = c(5, 4, 0.1, 2), xaxt = "s")
  if (all(is.na(data$Qobs))) {
    par(yaxt = "n")
    plot(x = range(data$DatesR), y = c(0, 0), type = "n",
         xlab = xlab, ylab = ylab[2L], main = "")
    text(x = median(data$DatesR), y = 0, labels = "No observed\ndischarges")
  } else {
    plot(Qobs ~ DatesR, data = data, type = type, col = col.Q,
         xlab = xlab, ylab = ylab[2L], main = "")
  }

  if (plot.na) {
    axis(side = 1, at = as.POSIXct(data$DatesR[is.na(data$Qobs)]),
         labels = FALSE, lwd.ticks = 3, col.ticks = col.na, tck = 0.025, lend = "butt")
    legend("topright", legend = "NA", pch = 15, col = col.na, bty = "n", cex = 0.8)
  }

  box()

}
