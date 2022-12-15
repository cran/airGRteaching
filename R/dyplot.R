
dyplot <- function(x, Qsup = NULL, Qsup.name = "Qsup",
                   col.Precip = c("royalblue", "lightblue"),
                   col.Q = c("black", "orangered", "grey"), col.na = "lightgrey",
                   ylab = NULL, main = NULL,
                   plot.na = TRUE, RangeSelector = TRUE, Roller = FALSE,
                   LegendShow = c("follow", "auto", "always", "onmouseover", "never"), ...) {
  UseMethod("dyplot")
}

# -----

dyplot.PrepGR <- function(x, Qsup = NULL, Qsup.name = "Qsup",
                         col.Precip = c("royalblue", "lightblue"),
                         col.Q = c("black", "orangered", "grey"), col.na = "lightgrey",
                         ylab = NULL, main = NULL,
                         plot.na = TRUE, RangeSelector = TRUE, Roller = FALSE,
                         LegendShow = c("follow", "auto", "always", "onmouseover", "never"), ...) {
  if (!inherits(x, "PrepGR")) {
    stop("Non convenient data for x argument. Must be of class \"PrepGR\"")
  }
  .dyplot(x, Qsup = Qsup, Qsup.name = Qsup.name,
          col.Precip = col.Precip,
          col.Q = col.Q, col.na = col.na,
          ylab = ylab, main = main,
          plot.na = plot.na, RangeSelector = RangeSelector, Roller = Roller,
          LegendShow = LegendShow, ...)
}

# -----

dyplot.CalGR <- function(x, Qsup = NULL, Qsup.name = "Qsup",
                         col.Precip = c("royalblue", "lightblue"),
                         col.Q = c("black", "orangered", "grey"), col.na = "lightgrey",
                         ylab = NULL, main = NULL,
                         plot.na = TRUE, RangeSelector = TRUE, Roller = FALSE,
                         LegendShow = c("follow", "auto", "always", "onmouseover", "never"), ...) {
  if (!inherits(x, "CalGR")) {
    stop("Non convenient data for x argument. Must be of class \"CalGR\"")
  }
  .dyplot(x, Qsup = Qsup, Qsup.name = Qsup.name,
          col.Precip = col.Precip,
          col.Q = col.Q, col.na = col.na,
          ylab = ylab, main = main,
          plot.na = plot.na, RangeSelector = RangeSelector, Roller = Roller,
          LegendShow = LegendShow, ...)
}

# -----

dyplot.SimGR <- function(x, Qsup = NULL, Qsup.name = "Qsup",
                         col.Precip = c("royalblue", "lightblue"),
                         col.Q = c("black", "orangered", "grey"), col.na = "lightgrey",
                         ylab = NULL, main = NULL,
                         plot.na = TRUE, RangeSelector = TRUE, Roller = FALSE,
                         LegendShow = c("follow", "auto", "always", "onmouseover", "never"), ...) {
  if (!inherits(x, "SimGR")) {
    stop("Non convenient data for x argument. Must be of class \"SimGR\"")
  }
  .dyplot(x, Qsup = Qsup, Qsup.name = Qsup.name,
          col.Precip = col.Precip,
          col.Q = col.Q, col.na = col.na,
          ylab = ylab, main = main,
          plot.na = plot.na, RangeSelector = RangeSelector, Roller = Roller,
          LegendShow = LegendShow, ...)
}


.dyplot <- function(x, Qsup = NULL, Qsup.name = "Qsup",
                    col.Precip = c("royalblue", "lightblue"), col.Q = c("black", "orangered", "grey"), col.na = "lightgrey",
                    ylab = NULL, main = NULL,
                    plot.na = TRUE, RangeSelector = TRUE, Roller = FALSE,
                    LegendShow = c("follow", "auto", "always", "onmouseover", "never"), ...) {

  # barChartPrecip <- scan(file = system.file("plugins/barChartPrecip.js", package = "airGRteaching"),
  #                        what = "character", quiet = TRUE)

  if (!inherits(x, c("PrepGR", "CalGR", "SimGR"))) {
    stop("Non convenient data for x argument. Must be of class \"PrepGR\", \"CalGR\" or \"SimGR\"")
  }

  if (is.null(ylab)) {
    yunit <- .TypeModelGR(x)$TimeUnit
    ylab  <- paste0(c("flow [mm/", "precip. [mm/"), yunit, "]")
  } else {
    if (length(ylab) < 2) {
      ylab <- c(ylab, "")
    }
  }

  if (is.null(Qsup)) {
    Qsup <- as.numeric(rep(NA, length.out = length(x$Qobs)))
  }
  if (!is.numeric(Qsup)) {
    stop("'Qsup' must be numeric")
  }
  if (length(Qsup) != length(x$Qobs)) {
    stop("Incorrect length of 'Qsup', must be of length ", length(x$Qobs))
  }
  if (!is.character(Qsup.name)) {
    Qsup.name <- as.character(Qsup.name)
  }


  if (inherits(x, "PrepGR")) {
    data <- data.frame(DatesR = x$InputsModel$DatesR,
                       Precip = x$InputsModel$Precip,
                       Qobs   = x$Qobs,
                       Qsim   = NA,
                       Qsup   = Qsup)
    if (grepl("CemaNeige", x$TypeModel)) {
      data$Psol <- rowMeans(as.data.frame(x$InputsModel$LayerPrecip) * as.data.frame(x$InputsModel$LayerFracSolidPrecip), na.rm = TRUE)
      data$Pliq <- data$Precip - data$Psol
      data$Precip <- NULL
    }
  } else {
    data <- data.frame(DatesR = x$OutputsModel$DatesR,
                       Precip = x$OutputsModel$Precip,
                       Qobs   = x$Qobs,
                       Qsim   = x$OutputsModel$Qsim,
                       Qsup   = Qsup)
    if (grepl("CemaNeige", x$TypeModel)) {
      data$Psol <- rowMeans(sapply(x$OutputsModel$CemaNeigeLayers, function(x) x$Psol))
      data$Pliq <- rowMeans(sapply(x$OutputsModel$CemaNeigeLayers, function(x) x$Pliq))
      data$Precip <- NULL
    }
  }
  data.xts <- xts::xts(data[, -1L], order.by = data$DatesR, tz = "UTC")


  rgba <- function(x, alpha = 1) {
    sprintf("rgba(%s, %f)", paste0(col2rgb(x), collapse = ", "), alpha)
  }
  if (length(col.Q) == 1) {
    col.Q <- c(rgba(col.Q), rgba(col.Q, alpha = 0.5), rgba(col.Q, alpha = 0.3))
  }
  if (length(col.Q) == 2) {
    col.Q <- c(rgba(col.Q[1L]), rgba(col.Q[2L]), rgba(col.Q[2L], alpha = 0.5))
  }
  if (length(col.Precip) < 2) {
    col.Precip <- c(rgba(col.Precip), rgba(col.Precip, alpha = 0.5))
  }


  if (grepl("CemaNeige", x$TypeModel)) {
    Plim <- c(-1e-3, max(data$Psol+data$Pliq, na.rm = TRUE))
  } else {
    Plim <- c(-1e-3, max(data$Precip, na.rm = TRUE))
    col.Precip <- col.Precip[1L]
  }


  dg <- dygraphs::dygraph(data.xts, main = main, ...)
  dg <- dygraphs::dySeries(dygraph = dg, name = "Qobs", axis = "y", color = col.Q[1L], drawPoints = TRUE)
  dg <- dygraphs::dySeries(dygraph = dg, name = "Qsim", axis = "y", color = col.Q[2L])
  dg <- dygraphs::dySeries(dygraph = dg, name = "Qsup", axis = "y", color = col.Q[3L], label = Qsup.name, strokePattern = "dashed")
  dg <- dygraphs::dyStackedBarGroup(dygraph = dg, name = rev(grep("^P", colnames(data.xts), value = TRUE)), axis = "y2", color = (col.Precip))
  dg <- dygraphs::dyAxis(dygraph = dg, name = "y" , label = ylab[1L],
                         valueRange = range(data.xts[, grep("^Q", colnames(data.xts))], na.rm = TRUE) * c(0.01, 1.59))
  dg <- dygraphs::dyAxis(dygraph = dg, name = "y2", label = ylab[2L], independentTicks = FALSE,
                         valueRange = rev(Plim) * c(2.99, 0.01))
  if (RangeSelector) {
    dg <- dygraphs::dyRangeSelector(dygraph = dg, height = 15)
  }
  if (plot.na) {
    idNA <- .StartStop(data$Qobs, FUN = is.na)
    dg <- .DyShadingMulti(dygraph = dg, color = col.na,
                          ts = data$DatesR, idStart = idNA$start, IdStop = idNA$stop)
  }
  if (Roller) {
    dg <- dygraphs::dyRoller(dygraph = dg, rollPeriod = 5)
  }
  if (is.numeric(Roller)) {
    dg <- dygraphs::dyRoller(dygraph = dg, rollPeriod = Roller)
  }
  if (any(LegendShow %in% c("follow", "auto", "always", "onmouseover", "never"))) {
    dg <- dygraphs::dyLegend(dygraph = dg, show = LegendShow[1L])
  }
  dg <- dygraphs::dyOptions(dygraph = dg, useDataTimezone = TRUE)
  dg

}


