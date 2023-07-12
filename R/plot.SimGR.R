plot.SimGR <- function(x,  which = "synth", log_scale = FALSE, ...) {

  if (!inherits(x, "SimGR")) {
    stop("Non convenient data for x argument. Must be of class \"SimGR\"")
  }

  # NA in Qobs
  isQobs <- !all(is.na(x$Qobs))
  if (!isQobs) {
    whichNeedQobs  <- c("Error", "CorQQ")
    warnMsgNoQobs  <- "the %s plot(s) cannot be drawn if there is no 'Qobs'"
    if (all(which %in% whichNeedQobs)) {
      stop(sprintf(warnMsgNoQobs, paste0(shQuote(intersect(which, whichNeedQobs)), collapse = " and ")))
    } else if (any(which %in% whichNeedQobs)) {
      warning(sprintf(warnMsgNoQobs, paste0(shQuote(intersect(which, whichNeedQobs)), collapse = " and ")))
    }
  }

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  plot(x$OutputsModel, Qobs = x$Qobs, which = which, log_scale = log_scale, ...)

}
