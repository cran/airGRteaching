## Code from the last version of the 'dygraphs' package that is only available on GitHub
## Will be removed from airGRteaching when the last ‘dygraphs’ version will be available on the CRAN
## https://github.com/rstudio/dygraphs
## License MIT
## RStudio Team
## Many thanks to J.J. Allaire and Petr Shevtsov



#' @rdname Plotters
#' @export
#' 
.dyStackedBarGroup <- function(dygraph, name, ...) {
  dots <- list(...)
  
  if(length(name) < 2) {
    dygraph <- do.call(.dyBarSeries, c(list(dygraph = dygraph, name = unlist(name)), dots))
    return(dygraph)
  }
  
  file <- system.file("plotters/stackedbargroup.js", package = "airGRteaching")
  plotter_ <- paste0(readLines(file, skipNul = TRUE), collapse = "\n")
  
  dygraph <- do.call(.dyGroup, c(list(dygraph = dygraph, name = name, plotter = plotter_), dots))
  
  dygraph <- .computeYaxisRange(dygraph, name)
  dygraph 
}


#' @rdname Plotters
#' @export
.dyBarSeries <- function(dygraph, name, ...) {
  file <- system.file("plotters/barseries.js", package = "airGRteaching")
  plotter_ <- paste0(readLines(file, skipNul = TRUE), collapse = "\n")
  
  dots <- list(...)
  do.call(dygraphs::dySeries, c(list(dygraph = dygraph, name = name, plotter = plotter_), dots))
  
}


#' @rdname Plotters
#' @export
.dyStackedRibbonGroup <- function(dygraph, name, ...) {
  dots <- list(...)
  
  if(length(name) < 2) {
    dygraph <- do.call('dyFilledLine', c(list(dygraph = dygraph, name = name), dots))
    return(dygraph)
  }
  
  file <- system.file("plotters/stackedribbongroup.js", package = "airGRteaching")
  plotter_ <- paste0(readLines(file, skipNul = TRUE), collapse = "\n")
  
  dygraph <- do.call(.dyGroup, c(list(dygraph = dygraph, name = name, plotter = plotter_), dots))
  
  dygraph <- .computeYaxisRange(dygraph, name)
  dygraph 
}


.computeYaxisRange <- function(dygraph, name) { 
  # most of what happens from here on out is a simplified version of the 
  # stackPoints and computeYaxis functions in the underlying dygraph package.
  # Since we can't modify the Yaxis range from within the specialized plotter,
  # we need to calculate an appropriate valueRange for the group's axis here, then
  # reconcile that against user-provided ranges and then pass into the widget... this
  # way we ensure that the newly stacked data (but initially unstacked to the widget) 
  # doesn't get cutoff by an axis range - computed by the widget - that won't consider
  # the points stacked
  attrs <- dygraph$x$attrs
  
  data <- attr(dygraph$x, "data")
  cols <- which(names(data) %in% name)
  
  #get all the series, minus the x axis, that are not part of the group
  data_ <- data[-c(1, cols)]
  name_ <- names(data)[which(!names(data) %in% name)][-1]
  
  #grab the axis for the group, we'll calculate the new range only on this axis  
  series <- attrs$series[[name[1]]]
  axisNm <- series$axis
  if (!is.null(axisNm)) {
    axis <- attrs$axes[[axisNm]]
    valueRange <- axis$valueRange
  } else {
    axisNm<-"y"
    axis <- attrs$axes[[axisNm]] <- NULL
    valueRange <- NULL
  }
  
  if(is.null(valueRange)) {
    valueRange <- c(0, 0)
  }
  
  # get the group data fields
  data <- data[cols]
  
  for(i in 1:length(data)) {
    # get the data points
    points <- data[i][[1]]
    # fill NAs... we're not saving this data back into the graph, so this is OK
    is.na(points) <- 0
    
    # add to cumulativeList
    if(!exists('cumulativeYval')) cumulativeYval <- points
    else cumulativeYval <- cumulativeYval + points
    # calculate extremes
    extremes <- range(cumulativeYval)
  }
  
  for(i in 1:length(data_)) {
    if (length(data_)==0) break
    
    # ranges are calcuated separately, so skip those from other axes
    series_ <- attrs$series[[name_[i]]]
    if (!is.null(series_$axis) && series_$axis != axisNm) next
    
    points <- data_[i][[1]]
    
    # fill NAs
    is.na(points) <- 0
    
    # getExtremes
    extremes_ <- range(points)
    extremes[1] <- min(extremes[1], extremes_[1])  
    extremes[2] <- max(extremes[2], extremes_[2])  
  }
  
  valueRange[1] <- min(extremes[1], valueRange[1])
  valueRange[2] <- max(extremes[2], valueRange[2])
  
  # add a little padding since we're hard-setting the range
  valueRange[2] <- valueRange[2] + 0.05 * abs(valueRange[2] - valueRange[1])
  
  axis$options$valueRange <- valueRange
  attrs$axes[[axisNm]] <- axis$options  
  
  # return modified dygraph
  dygraph$x$attrs <- .mergeLists(dygraph$x$attrs, attrs)
  return (dygraph)
}