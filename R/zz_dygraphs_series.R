## Code from the last version of the 'dygraphs' package that is only available on GitHub
## Will be removed from airGRteaching when the last ‘dygraphs’ version will be available on the CRAN
## https://github.com/rstudio/dygraphs
## License MIT
## RStudio Team
## Many thanks to J.J. Allaire and Petr Shevtsov



# provide a custom plotter if stemPlot has been specified
.resolveStemPlot <- function(stemPlot, plotter) {
  
  # check for stemPlot argument
  if (isTRUE(stemPlot)) {
    
    # verify that a custom plotter hasn't been specified
    if (!is.null(plotter)) {
      stop("stemPlot provides it's own plotter so is incompatible with ",
           "specifying a custom plotter", call. = FALSE)
    }
    
    # provide custom plotter JS
    "function stemPlotter(e) { 
    var ctx = e.drawingContext; 
    var points = e.points; 
    var y_bottom = e.dygraph.toDomYCoord(0);
    ctx.fillStyle = e.color; 
    for (var i = 0; i < points.length; i++) { 
    var p = points[i]; 
    var center_x = p.canvasx;
    var center_y = p.canvasy; 
    ctx.beginPath(); 
    ctx.moveTo(center_x, y_bottom); 
    ctx.lineTo(center_x, center_y); 
    ctx.stroke();
    ctx.beginPath(); 
    ctx.arc(center_x, center_y, 3, 0, 2*Math.PI); 
    ctx.stroke();
    }
  }"
  } else {
    # specified plotter
    plotter
}
  }