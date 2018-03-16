## Code from the last version of the 'dygraphs' package that is only available on GitHub
## Will be removed from airGRteaching when the last ‘dygraphs’ version will be available on the CRAN
## https://github.com/rstudio/dygraphs
## License MIT
## RStudio Team
## Many thanks to J.J. Allaire and Petr Shevtsov



.mergeLists <- function (base_list, overlay_list, recursive = TRUE) {
  if (length(base_list) == 0)
    overlay_list
  else if (length(overlay_list) == 0)
    base_list
  else {
    merged_list <- base_list
    for (name in names(overlay_list)) {
      base <- base_list[[name]]
      overlay <- overlay_list[[name]]
      if (is.list(base) && is.list(overlay) && recursive)
        merged_list[[name]] <- .mergeLists(base, overlay)
      else {
        merged_list[[name]] <- NULL
        merged_list <- append(merged_list,
                              overlay_list[which(names(overlay_list) %in% name)])
      }
    }
    merged_list
  }
}


.resolveStrokePattern <- function (strokePattern) {
  if (is.character(strokePattern)) {
    if (strokePattern == "dotted") 
      strokePattern <- c(2, 2)
    else if (strokePattern == "dashed") 
      strokePattern <- c(7, 3)
    else if (strokePattern == "dotdash") 
      strokePattern <- c(7, 2, 2, 2)
    else if (strokePattern == "solid") 
      strokePattern <- c(1, 0)
    else stop("Invalid stroke pattern: valid values are dotted, ", 
              "dashed, and dotdash")
  }
  strokePattern
}
