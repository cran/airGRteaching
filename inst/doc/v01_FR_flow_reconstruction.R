## ----warning=FALSE, include=FALSE---------------------------------------------
R_bib <- toBibtex(citation())
R_bib <- gsub("@Manual\\{", "@Manual{Rsoftware_man", R_bib)
airGR_bib <- toBibtex(citation("airGR"))
airGR_bib <- gsub("@Article\\{", "@Article{airGR_art", airGR_bib)
airGR_bib <- gsub("@Manual\\{", "@Manual{airGR_man", airGR_bib)
airGRteaching_bib <- toBibtex(citation("airGRteaching"))
airGRteaching_bib <- gsub("@Manual\\{", "@Manual{airGRteaching_man", airGRteaching_bib)
airGRteaching_bib <- gsub("@Article\\{", "@Article{airGRteaching_art", airGRteaching_bib)
airGRdatasets_bib <- toBibtex(citation("airGRdatasets"))
airGRdatasets_bib <- gsub("@Manual\\{", "@Manual{airGRdatasets_man", airGRdatasets_bib)
options(encoding = "UTF-8")
writeLines(text = c(R_bib, airGR_bib, airGRteaching_bib, airGRdatasets_bib), con = "airGR_galaxy.bib")
options(encoding = "native.enc")

## ----include=FALSE------------------------------------------------------------
formatGR      <- '<strong><font color="#0BA6AA">%s</font></strong>'
GR            <- sprintf(formatGR, "GR")
airGR         <- sprintf(formatGR, "airGR")
airGRteaching <- sprintf(formatGR, "airGRteaching")

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.path = "figure/")
Sys.setlocale("LC_TIME", "fr_FR.UTF-8")
library(airGRteaching)
colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color, x)
  } else {
    x
  }
}

## ----format_ts_1, eval=TRUE, echo=FALSE, results='hide'-----------------------
# Catchment data loading
library(airGRdatasets)
data("Y643401001", package = "airGRdatasets")

# Catchment metadata
str(Y643401001$Meta)

# Observed daily time series
ts_obs_d <- Y643401001$TS

# Summary of the time series
summary(ts_obs_d)

## ----include=FALSE------------------------------------------------------------
area <- Y643401001$Meta$Area
name_sta <- gsub("the ", "", Y643401001$Meta$Name)
name_riv <- gsub("(the )(.*)( at.*)", "\\2", name_sta)
year_na <- format(ts_obs_d[is.na(ts_obs_d$Qmmd), "Date"], format = "%Y")
year_na <- names(sort(table(year_na), decreasing = TRUE)[1])

## ----v01_set_per, eval=TRUE, echo=FALSE---------------------------------------
# Calibration period
per_cal_wup <- c("2003-01-01", "2004-12-01")
per_cal_run <- c("2005-01-01", "2010-12-01")

# Evaluation period 
per_eva_wup <- c("2009-01-01", "2010-12-01")
per_eva_run <- c("2011-01-01", "2018-12-01")

# Simulation period
per_sim_wup <- c("1999-01-01", "2000-12-01")
per_sim_run <- c("2001-01-01", "2018-12-01")

## ----v01_regime, eval=TRUE, echo=FALSE----------------------------------------
# Monthly aggregation of observed daily time series
ts_obs_m <- SeriesAggreg(ts_obs_d[, c("Date", "Ptot", "Evap", "Qmmd", "Temp")],
                         Format = "%Y%m",
                         ConvertFun = c("sum", "sum", "sum", "mean"))

## ----eval=TRUE, echo=FALSE----------------------------------------------------
# Information for the vignette
per_all_pct <- range(ts_obs_m$Date)
per_all_for <- format(range(ts_obs_m$Date), format = "%Y")
per_cal_wup_pct <- as.POSIXct(per_cal_wup, tz = "UTC")
per_cal_wup_for <- format(per_cal_wup_pct, format = "%B %Y")
per_cal_run_pct <- as.POSIXct(per_cal_run, tz = "UTC")
per_cal_run_for <- format(per_cal_run_pct, format = "%B %Y")
per_eva_wup_pct <- as.POSIXct(per_eva_wup, tz = "UTC")
per_eva_wup_for <- format(per_eva_wup_pct, format = "%B %Y")
per_eva_run_pct <- as.POSIXct(per_eva_run, tz = "UTC")
per_eva_run_for <- format(per_eva_run_pct, format = "%B %Y")
per_sim_wup_pct <- as.POSIXct(per_sim_wup, tz = "UTC")
per_sim_wup_for <- format(per_sim_wup_pct, format = "%B %Y")
per_sim_run_pct <- as.POSIXct(per_sim_run, tz = "UTC")
per_sim_run_for <- format(per_sim_run_pct, format = "%B %Y")

## ----v01_fig_presentation, echo=FALSE, eval=TRUE, fig.width=7*1.5, fig.height=3*1.5, dev.args=list(pointsize=12), out.width='98%'----
ind_na <- as.matrix(airGRteaching:::.StartStop(ts_obs_m$Qmmd, FUN = is.na))

plot(x = ts_obs_m$Date, y = ts_obs_m$Qmmd, 
     xlab = "time [months]", ylab = "flow [mm/month]",
     type = "l", lwd = 1, 
     panel.first = rect(xleft  = ts_obs_m$Date[ind_na[, 1]], ybottom = -1e6, 
                        xright = ts_obs_m$Date[ind_na[, 2]], ytop    = +1e6,
                        col = "lightgrey", border = NA))
text(x = ts_obs_m$Date[floor(apply(ind_na, MARGIN = 1, median))], 
     y = quantile(range(ts_obs_m$Qmmd, na.rm = TRUE), probs = 0.75), 
     labels = "?", cex = 8, font = 2, col = "orangered")

## ----format_ts_1, echo=TRUE---------------------------------------------------
# Catchment data loading
library(airGRdatasets)
data("Y643401001", package = "airGRdatasets")

# Catchment metadata
str(Y643401001$Meta)

# Observed daily time series
ts_obs_d <- Y643401001$TS

# Summary of the time series
summary(ts_obs_d)

## ----v01_set_per, echo=TRUE---------------------------------------------------
# Calibration period
per_cal_wup <- c("2003-01-01", "2004-12-01")
per_cal_run <- c("2005-01-01", "2010-12-01")

# Evaluation period 
per_eva_wup <- c("2009-01-01", "2010-12-01")
per_eva_run <- c("2011-01-01", "2018-12-01")

# Simulation period
per_sim_wup <- c("1999-01-01", "2000-12-01")
per_sim_run <- c("2001-01-01", "2018-12-01")

## ----v01_regime, echo=TRUE----------------------------------------------------
# Monthly aggregation of observed daily time series
ts_obs_m <- SeriesAggreg(ts_obs_d[, c("Date", "Ptot", "Evap", "Qmmd", "Temp")],
                         Format = "%Y%m",
                         ConvertFun = c("sum", "sum", "sum", "mean"))

## ----echo=TRUE, eval=TRUE, warning=FALSE--------------------------------------
# Data processing for GR2M
prep <- PrepGR(DatesR     = ts_obs_m$Date,
               Precip     = ts_obs_m$Ptot,
               PotEvap    = ts_obs_m$Evap, 
               Qobs       = ts_obs_m$Qmmd,
               HydroModel = "GR2M", 
               CemaNeige  = FALSE)

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
# Parameter set to test
i_param_gr2m <- c(X1 = 380, X2 = 0.92)

# Simulation over the calibration period
i_sim_manu <- SimGR(PrepGR  = prep, 
                    Param   = i_param_gr2m,
                    EffCrit = "NSE",
                    WupPer  = per_cal_wup, 
                    SimPer  = per_cal_run,
                    verbose = TRUE)

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
# Calibration criterion
GetCrit(i_sim_manu)

## ----v01_fig_cal_manu_gr2m, echo=TRUE, eval=TRUE, warning=FALSE, fig.width=7*1.5, fig.height=5*1.5, dev.args=list(pointsize=16), out.width='98%'----
# Graphical assessment of the calibration performance
plot(i_sim_manu)

## ----v01_fig_cal_auto_gr2m, echo=TRUE, eval=TRUE, fig.width=7*1.5, fig.height=5*1.5, dev.args=list(pointsize=16), out.width='98%'----
# Calibration
cal_auto <- CalGR(PrepGR  = prep, 
                  CalCrit = "NSE",
                  WupPer  = per_cal_wup, 
                  CalPer  = per_cal_run,
                  verbose = TRUE)

# Graphical assessment of the calibration performance
plot(cal_auto)

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
# Get parameter values
param_cal <- GetParam(cal_auto)
param_cal

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
# Get criterion value
GetCrit(cal_auto)

## ----v01_ts_q, echo=TRUE, eval=TRUE-------------------------------------------
# Combination of observed and simulated streamflow time series on the calibration period
ts_cal <- as.data.frame(cal_auto)

# Combination of observed and simulated streamflow time series on the entire period
ts_cal_all <- merge(x = ts_obs_m[, "Date", drop = FALSE], y = ts_cal, 
                    by.x = "Date", by.y = "Dates", 
                    all.x = TRUE)

## ----v01_fig_cal, echo=FALSE, eval=TRUE, fig.width=7*1.5, fig.height=3*1.5, dev.args=list(pointsize=12), out.width='98%'----
ind_wup <- ts_obs_m$Date >= per_cal_wup_pct[1] & ts_obs_m$Date <= per_cal_run_pct[1]
ind_wup <- range(which(ind_wup))

plot(x = ts_obs_m$Date, y = ts_obs_m$Qmmd, 
     xlab = "time [months]", ylab = "flow [mm/month]",
     main = "Simulation on the calibration period",
     type = "l", lwd = 1, 
     panel.first = list(rect(xleft  = ts_obs_m$Date[ind_na[, 1]], ybottom = -1e6, 
                             xright = ts_obs_m$Date[ind_na[, 2]], ytop    = +1e6,
                             col = "lightgrey", border = NA),
                        rect(xleft  = ts_obs_m$Date[ind_wup[1]], ybottom = -1e6, 
                             xright = ts_obs_m$Date[ind_wup[2]], ytop    = +1e6,
                             col = adjustcolor("orangered", 0.6), border = NA)))
lines(ts_cal$Date, ts_cal$Qsim, lwd = 1, col = "orangered")

legend("topright", 
       legend = c("Qobs", "Qsim", "warm-up", "NA"), 
       pch = c(NA, NA, 15, 15), 
       lwd = c(1, 1, NA, NA), pt.cex = c(NA, NA, 2, 2), 
       col = c("black", "orangered", adjustcolor("orangered", 0.6), "lightgray"),
       bg = "white")

## ----v01_eval, echo=TRUE, eval=TRUE-------------------------------------------
# Simulation over the evaluation period
eva <- SimGR(PrepGR  = prep, 
             Param   = param_cal,
             WupPer  = per_eva_wup, 
             SimPer  = per_eva_run,
             EffCrit = "NSE",
             verbose = FALSE)

# Get the criterion value
GetCrit(eva)

# Combination of observed and simulated streamflow time series
ts_eva <- as.data.frame(eva)

## ----v01_fig_eval, echo=FALSE, eval=TRUE, fig.width=7*1.5, fig.height=3*1.5, dev.args=list(pointsize=12), out.width='98%'----
ind_wup <- range(which(ts_obs_m$Date >= per_eva_wup_pct[1] & ts_obs_m$Date <= per_eva_run_pct[1]))

plot(x = ts_obs_m$Date, y = ts_obs_m$Qmmd, 
     xlab = "time [months]", ylab = "flow [mm/month]",
     main = "Simulation on the evaluation period",
     type = "l", lwd = 1, 
     panel.first = list(rect(xleft  = ts_obs_m$Date[ind_na[, 1]], ybottom = -1e6, 
                             xright = ts_obs_m$Date[ind_na[, 2]], ytop    = +1e6,
                             col = "lightgrey", border = NA),
                        rect(xleft  = ts_obs_m$Date[ind_wup[1]], ybottom = -1e6, 
                             xright = ts_obs_m$Date[ind_wup[2]], ytop    = +1e6,
                             col = adjustcolor("orangered", 0.6), border = NA)))
lines(ts_eva$Date, ts_eva$Qsim, lwd = 1, col = "orangered")

legend("topright", 
       legend = c("Qobs", "Qsim", "warm-up", "NA"), 
       pch = c(NA, NA, 15, 15), 
       lwd = c(1, 1, NA, NA), pt.cex = c(NA, NA, 2, 2), 
       col = c("black", "orangered",  adjustcolor("orangered", 0.6), "lightgray"),
       bg = "white")

## ----v01_sim, echo=TRUE, eval=TRUE--------------------------------------------
# Simulation over the entire period
sim <- SimGR(PrepGR  = prep, 
             Param   = param_cal,
             WupPer  = per_sim_wup, 
             SimPer  = per_sim_run,
             EffCrit = "NSE",
             verbose = FALSE)

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
# Get the criterion value
GetCrit(sim)

# Combination of observed and simulated streamflow time series
ts_sim <- as.data.frame(sim)

## ----v01_fig_sim, echo=FALSE, eval=TRUE, fig.width=7*1.5, fig.height=3*1.5, dev.args=list(pointsize=12), out.width='98%'----
ind_wup <- range(which(ts_obs_m$Date >= per_sim_wup_pct[1] & ts_obs_m$Date <= per_sim_run_pct[1]))

plot(x = ts_obs_m$Date, y = ts_obs_m$Qmmd, 
     xlab = "time [months]", ylab = "flow [mm/month]",
     main = "Simulation on the entire period",
     type = "l", lwd = 1, 
     panel.first = list(rect(xleft  = ts_obs_m$Date[ind_na[, 1]], ybottom = -1e6, 
                             xright = ts_obs_m$Date[ind_na[, 2]], ytop    = +1e6,
                             col = "lightgrey", border = NA),
                        rect(xleft  = ts_obs_m$Date[ind_wup[1]], ybottom = -1e6, 
                             xright = ts_obs_m$Date[ind_wup[2]], ytop    = +1e6,
                             col = adjustcolor("orangered", 0.6), border = NA)))
lines(ts_sim$Date, ts_sim$Qsim, lwd = 1, col = "orangered")

legend("topright", 
       legend = c("Qobs", "Qsim", "warm-up", "NA"), 
       pch = c(NA, NA, 15, 15), 
       lwd = c(1, 1, NA, NA), pt.cex = c(NA, NA, 2, 2), 
       col = c("black", "orangered",  adjustcolor("orangered", 0.6), "lightgray"),
       bg = "white")

