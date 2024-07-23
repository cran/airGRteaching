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
library(airGRteaching)
Sys.setlocale("LC_TIME", "English")
colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color, x)
  } else {
    x
  }
}

## ----v02_format_ts_1_ini, include=FALSE---------------------------------------
# Catchment data loading
library(airGRdatasets)
data("B222001001", package = "airGRdatasets")

# Ctachment area
area <- B222001001$Meta$Area

# Observed daily time series
ts_init <- B222001001$TS

# Add date information to the time series
ts_init$Year     <- format(ts_init$Date, format = "%Y")
ts_init$MonthDay <- format(ts_init$Date, format = "%m-%d")

# Display of the 1st time steps of the time series
head(ts_init)

## ----include=FALSE------------------------------------------------------------
name_sta <- gsub("the ", "", B222001001$Meta$Name)
name_riv <- gsub("(the )(.*)( at.*)", "\\2", name_sta)

## ----v02_set_per, echo=FALSE, eval=TRUE, include=FALSE------------------------
# Calibration period
per_cal_hist <- c("2000-09-01", "2018-08-31")

# Forecasting period
per_fcst <- c("2018-09-01", "2018-10-20")

# Warm-up period to simulate on the historical period 
per_wup_hist <- c("1999-01-01", "2000-08-31")

# Warm-up period to simulate on the forcasting period
per_wup_fcst <- c(per_wup_hist[1], per_cal_hist[2])

# Forecasting dates
dates_fcst <- seq(from = as.POSIXct(per_fcst[1], tz = "UTC", format = "%Y-%m-%d"), 
                  to   = as.POSIXct(per_fcst[2], tz = "UTC", format = "%Y-%m-%d"), 
                  by   = "1 day")
head(dates_fcst)

# Formatting of forecast dates (Month-Day)
month_day_fcst <- format(dates_fcst, "%m-%d")
head(month_day_fcst)

## ----v02_format_ts_2_gph, include=FALSE---------------------------------------
# Set values of the last winter to missing data
ts_plot <- ts_init
isd_wint <- ts_plot$Date >= as.POSIXct(per_fcst[1], tz = "UTC", format = "%Y-%m-%d")
ts_plot[isd_wint, c("Ptot", "Temp", "Evap", "Qls", "Qmmd")] <- NA

# Display of the last time steps of the time series
tail(ts_plot)

## ----echo=FALSE, eval=TRUE----------------------------------------------------
per_wup_pct <- as.POSIXct(per_wup_hist, tz = "UTC", format = "%Y-%m-%d")
per_wup_for <- format(per_wup_pct, "%e %B %Y")
per_cal_pct <- as.POSIXct(per_cal_hist, tz = "UTC", format = "%Y-%m-%d")
per_cal_for <- format(per_cal_pct, "%e %B %Y")
per_fcst_pct <- as.POSIXct(per_fcst, tz = "UTC", format = "%Y-%m-%d")
per_fcst_for <- format(per_fcst_pct, "%e %B %Y")
per_fcst_for_no_y <- format(per_fcst_pct, "%e %B")

## ----v02_format_ts_3_hist, include=FALSE--------------------------------------
# Select the time series over the observed period (no "future" dates)
ts_hist <- ts_plot[ts_plot$Date < dates_fcst[1], ]

# Display of the last time steps of the time series
tail(ts_hist)

## ----v02_format_ts_4_fcst, include=FALSE--------------------------------------
# Select the time series after the observed period (only "future" dates)
ts_fcst <- ts_plot[ts_plot$Date >= dates_fcst[1] & ts_plot$Date <= dates_fcst[length(dates_fcst)], ]

# Display of the 1st time steps of the time series
head(ts_fcst)

## ----include=FALSE------------------------------------------------------------
# Information for the vignette
year_per <- as.integer(format(range(ts_hist$Date), format = "%Y"))
year_min <- year_per[1L]
year_max <- year_per[2L]
n_year   <- year_max - year_min + 1

## ----v02_fig_pattern, echo=FALSE, eval=TRUE, fig.show='hide'------------------
# Indices of the plotting period
ind_xlim <- c(nrow(ts_plot)-300, nrow(ts_plot))

# Plotting hyrdograph
plot(x = ts_plot$Date, y = ts_plot$Qmmd,
     xlab = "time [d]", ylab = "flow [mm/d]",
     main = paste("Year", year_max),
     type = "l", lwd = 1,
     log = "y",
     xlim = ts_plot$Date[ind_xlim])

## ----V02_fig_presentation, echo=FALSE, eval=TRUE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Indices of the plotting period
ind_xlim <- c(nrow(ts_plot)-300, nrow(ts_plot))

# Plotting hyrdograph
plot(x = ts_plot$Date, y = ts_plot$Qmmd,
     xlab = "time [d]", ylab = "flow [mm/d]",
     main = paste("Year", year_max),
     type = "l", lwd = 1,
     log = "y",
     xlim = ts_plot$Date[ind_xlim])

# Add a future shading period
rect(xleft  = per_fcst_pct[1], ybottom = 1e-6, 
     xright = per_fcst_pct[2], ytop    = 1e+6, 
     col = "lightgrey", border = NA)
box()

# Add quetion mark in the future period
text(x = mean(per_fcst_pct), 
     y = quantile(ts_plot$Qmmd, probs = 0.5, na.rm = TRUE), 
     labels = "?", cex = 8, font = 2, col = "orangered")

## ----v02_set_per, echo=TRUE---------------------------------------------------
# Calibration period
per_cal_hist <- c("2000-09-01", "2018-08-31")

# Forecasting period
per_fcst <- c("2018-09-01", "2018-10-20")

# Warm-up period to simulate on the historical period 
per_wup_hist <- c("1999-01-01", "2000-08-31")

# Warm-up period to simulate on the forcasting period
per_wup_fcst <- c(per_wup_hist[1], per_cal_hist[2])

# Forecasting dates
dates_fcst <- seq(from = as.POSIXct(per_fcst[1], tz = "UTC", format = "%Y-%m-%d"), 
                  to   = as.POSIXct(per_fcst[2], tz = "UTC", format = "%Y-%m-%d"), 
                  by   = "1 day")
head(dates_fcst)

# Formatting of forecast dates (Month-Day)
month_day_fcst <- format(dates_fcst, "%m-%d")
head(month_day_fcst)

## ----v02_format_ts_1_ini, echo=TRUE-------------------------------------------
# Catchment data loading
library(airGRdatasets)
data("B222001001", package = "airGRdatasets")

# Ctachment area
area <- B222001001$Meta$Area

# Observed daily time series
ts_init <- B222001001$TS

# Add date information to the time series
ts_init$Year     <- format(ts_init$Date, format = "%Y")
ts_init$MonthDay <- format(ts_init$Date, format = "%m-%d")

# Display of the 1st time steps of the time series
head(ts_init)

## ----v02_format_ts_2_gph, echo=TRUE-------------------------------------------
# Set values of the last winter to missing data
ts_plot <- ts_init
isd_wint <- ts_plot$Date >= as.POSIXct(per_fcst[1], tz = "UTC", format = "%Y-%m-%d")
ts_plot[isd_wint, c("Ptot", "Temp", "Evap", "Qls", "Qmmd")] <- NA

# Display of the last time steps of the time series
tail(ts_plot)

## ----v02_format_ts_3_hist, echo=TRUE------------------------------------------
# Select the time series over the observed period (no "future" dates)
ts_hist <- ts_plot[ts_plot$Date < dates_fcst[1], ]

# Display of the last time steps of the time series
tail(ts_hist)

## ----v02_format_ts_4_fcst, echo=TRUE------------------------------------------
# Select the time series after the observed period (only "future" dates)
ts_fcst <- ts_plot[ts_plot$Date >= dates_fcst[1] & ts_plot$Date <= dates_fcst[length(dates_fcst)], ]

# Display of the 1st time steps of the time series
head(ts_fcst)

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
# Calculation of the historical streamflow quantiles
ts_qclim_quant <- aggregate(Qmmd ~ MonthDay, 
                            data = ts_hist[ts_hist$MonthDay %in% month_day_fcst, ], 
                            FUN = function(x) {
                              quantile(x, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
                            })
ts_qclim_quant <- as.data.frame(ts_qclim_quant$Qmmd)
colnames(ts_qclim_quant) <- paste0("Q", gsub("\\D", "", colnames(ts_qclim_quant)))
rownames(ts_qclim_quant) <- month_day_fcst

# Display of the 1st calculated quantiles
head(ts_qclim_quant)

## ----v02_fig_qclim, echo=FALSE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Indices of the plotting period
ind_xlim <- c(nrow(ts_plot)-300, nrow(ts_plot))

# Plotting hyrdograph
plot(x = ts_plot$Date, y = ts_plot$Qmmd,
     xlab = "time [d]", ylab = "flow [mm/d]",
     main = paste("Year", year_max),
     type = "l", lwd = 1,
     log = "y",
     xlim = ts_plot$Date[ind_xlim])

polygon(x = c(dates_fcst, rev(dates_fcst)), 
        y = c(ts_qclim_quant$Q10, rev(ts_qclim_quant$Q90)), 
        col = "lightgrey", border = NA)
polygon(x = c(dates_fcst, rev(dates_fcst)), 
        y = c(ts_qclim_quant$Q25, rev(ts_qclim_quant$Q75)), 
        col = "darkgrey", border = NA)
lines(x = dates_fcst, y = ts_qclim_quant$Q50, lwd = 2, col = "grey50")

legend("topright", 
       legend = c("Qobs", "Qclim (10,25,50,75,90 %)"), 
       col = c("black", "grey50"), 
       pch = c(NA, 15), lwd = c(1, 2), pt.cex = 2, 
       lty = c(1, 1), pt.bg = c(NA, "lightgrey"),
       bg = "white")

## ----v02_step_prep_hist, echo=TRUE, eval=TRUE, warning=FALSE------------------
# Adding an epsilon to observed streamflows for criterion calculation
epsilon_qobs <- mean(ts_hist$Qmmd, na.rm = TRUE) / 100

# Data processing for GR6J
prep_hist <- PrepGR(DatesR     = ts_hist$Date,
                    Precip     = ts_hist$Ptot,
                    PotEvap    = ts_hist$Evap, 
                    Qobs       = ts_hist$Qmmd + epsilon_qobs,
                    HydroModel = "GR6J", 
                    CemaNeige  = FALSE)

## ----v02_fig_step_cal_hist, echo=TRUE, eval=TRUE, fig.width=3*3, fig.height=3*2, out.width='98%'----
# Calibration step
cal_hist <- CalGR(PrepGR  = prep_hist, 
                  CalCrit = "NSE",
                  WupPer  = per_wup_hist, 
                  CalPer  = per_cal_hist,
                  transfo = "log",
                  verbose = TRUE)

# Get parameter and criterion values
param_cal_hist <- GetParam(cal_hist)
crit_cal_hist <- GetCrit(cal_hist)

# Graphical assessment of the calibration performance
plot(cal_hist)

## ----v02_format_cal_hist, echo=TRUE, eval=TRUE--------------------------------
# Combination of observed and simulated streamflow time series on the calibration period
ts_cal_hist <- as.data.frame(cal_hist)
head(ts_cal_hist)

## ----v02_fig_cal_hist, echo=FALSE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Plot framework
# Indices of the plotting period
ind_xlim <- c(nrow(ts_plot)-300, nrow(ts_plot))

# Plotting hyrdograph
plot(x = ts_plot$Date, y = ts_plot$Qmmd,
     xlab = "time [d]", ylab = "flow [mm/d]",
     main = paste("Year", year_max),
     type = "l", lwd = 1,
     log = "y",
     xlim = ts_plot$Date[ind_xlim])

# sim
lines(x = ts_cal_hist$Date, y = ts_cal_hist$Qsim, col = "orangered")

legend("topright", 
       legend = c("Qobs", "Qsim"), 
       lty = 1, col = c("black", "orangered"), 
       bg = "white")

## ----v02_format_p0, echo=TRUE, eval=TRUE--------------------------------------
# Duplicate the time series with future dates to fill the forecast period
ts_fcst_p0 <- ts_fcst

# Setting zero precipitation for the forecast period
ts_fcst_p0$Ptot <- 0

# Addition of interannual average PE
ts_eclim <- aggregate(Evap ~ MonthDay, 
                      data = ts_hist[ts_hist$MonthDay %in% month_day_fcst, ], 
                      FUN = mean)
ts_fcst_p0$Evap <- ts_eclim$Evap

# Combine historical and forecasting time series
ts_scen_p0 <- rbind(ts_hist, ts_fcst_p0)

# Display of the last lines
tail(ts_scen_p0)

## ----v02_step_sim_p0, echo=TRUE, eval=TRUE------------------------------------
# Data processing for GR6J
prep_scen_p0 <- PrepGR(DatesR     = ts_scen_p0$Date,
                       Precip     = ts_scen_p0$Ptot,
                       PotEvap    = ts_scen_p0$Evap, 
                       Qobs       = NULL,
                       HydroModel = "GR6J", 
                       CemaNeige  = FALSE)

# Hydrological forecast
sim_scen_p0 <- SimGR(PrepGR  = prep_scen_p0, 
                     WupPer  = per_wup_fcst, 
                     SimPer  = per_fcst,
                     Param   = param_cal_hist,
                     verbose = FALSE)

# Simulated streamflow time series
ts_sim_scen_p0 <- as.data.frame(sim_scen_p0)

## ----v02_fig_qscen_p0, echo=FALSE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Plot framework
# Plot framework
# Indices of the plotting period
ind_xlim <- c(nrow(ts_plot)-300, nrow(ts_plot))

# Plotting hyrdograph
plot(x = ts_plot$Date, y = ts_plot$Qmmd,
     xlab = "time [d]", ylab = "flow [mm/d]",
     main = paste("Year", year_max),
     type = "l", lwd = 1,
     log = "y",
     xlim = ts_plot$Date[ind_xlim])

# sim
lines(x = ts_cal_hist$Date, y = ts_cal_hist$Qsim, col = "orangered")

legend("topright", 
       legend = c("Qobs", "Qsim"), 
       lty = 1, col = c("black", "orangered"), 
       bg = "white")

# Zero precipitation
lines(x = ts_sim_scen_p0$Dates, y = ts_sim_scen_p0$Qsim, lwd = 2, col = "blue")

# Legend
legend("topright", 
       legend = c("Qobs", "Qsim", "QscenP0"), 
       lwd = 2, lty = 1, col = c("black", "orangered", "blue"), 
       bg = "white")

## ----v02_corr_q, echo=TRUE, eval=TRUE-----------------------------------------
# Correction (~ assimilation)
corr_qsim <- ts_sim_scen_p0$Qsim[1] / ts_hist$Qmmd[nrow(ts_hist)]

# Ratio display
corr_qsim

## ----v02_fig_qscen_p0corr, echo=FALSE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Plot framework
# Indices of the plotting period
ind_xlim <- c(nrow(ts_plot)-300, nrow(ts_plot))

# Plotting hyrdograph
plot(x = ts_plot$Date, y = ts_plot$Qmmd,
     xlab = "time [d]", ylab = "flow [mm/d]",
     main = paste("Year", year_max),
     type = "l", lwd = 1,
     log = "y",
     xlim = ts_plot$Date[ind_xlim])

# Zero precipitation
lines(x = ts_sim_scen_p0$Date, y = ts_sim_scen_p0$Qsim / corr_qsim, lwd = 2, col = "cornflowerblue")

# Legend
legend("topright", 
       legend = c("Qobs", "QscenP0corr"), 
       lwd = 2, lty = 1, col = c("black", "cornflowerblue"), 
       bg = "white")

## ----v02_scen_py, echo=TRUE, eval=TRUE, warning=FALSE-------------------------
# Historical years
year_hist <- unique(ts_hist$Year)
year_hist <- setdiff(year_hist, unique(ts_fcst$Year))
year_hist

# Loop on the historical years
ts_qscen_year <- sapply(year_hist, FUN = function(i_year) {
  
  i_ts_hist <- ts_hist[ts_hist$Year == i_year & ts_hist$MonthDay %in% month_day_fcst, ]
  i_ts_hist$Date <- dates_fcst
  
  # Combine historical and forecasting (based on precipitation climatology) time series
  i_ts_scen_py <- rbind(ts_hist, i_ts_hist)
  
  # Data processing for GR6J
  prep_scen_py <- PrepGR(DatesR     = i_ts_scen_py$Date,
                         Precip     = i_ts_scen_py$Ptot, 
                         PotEvap    = i_ts_scen_py$Evap, 
                         HydroModel = "GR6J", 
                         CemaNeige  = FALSE)
  
  # Hydrological forecast
  sim_scen_py <- SimGR(PrepGR  = prep_scen_py, 
                       WupPer  = per_wup_fcst, 
                       SimPer  = per_fcst, 
                       Param   = param_cal_hist, 
                       verbose = FALSE)
  
  # Correction of the simulated streamflow
  sim_scen_py$OutputsModel$Qsim / corr_qsim
  
})

# Compute future streamflow quantiles (based on precipitation climatology)
ts_qscen_quant <- t(apply(ts_qscen_year, MARGIN = 1, FUN = function(x) {
  quantile(x, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
}))
ts_qscen_quant <- as.data.frame(ts_qscen_quant)
colnames(ts_qscen_quant) <- paste0("Q", gsub("\\D", "", colnames(ts_qscen_quant)))
rownames(ts_qscen_quant) <- month_day_fcst

# Display of the first calculated quantiles
head(ts_qscen_quant)

## ----v02_fig_qscen_py, echo=FALSE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Plot framework
# Indices of the plotting period
ind_xlim <- c(nrow(ts_plot)-300, nrow(ts_plot))

# Plotting hyrdograph
plot(x = ts_plot$Date, y = ts_plot$Qmmd,
     xlab = "time [d]", ylab = "flow [mm/d]",
     main = paste("Year", year_max),
     type = "l", lwd = 1,
     log = "y",
     xlim = ts_plot$Date[ind_xlim])

polygon(x = c(dates_fcst, rev(dates_fcst)), 
        y = c(ts_qscen_quant$Q10, rev(ts_qscen_quant$Q90)), 
        col = adjustcolor("green2", alpha.f = 0.3), border = NA)
polygon(x = c(dates_fcst, rev(dates_fcst)), 
        y = c(ts_qscen_quant$Q25, rev(ts_qscen_quant$Q75)), 
        col = adjustcolor("green2", alpha.f = 0.5), border = NA)
lines(x = dates_fcst, y = ts_qscen_quant$Q50, lwd = 2, col = "green4")

legend("topright", 
       legend = c("Qobs", "QscenPy (10,25,50,75,90 %)"), 
       col = c("black", "green2"), 
       pch = c(NA, 15), lwd = c(1, 2), pt.cex = 2, 
       lty = c(1, 1), pt.bg = c(NA, "green2"),
       bg = "white")

## ----v02_fig_summary, echo=FALSE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Plot framework
# Indices of the plotting period
ind_xlim <- c(nrow(ts_plot)-300, nrow(ts_plot))

# Plotting hyrdograph
plot(x = ts_plot$Date, y = ts_plot$Qmmd,
     xlab = "time [d]", ylab = "flow [mm/d]",
     main = paste("Year", year_max),
     type = "l", lwd = 1,
     log = "y",
     xlim = ts_plot$Date[ind_xlim])

polygon(x = c(dates_fcst, rev(dates_fcst)), 
        y = c(ts_qclim_quant$Q10, rev(ts_qclim_quant$Q90)), 
        col = "lightgrey", border = NA)
polygon(x = c(dates_fcst, rev(dates_fcst)), 
        y = c(ts_qclim_quant$Q25, rev(ts_qclim_quant$Q75)), 
        col = "darkgrey", border = NA)
lines(x = dates_fcst, y = ts_qclim_quant$Q50, lwd = 2, col = "grey50")

legend("topright", 
       legend = c("Qobs", "Qclim (10,25,50,75,90 %)"), 
       col = c("black", "grey50"), 
       pch = c(NA, 15), lwd = c(1, 2), pt.cex = 2, 
       lty = c(1, 1), pt.bg = c(NA, "lightgrey"),
       bg = "white")
par(new = TRUE)
# Plot framework
# Indices of the plotting period
ind_xlim <- c(nrow(ts_plot)-300, nrow(ts_plot))

# Plotting hyrdograph
plot(x = ts_plot$Date, y = ts_plot$Qmmd,
     xlab = "time [d]", ylab = "flow [mm/d]",
     main = paste("Year", year_max),
     type = "l", lwd = 1,
     log = "y",
     xlim = ts_plot$Date[ind_xlim])

# sim
lines(x = ts_cal_hist$Date, y = ts_cal_hist$Qsim, col = "orangered")

legend("topright", 
       legend = c("Qobs", "Qsim"), 
       lty = 1, col = c("black", "orangered"), 
       bg = "white")
par(new = TRUE)
# Plot framework
# Indices of the plotting period
ind_xlim <- c(nrow(ts_plot)-300, nrow(ts_plot))

# Plotting hyrdograph
plot(x = ts_plot$Date, y = ts_plot$Qmmd,
     xlab = "time [d]", ylab = "flow [mm/d]",
     main = paste("Year", year_max),
     type = "l", lwd = 1,
     log = "y",
     xlim = ts_plot$Date[ind_xlim])

polygon(x = c(dates_fcst, rev(dates_fcst)), 
        y = c(ts_qscen_quant$Q10, rev(ts_qscen_quant$Q90)), 
        col = adjustcolor("green2", alpha.f = 0.3), border = NA)
polygon(x = c(dates_fcst, rev(dates_fcst)), 
        y = c(ts_qscen_quant$Q25, rev(ts_qscen_quant$Q75)), 
        col = adjustcolor("green2", alpha.f = 0.5), border = NA)
lines(x = dates_fcst, y = ts_qscen_quant$Q50, lwd = 2, col = "green4")

legend("topright", 
       legend = c("Qobs", "QscenPy (10,25,50,75,90 %)"), 
       col = c("black", "green2"), 
       pch = c(NA, 15), lwd = c(1, 2), pt.cex = 2, 
       lty = c(1, 1), pt.bg = c(NA, "green2"),
       bg = "white")
par(new = TRUE)
# Plot framework
# Indices of the plotting period
ind_xlim <- c(nrow(ts_plot)-300, nrow(ts_plot))

# Plotting hyrdograph
plot(x = ts_plot$Date, y = ts_plot$Qmmd,
     xlab = "time [d]", ylab = "flow [mm/d]",
     main = paste("Year", year_max),
     type = "l", lwd = 1,
     log = "y",
     xlim = ts_plot$Date[ind_xlim])

# Zero precipitation
lines(x = ts_sim_scen_p0$Date, y = ts_sim_scen_p0$Qsim / corr_qsim, lwd = 2, col = "cornflowerblue")

# Legend
legend("topright", 
       legend = c("Qobs", "QscenP0corr"), 
       lwd = 2, lty = 1, col = c("black", "cornflowerblue"), 
       bg = "white")

legend("topright", 
       legend = c("Qobs", 
                  "Qsim", 
                  "QscenP0corr", 
                  "Qclim (10,25,50,75,90 %)", 
                  "QscenPy (10,25,50,75,90 %)"), 
       col = c("black", "orangered", "cornflowerblue", "grey50", "green2"), 
       pch = c(NA, NA, NA, 15, 15), 
       lwd = c(1, 1, 2, 2, 2), 
       pt.cex = 2, 
       lty = c(1, 1, 1, 1, 1), 
       pt.bg = c(NA, NA, NA, "grey50", "green2"), 
       bg = "white")

