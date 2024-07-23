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
Sys.setlocale("LC_TIME", "fr_FR.UTF-8")
colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color, x)
  } else {
    x
  }
}

## ----v03_format_delta_temp, echo=FALSE, results="asis", warning=FALSE---------
# Delta of temperature (assigned on the 15th of each month)
delta_temp <- data.frame(Month = sprintf("%02i-15", 1:12),
                         Tscen1 = rep(1.5, 12),
                         Tscen2 = c(2.5, 2.5, 3.0, 3.0, 3.5, 4.0, 4.0, 3.5, 3.0, 3.0, 2.5, 2.5),
                         Tscen3 = c(3.5, 3.5, 4.0, 4.5, 5.0, 6.0, 6.0, 5.0, 4.5, 4.0, 3.5, 3.5))

## ----v03_format_delta_precip, echo=FALSE, results="asis", warning=FALSE-------
# Delta of precipitation (assigned on the 15th of each month)
delta_ptot <- data.frame(Month = sprintf("%02i-15", 1:12),
                         Pscen1 = c(+10, +10, +05, 0, -05, -10, -10, -05, 0, +05, +10, +10),
                         Pscen2 = c(+15, +15, +07, 0, -07, -15, -15, -07, 0, +07, +25, +20),
                         Pscen3 = c(+20, +15, +10, 0, -15, -30, -30, -15, 0, +20, +40, +30))

## ----v03_format_ts_1_ini, echo=FALSE, results="asis", warning=FALSE-----------
# Catchment data loading
library(airGRdatasets)
data("X031001001", package = "airGRdatasets")

# Observed daily time series
ts_obs <- X031001001$TS

# Latitude of the catchment outlet
lat <- X031001001$Meta$Coor$Y

# Catchment elevation distribution
hypso <- X031001001$Hypso

## ----include=FALSE------------------------------------------------------------
name_sta <- gsub("the ", "", X031001001$Meta$Name)
name_riv <- gsub("(the )(.*)( at.*)", "\\2", name_sta)

## ----v03_set_per, echo=FALSE, eval=TRUE, include=FALSE------------------------
# Warm-up period
per_ini <- c("1999-01-01", "2000-08-31")

# Calibration period
per_cal <- c("2000-09-01", "2009-06-29")

## ----echo=FALSE, eval=TRUE----------------------------------------------------
per_ini_pct <- as.POSIXct(per_ini, tz = "UTC", format = "%Y-%m-%d")
per_ini_for <- format(per_ini_pct, "%e %B %Y")
per_cal_pct <- as.POSIXct(per_cal, tz = "UTC", format = "%Y-%m-%d")
per_cal_for <- format(per_cal_pct, "%e %B %Y")

## ----v03_tab_delta_temp, echo=FALSE, results="asis", warning=FALSE------------
# Table formatting
delta_temp2 <- delta_temp
delta_temp2 <- delta_temp2[, setdiff(colnames(delta_temp2), "Month")]
colnames(delta_temp2) <- c("Temp. scenario 1", "Temp. scenario 2", "Temp. scenario 3")
rownames(delta_temp2) <- month.abb
delta_temp2 <- t(delta_temp2)

# Table display
knitr::kable(delta_temp2, 
             caption = "Monthly mean air temperature scenarios (°C), calculated by comparing the present climate period with the future climate period.")

## ----v03_tab_delta_precip, echo=FALSE, results="asis", warning=FALSE----------
# Table formatting
delta_ptot2 <- delta_ptot
delta_ptot2 <- delta_ptot2[, setdiff(colnames(delta_ptot2), "Month")]
colnames(delta_ptot2) <- c("Precip. scenario 1", "Precip. scenario 2", "Precip. scenario 3")
rownames(delta_ptot2) <- month.abb
delta_ptot2 <- t(delta_ptot2)

# Table display
knitr::kable(delta_ptot2 , 
             caption = "Monthly total precipitation scenarios (%), calculated by comparing the present climate period with the future climate period.")

## ----v03_fig_presentation, echo=FALSE, eval=TRUE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Daily streamflow regimes
is_per_cal <- ts_obs$Date >= as.POSIXct(per_cal[1L], tz = "UTC") & ts_obs$Date <= as.POSIXct(per_cal[2L], tz = "UTC")
reg_d <- SeriesAggreg(x = ts_obs[is_per_cal, c("Date", "Qmmd")],
                      Format = "%d",
                      ConvertFun = c("mean"))
is_feb29 <- format(x = reg_d$Date, format = "%m-%d") == "02-29"
reg_d <- reg_d[!is_feb29, ]

# Plot layout
par(mfrow = c(1, 2))

# Present time regime
plot(x = reg_d$Date, y = reg_d$Qmmd, 
     xlab = "time [d]", ylab = "flow [mm/d]", 
     main = "Present time climate",
     type = "l")

# Future time regime
plot(x = reg_d$Date, y = reg_d$Qmmd, 
     xlab = "time [d]", ylab = "", 
     main = "Future climate",
     type = "n", yaxt = "n")
axis(side = 2, labels = FALSE)
text(x = median(reg_d$Date), y = mean(range(reg_d$Qmmd)), 
     labels = "?", cex = 10, font = 2, col = "orangered")

## ----eval=TRUE, include=FALSE-------------------------------------------------
k_pscen <- 1
k_month <- 1
name_k_month <- format(as.POSIXct(delta_ptot[k_month, "Month"], format = "%m-%d"), format = "%B")

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
# Catchment data loading
library(airGRdatasets)
data("X031001001", package = "airGRdatasets")

# Observed daily time series
ts_obs <- X031001001$TS

# Latitude of the catchment outlet
lat <- X031001001$Meta$Coor$Y

# Catchment elevation distribution
hypso <- X031001001$Hypso

# Warm-up period
per_ini <- c("1999-01-01", "2000-08-31")

# Calibration period
per_cal <- c("2000-09-01", "2009-06-29")

## ----v03_fig_hypso, echo=TRUE, eval=TRUE, fig.width=3*1.7, fig.height=3*1.7, out.width='66%', dev.args=list(pointsize=10)----
# Elevation distribution
plot(x = hypso,
     xlab = "Frequency (%)", ylab = "Catchment elevation [m]")

## ----v03_fig_cal, echo=TRUE, eval=TRUE, warning=FALSE, fig.width=3*3, fig.height=3*3, out.width='98%', dev.args=list(pointsize=14)----
# Data processing for GR4J
prep_hist <- PrepGR(DatesR     = ts_obs$Date,
                    Precip     = ts_obs$Ptot,
                    PotEvap    = ts_obs$Evap,
                    TempMean   = ts_obs$Temp,
                    ZInputs    = hypso[51],
                    HypsoData  = hypso,
                    Qobs       = ts_obs$Qmmd,
                    HydroModel = "GR4J", 
                    CemaNeige  = TRUE)

# Calibration step
cal_hist <- CalGR(PrepGR  = prep_hist, 
                  CalCrit = "NSE",
                  WupPer  = per_ini, 
                  CalPer  = per_cal,
                  verbose = TRUE)

# Get parameter and criterion values
param_cal_hist <- GetParam(cal_hist)
GetCrit(cal_hist)

# Graphical assessment
plot(cal_hist)

# Combination of observed and simulated streamflow time series 
ts_qhist <- as.data.frame(cal_hist)
ts_qhist <- ts_qhist[, c("Dates", "Qobs", "Qsim")]

## ----v03_regime, echo=TRUE, eval=TRUE, warning=FALSE--------------------------
# Daily regimes
reg_hist_d <- SeriesAggreg(ts_qhist,
                           Format = "%d",
                           ConvertFun = c("mean", "mean"))
is_feb29 <- format(x = reg_d$Date, format = "%m-%d") == "02-29"
reg_hist_d <- reg_d[!is_feb29, ]

# Monthly regimes
reg_hist_m <- SeriesAggreg(ts_qhist,
                           Format = "%m",
                           ConvertFun = c("sum", "sum"))

# Calculated regimes
reg_hist_m

## ----v03_fig_regime, echo=FALSE, eval=TRUE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Graphical parameters
plot(x = reg_hist_m$Dates, y = reg_hist_m$Qobs,
     xlab = "time [months]", ylab = "flow [mm/month]",
     type = "l")
lines(x = reg_hist_m$Dates, y = reg_hist_m$Qsim,
      type = "l", col = "orangered")

# Legend
legend("topright",
       legend = c("Qobs", "Qsim"),
       lty = 1, 
       col = c("black", "orangered"), 
       bg = "white")

## ----v03_generation_clim_cc, echo=TRUE, eval=TRUE, warning=FALSE--------------
# Delta of temperature (assigned on the 15th of each month)
delta_temp <- data.frame(Month = sprintf("%02i-15", 1:12),
                         Tscen1 = rep(1.5, 12),
                         Tscen2 = c(2.5, 2.5, 3.0, 3.0, 3.5, 4.0, 4.0, 3.5, 3.0, 3.0, 2.5, 2.5),
                         Tscen3 = c(3.5, 3.5, 4.0, 4.5, 5.0, 6.0, 6.0, 5.0, 4.5, 4.0, 3.5, 3.5))

# Delta of precipitation (assigned on the 15th of each month)
delta_ptot <- data.frame(Month = sprintf("%02i-15", 1:12),
                         Pscen1 = c(+10, +10, +05, 0, -05, -10, -10, -05, 0, +05, +10, +10),
                         Pscen2 = c(+15, +15, +07, 0, -07, -15, -15, -07, 0, +07, +25, +20),
                         Pscen3 = c(+20, +15, +10, 0, -15, -30, -30, -15, 0, +20, +40, +30))

# Time series with additional date information
ts_cc <- data.frame(Dates = ts_obs$Date)
ts_cc$Month <- format(x = ts_cc$Date, format = "%m-%d")

# Delta of temperature and precipitation in the same table
delta_cc <- merge(delta_temp, delta_ptot, by = "Month", all = TRUE)

# Merging the complete time series and the delta table
ts_cc <- merge(ts_cc, delta_cc, by = "Month", all = TRUE)
ts_cc <- ts_cc[order(ts_cc$Dates), ]

# Sub-setting of the time series with the dates available in the delta table
ts_cc_15 <- na.omit(ts_cc)

# Constitution of "future climate" time series
ts_clim_cc <- sapply(grep("scen", colnames(ts_cc_15), value = TRUE), FUN = function(i) {
  # Daily delta interpolation (between the 15th of each month)
  i_interpol <- approx(x = ts_cc_15$Dates, y = ts_cc_15[, i], 
                       xout = ts_cc$Dates, rule = 2)$y
  if (grepl("T", i)) {
    ts_obs$Temp + i_interpol
  } else {
    ts_obs$Ptot * (1 + i_interpol / 100)
  }
})
ts_clim_cc <- as.data.frame(ts_clim_cc)

# Summary of the "future climate" time series
summary(ts_clim_cc)

# PE calculation
ts_clim_cc$Julian <- as.numeric(x = format(x = ts_cc$Date, format = "%j"))
ts_clim_cc$Escen1 <- PE_Oudin(JD = ts_clim_cc$Julian, 
                              Temp = ts_clim_cc$Tscen1, 
                              Lat = lat, LatUnit = "deg")
ts_clim_cc$Escen2 <- PE_Oudin(JD = ts_clim_cc$Julian, 
                              Temp = ts_clim_cc$Tscen2, 
                              Lat = lat, LatUnit = "deg")
ts_clim_cc$Escen3 <- PE_Oudin(JD = ts_clim_cc$Julian, 
                              Temp = ts_clim_cc$Tscen3, 
                              Lat = lat, LatUnit = "deg")

## ----v03_sim_cc, echo=TRUE, eval=TRUE, warning=FALSE--------------------------
# Loop on the three scenarios
ts_qcc <- list()
for (i in 1:3) {
  i_col_P <- paste0("Pscen", i)
  i_col_E <- paste0("Escen", i)
  i_col_T <- paste0("Tscen", i)
  i_col_Q <- paste0("Qscen", i)
  
  # Data processing for GR4J
  i_prep_cc <- PrepGR(DatesR     = ts_cc$Date,
                      Precip     = ts_clim_cc[, i_col_P],
                      PotEvap    = ts_clim_cc[, i_col_E],
                      TempMean   = ts_clim_cc[, i_col_T],
                      ZInputs    = hypso[51],
                      HypsoData  = hypso,
                      HydroModel = "GR4J", 
                      CemaNeige  = TRUE)
  
  # Simulation step
  i_sim_cc <- SimGR(PrepGR  = i_prep_cc, 
                    WupPer  = per_ini, 
                    SimPer  = per_cal,
                    Param   = param_cal_hist,
                    verbose = FALSE)
  
  # Storage of observed and simulated streamflow series
  i_ts_cc_15 <- as.data.frame(i_sim_cc)
  ts_qcc[[i_col_Q]] <- i_ts_cc_15$Qsim
}

# Combine historical and future time series
ts_q <- cbind(ts_qhist, as.data.frame(ts_qcc))

## ----v03_regime_cc, echo=TRUE, eval=TRUE, warning=FALSE-----------------------
# Daily regimes
reg_cc_d <- SeriesAggreg(ts_q,
                         Format = "%d",
                         ConvertFun = rep("mean", 5))
is_feb29 <- format(x = reg_cc_d$Dates, format = "%m-%d") == "02-29"
reg_cc_d <- reg_cc_d[!is_feb29, ]

# Monthly regimes
reg_cc_m <- SeriesAggreg(ts_q,
                         Format = "%m",
                         ConvertFun = rep("sum", 5))

# Calculated regimes
reg_cc_m

## ----v03_fig_regime_month_cc, echo=FALSE, eval=TRUE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Plot framework
# Graphical parameters
plot(x = reg_hist_m$Dates, y = reg_hist_m$Qobs,
     xlab = "time [months]", ylab = "flow [mm/month]",
     type = "l")
lines(x = reg_hist_m$Dates, y = reg_hist_m$Qsim,
      type = "l", col = "orangered")

# Legend
legend("topright",
       legend = c("Qobs", "Qsim"),
       lty = 1, 
       col = c("black", "orangered"), 
       bg = "white")

# Simulations (future)
col_qscen <- colorRampPalette(c("steelblue1", "steelblue4"))(3)
matlines(x = reg_cc_m$Dates, y = reg_cc_m[, c("Qscen1", "Qscen2", "Qscen3")],
         lty = 1, col = col_qscen)

# Legend
legend("topright",
       legend = c("Qobs", "Qsim", "Qscen1", "Qscen2", "Qscen3"),
       lty = 1, 
       col = c("black", "orangered", col_qscen),
       bg = "white")

## ----v03_fig_regime_day_cc, echo=FALSE, eval=TRUE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Plot framework
matplot(x = reg_cc_d$Dates, y = reg_cc_d[, -1], 
        xlab = "time [d]", ylab = "flow [mm/d]", 
        type = "l", lty = 1, 
        col = c("black", "orangered", col_qscen))

# Legend
legend("topright",
       legend = c("Qobs", "Qsim", "Qscen1", "Qscen2", "Qscen3"),
       lty = 1, 
       col = c("black", "orangered", col_qscen),
       bg = "white")

