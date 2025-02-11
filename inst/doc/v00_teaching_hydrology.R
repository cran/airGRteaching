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
airGRdatasets <- sprintf(formatGR, "airGRdatasets")

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

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
# Package loading
library(airGRdatasets)

# Catchment data loading
data("B222001001", package = "airGRdatasets")

# Structure of the catchment data object
str(B222001001)

## ----echo=TRUE, eval=TRUE-----------------------------------------------------
# Package loading
library(airGRdatasets)

# Catchment data loading
data("B222001001", package = "airGRdatasets")

# Observed daily time series
ts_obs <- B222001001$TS

# Data processing for GR4J (without Q)
prep_no_q <- PrepGR(DatesR     = ts_obs$Date,
                    Precip     = ts_obs$Ptot,
                    PotEvap    = ts_obs$Evap,
                    HydroModel = "GR4J", 
                    CemaNeige  = FALSE)

# Simulation period
per_sim <- range(prep_no_q$InputsModel$DatesR)

## ----v00_fig_gr4j_x2, echo=TRUE, eval=TRUE, warning=FALSE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Different X2 values around its median values (0 [mm/d])
param_x2 <- seq(from = -2, to = 2, by = 1)

# Combination of parameter values (X1, X3 and X4 are fixed; X2 changes)
param_gr4j <- expand.grid(X1 = 350, 
                          X2 = param_x2,
                          X3 = 90,
                          X4 = 1.4)

# Streamflow simulations using parameter sets
sim_x2 <- apply(param_gr4j, MARGIN = 1, FUN = function(i_param_gr4j) {
  i_sim <- SimGR(PrepGR  = prep_no_q, 
                 Param   = i_param_gr4j, 
                 SimPer  = per_sim, 
                 verbose = FALSE)
  i_sim$OutputsModel$Qsim
})

# Graphical comparison
ind_zoom <- 400:430
col_param_x2 <- colorRampPalette(c("green1", "green4"))(ncol(sim_x2))
matplot(x = as.POSIXct(prep_no_q$InputsModel$DatesR[ind_zoom]),
        y = sim_x2[ind_zoom, ],
        xlab = "time [d]", ylab = "flow [mm/d]",
        type = "l", lty = 1, lwd = 2, col = col_param_x2)
legend("topright",
       legend = sprintf("% .1f", param_x2),
       lwd = 2, col = col_param_x2,
       title = "X2 values [mm/d]",
       bg = "white")

## ----v00_fig_gr4j_x4, echo=TRUE, eval=TRUE, warning=FALSE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Different X4 values around its median values (1.4 [d])
param_x4 <- seq(from = 1.0, to = 3.0, by = 0.5)

# Combination of parameter values (X1, X2 and X3 are fixed; X4 changes)
param_gr4j <- expand.grid(X1 = 350, 
                          X2 = 0,
                          X3 = 90,
                          X4 = param_x4)

# Streamflow simulations using parameter sets
sim_x4 <- apply(param_gr4j, MARGIN = 1, FUN = function(i_param_gr4j) {
  i_sim <- SimGR(PrepGR = prep_no_q, 
                 Param = i_param_gr4j, 
                 SimPer = per_sim, 
                 verbose = FALSE)
  i_sim$OutputsModel$Qsim
})

# Graphical comparison
ind_zoom <- 400:430
col_param_x4 <- colorRampPalette(c("steelblue1", "steelblue4"))(ncol(sim_x4))
matplot(x = as.POSIXct(prep_no_q$InputsModel$DatesR[ind_zoom]),
        y = sim_x4[ind_zoom, ],
        xlab = "time [d]", ylab = "flow [mm/d]",
        type = "l", lty = 1, lwd = 2, col = col_param_x4)
legend("topright",
       legend = sprintf("% .1f", param_x4),
       lwd = 2,col = col_param_x4, 
       title = "X4 values [d]",
       bg = "white")

## ----v00_fig_sim_aggreg, echo=TRUE, eval=TRUE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Aggregation of the simulated streamflow at the yearly time step
sim_x2_y <- cbind(DatesR = as.POSIXct(prep_no_q$InputsModel$DatesR), 
                  as.data.frame(sim_x2))
sim_x2_y <- SeriesAggreg(x = sim_x2_y, 
                         Format = "%Y",
                         ConvertFun = rep("sum", ncol(sim_x2_y) - 1))
sim_x4_y <- cbind(DatesR = as.POSIXct(prep_no_q$InputsModel$DatesR), 
                  as.data.frame(sim_x4))
sim_x4_y <- SeriesAggreg(x = sim_x4_y, 
                         Format = "%Y",
                         ConvertFun = rep("sum", ncol(sim_x4_y) - 1))

# Graphical comparison
matplot(x = sim_x2_y$DatesR, y = sim_x2_y[, -1], 
        type = "l", lty = 1, lwd = 2, col = col_param_x2, 
        xlab = "time [yr]", ylab = "flow [mm/yr]")
matlines(x = sim_x4_y$DatesR, y = sim_x4_y[, -1], 
         type = "l", lty = 1, lwd = 2, col = col_param_x4)
legend("topright", 
       legend = c("X2", "X4"), 
       lwd = 2, col = c(median(col_param_x2), median(col_param_x4)),
       bg = "white")

## ----v00_fig_wup, echo=TRUE, eval=TRUE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Warm-up and simulation periods
per_wup1m <- c("2002-12-01", "2002-12-31")
per_wup1y <- c("2002-01-01", "2002-12-31")
per_sim   <- c("2003-01-01", "2006-12-31")

# Parameter set
param_gr4j <- c(X1 = 350, X32 = 0, X3 = 90, X4 = 1.4)

# Simulation without warm-up period
sim_wup0d <- SimGR(PrepGR = prep_no_q, 
                   Param  = param_gr4j,
                   WupPer = 0L,
                   SimPer = per_sim)

# Simulation with a 1-month warm-up period
sim_wup1m <- SimGR(PrepGR = prep_no_q,
                   Param  = param_gr4j,
                   WupPer = per_wup1m, 
                   SimPer = per_sim)

# Simulation with a 4-year warm-up period
sim_wup1y <- SimGR(PrepGR = prep_no_q, 
                   Param  = param_gr4j,
                   WupPer = per_wup1y, 
                   SimPer = per_sim)

# Graphical comparison
col_wup <- c("orchid", "orange2", "green3")
matplot(x = as.POSIXct(sim_wup0d$OutputsModel$DatesR),
        y = cbind(sim_wup0d$OutputsModel$Qsim,
                  sim_wup1m$OutputsModel$Qsim,
                  sim_wup1y$OutputsModel$Qsim),
        xlab = "time [d]", ylab = "flow [mm/d]", 
        type = "l", lty = 1, lwd = 2, col = col_wup,
        xlim = as.POSIXct(x = c("2003-01-01", "2003-09-01"), tz = "UTC"))
legend("topright", 
       legend = c("no warm-up", "1-month warm-up", "1-year warm-up"), 
       col = col_wup, lwd = 2,
       bg = "white")

## ----v00_fig_cal_manu, echo=TRUE, eval=TRUE, warning=FALSE, fig.width=7*1.5, fig.height=5*1.5, out.width='98%'----
# Data processing for GR4J (with Q for calibration)
prep <- PrepGR(DatesR     = ts_obs$Date,
               Precip     = ts_obs$Ptot,
               PotEvap    = ts_obs$Evap, 
               Qobs       = ts_obs$Qmmd,
               HydroModel = "GR4J", 
               CemaNeige  = FALSE)

# Parameter set to test
i_param_gr4j <- c(X1 = 350, X2 = 0, X3 = 90, X4 = 1.4)

# Rainfall-runoff simulation on the calibration period
i_sim_manu <- SimGR(PrepGR  = prep, 
                    Param   = param_gr4j,
                    WupPer  = c("1999-01-01", "2000-12-31"),
                    SimPer  = c("2001-01-01", "2010-12-31"),
                    EffCrit = "NSE",
                    verbose = TRUE)

# Get the criterion value
GetCrit(i_sim_manu)

# Graphical assessment of the calibration performance
plot(i_sim_manu)

## ----v00_fig_cal_auto, echo=TRUE, eval=TRUE, fig.width=7*1.5, fig.height=5*1.5, out.width='98%'----
# Calibration using NSE score 
cal_auto <- CalGR(PrepGR  = prep, 
                  CalCrit = "NSE",
                  WupPer  = c("1999-01-01", "2000-12-31"), 
                  CalPer  = c("2001-01-01", "2010-12-31"))

# Get parameter and criteria values at the end of the calibration step
GetParam(cal_auto)
GetCrit(cal_auto)

# Graphical assessment of the calibration performance
plot(cal_auto)

## ----v00_fig_cal_eval, echo=TRUE, eval=TRUE, warning=FALSE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Catchment data loading
data("X031001001", package = "airGRdatasets")

# Observed daily time series
ts_obs <- X031001001$TS

# Catchment elevation distribution
hypso <- X031001001$Hypso

# Temporal subset
is_per <- ts_obs$Date >= as.POSIXct("1999-01-01", tz = "UTC") & 
  ts_obs$Date <= as.POSIXct("2009-12-30", tz = "UTC")
ts_obs <- ts_obs[is_per, ]

# Data processing for GR4J (without snow module)
prep_snow_n <- PrepGR(DatesR     = ts_obs$Date,
                      Precip     = ts_obs$Ptot,
                      PotEvap    = ts_obs$Evap, 
                      Qobs       = ts_obs$Qmmd,
                      HydroModel = "GR4J", 
                      CemaNeige  = FALSE)

# Data processing for GR4J with snow module
prep_snow_y <- PrepGR(DatesR     = ts_obs$Date,
                      Precip     = ts_obs$Ptot,
                      PotEvap    = ts_obs$Evap, 
                      Qobs       = ts_obs$Qmmd,
                      TempMean   = ts_obs$Temp,
                      ZInputs    = median(hypso),
                      HypsoData  = hypso,
                      NLayers    = 5,
                      HydroModel = "GR4J", 
                      CemaNeige  = TRUE)

# Calibration using NSE score (without snow module)
cal_snow_n <- CalGR(PrepGR  = prep_snow_n, 
                    CalCrit = "NSE",
                    WupPer  = c("1999-01-01", "2000-12-31"), 
                    CalPer  = c("2001-01-01", "2009-12-30"),
                    verbose = TRUE)

# Calibration using NSE score (with snow module)
cal_snow_y <- CalGR(PrepGR  = prep_snow_y, 
                    CalCrit = "NSE",
                    WupPer  = c("1999-01-01", "2000-12-31"), 
                    CalPer  = c("2001-01-01", "2009-12-30"),
                    verbose = TRUE)

# Combination of observed and simulated streamflow
tab_cal <- data.frame(Date        = cal_snow_n$OutputsModel$DatesR,
                      QOobs       = cal_snow_n$Qobs,
                      Qsim_snow_n = cal_snow_n$OutputsModel$Qsim,
                      Qsim_snow_y = cal_snow_y$OutputsModel$Qsim)

# Computation of regime streamflow
tab_cal_reg <- SeriesAggreg(tab_cal, 
                            Format = "%m", 
                            ConvertFun = rep("mean", ncol(tab_cal) - 1))

# Graphical comparison between simulated and observed streamflow regimes
col_snow <- c("black", rep("orangered", 2))
lty_snow <- c(1, 1:2)
matplot(y = tab_cal_reg[, grep("^Q", colnames(tab_cal))],
        xlab = "time [months]", ylab = "flow [mm/d]", 
        type = "l", lty = lty_snow, lwd = 2, col = col_snow)
legend("topright", 
       legend = c("Qobs", "Qsim without snow mod.", "Qsim with snow mod."),
       lty = lty_snow, lwd = 2, col = col_snow,
       bg = "white")

## ----v00_fig_flow_transfo, echo=TRUE, eval=TRUE, warning=FALSE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Catchment data loading
data("B222001001", package = "airGRdatasets")
ts_obs <- B222001001$TS

# Data processing for GR4J (with Q for calibration)
prep <- PrepGR(DatesR     = ts_obs$Date,
               Precip     = ts_obs$Ptot,
               PotEvap    = ts_obs$Evap, 
               Qobs       = ts_obs$Qmmd,
               HydroModel = "GR4J", 
               CemaNeige  = FALSE)

# Calibration using NSE score on raw Q
cal_raw <- CalGR(PrepGR  = prep, 
                 CalCrit = "NSE",
                 transfo = "",
                 WupPer  = c("1999-01-01", "2001-12-31"), 
                 CalPer  = c("2002-01-01", "2016-12-31"))

# Calibration using NSE score on sqrt(Q)
cal_sqrt <- CalGR(PrepGR  = prep, 
                  CalCrit = "NSE",
                  transfo = "sqrt",
                  WupPer  = c("1999-01-01", "2001-12-31"),
                  CalPer  = c("2002-01-01", "2016-12-31"))

# Calibration using NSE score on log(Q)
cal_log <- CalGR(PrepGR  = prep, 
                 CalCrit = "NSE",
                 transfo = "log",
                 WupPer  = c("1999-01-01", "2001-12-31"), 
                 CalPer  = c("2002-01-01", "2016-12-31"))

# Combination of simulated streamflow
tab_sim_trsf <- data.frame(Date       = cal_raw$OutputsModel$DatesR,
                           QSIM_rawQ  = cal_raw$OutputsModel$Qsim,
                           QSIM_sqrtQ = cal_sqrt$OutputsModel$Qsim,
                           QSIM_logQ  = cal_log$OutputsModel$Qsim)
tab_sim_trsf <- merge(x = ts_obs[, c("Date", "Qmmd")], 
                      y = tab_sim_trsf, 
                      by = "Date", 
                      all.y = TRUE)

# Computation of regime streamflow
tab_sim_reg <- SeriesAggreg(tab_sim_trsf, 
                            Format = "%m", 
                            ConvertFun = rep("mean", ncol(tab_sim_trsf) - 1))

# Graphical comparison between simulated and observed streamflow regimes
col_trsf <- c("black", rep("orangered", 3))
lty_trsf <- c(1, 1:3)
matplot(y = tab_sim_reg[, -1],
        xlab = "time [months]", ylab = "flow [mm/d]", 
        type = "l", lty = lty_trsf, lwd = 2, col = col_trsf)
legend("bottomleft", 
       legend = c("Qobs", "Qsim", "sqrt(Qsim)", "log(Qsim)"),
       lty = lty_trsf, lwd = 2, col = col_trsf,
       bg = "white")

## ----v00_fig_obj_fun, echo=TRUE, eval=TRUE, warning=FALSE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Calibration using NSE score on Q
cal_nse <- CalGR(PrepGR  = prep, 
                 CalCrit = "NSE",
                 transfo = "",
                 WupPer  = c("1999-01-01", "2001-12-31"), 
                 CalPer  = c("2002-01-01", "2016-12-31"))

# Calibration using KGE score on Q
cal_kge <- CalGR(PrepGR  = prep, 
                 CalCrit = "KGE",
                 transfo = "",
                 WupPer  = c("1999-01-01", "2001-12-31"), 
                 CalPer  = c("2002-01-01", "2016-12-31"))

# Combination of observed and simulated streamflow
tab_crit <- data.frame(Date     = as.POSIXct(cal_nse$OutputsModel$DatesR),
                       Qobs     = cal_nse$Qobs,
                       Qsim_nse = cal_nse$OutputsModel$Qsim,
                       Qsim_kge = cal_kge$OutputsModel$Qsim)

# Graphical comparison
col_crit <- c("black", rep("orangered", 2))
lty_crit <- c(1, 1:2)
matplot(x = tab_crit$Date, y = tab_crit[, -1],
        xlab = "time [d]", ylab = "flow [mm/d]", 
        type = "l", lty = lty_crit, lwd = 2, col = col_crit,
        xlim = as.POSIXct(x = c("2004-01-01", "2004-03-01"), tz = "UTC"))
legend("topleft", 
       legend = c("Qobs", "Qsim NSE", "Qsim KGE"), 
       lty = lty_crit, lwd = 2, col = col_crit,
       bg = "white")

## ----v00_fig_sst, echo=TRUE, eval=TRUE, warning=FALSE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Calibration and evaluation sub-periods
per1_wup <- c("1999-01-01", "2001-12-31")
per1_sim <- c("2002-01-01", "2008-12-31")
per2_wup <- c("2009-01-01", "2011-12-31")
per2_sim <- c("2012-01-01", "2018-12-31")

# Calibration on per1 and per2
cal_per1 <- CalGR(PrepGR  = prep,
                  CalCrit = "KGE",
                  transfo = "",
                  WupPer  = per1_wup,
                  CalPer  = per1_sim,
                  verbose = TRUE)
cal_per2 <- CalGR(PrepGR  = prep,
                  CalCrit = "KGE",
                  transfo = "",
                  WupPer  = per2_wup,
                  CalPer  = per2_sim,
                  verbose = TRUE)

# Get parameter values at the end of the calibration step
param_per1 <- GetParam(cal_per1)
param_per2 <- GetParam(cal_per2)

# Get criteria values at the end of the calibration step
crit_cal_per1 <- GetCrit(cal_per1)
crit_cal_per2 <- GetCrit(cal_per2)

# Evaluation over per1 and per2
eva_per1 <- SimGR(PrepGR  = prep, 
                  Param   = param_per2,
                  WupPer  = per1_wup,
                  SimPer  = per1_sim,
                  EffCrit = "KGE",
                  verbose = TRUE)

eva_per2 <- SimGR(PrepGR  = prep, 
                  Param   = param_per1,
                  WupPer  = per2_wup,
                  SimPer  = per2_sim,
                  EffCrit = "KGE",
                  verbose = TRUE)

# Get criteria values
crit_eva_per1 <- GetCrit(eva_per1)
crit_eva_per2 <- GetCrit(eva_per2)

# Cleveland dot plot of the criteria
dotchart(c(crit_eva_per1, crit_cal_per1, crit_eva_per2, crit_cal_per2),
         labels = c("eva (per1)", "cal (per1)", "eva (per2)", "cal (per2)"),
         groups = rep(1:2, each = 2),
         col = rep(c("darkred", "darkblue"), each = 2), pch = 19, 
         xlab = "KGE [-]")

## ----v00_fig_dsst, echo=TRUE, eval=TRUE, warning=FALSE, fig.width=3*3, fig.height=3*1.7, out.width='98%'----
# Estimation of annual aridity index (PE/P)
ts_obs_y <- SeriesAggreg(x = ts_obs[, c("Date", "Ptot", "Evap")], 
                         Format = "%Y", 
                         ConvertFun = c("sum", "sum"), 
                         YearFirstMonth = 10)
ts_obs_y$Arid <- ts_obs_y$Evap / ts_obs_y$Ptot

# Identification of wetter and dryer hydrological years
barplot(height = ts_obs_y$Arid, 
        names.arg = format(ts_obs_y$Date, format = "%Y"), 
        xlab = "time [yr]", ylab = "aridity index [-]",
        col = "royalblue")

# Wet and dry periods
per_wet <- c("2016-10-01", "2017-09-30")
per_dry <- c("2000-10-01", "2001-09-30")

# Calibration over the wet and the dry periods
cal_wet <- CalGR(PrepGR  = prep,
                 CalCrit = "KGE",
                 CalPer  = per_wet,
                 verbose = TRUE)
cal_dry <- CalGR(PrepGR  = prep,
                 CalCrit = "KGE",
                 CalPer  = per_dry,
                 verbose = TRUE)

# Get parameter values at the end of the calibration step
param_dry <- GetParam(cal_dry)
param_wet <- GetParam(cal_wet)

# Get criteria values at the end of the calibration step
crit_cal_dry <- GetCrit(cal_dry)
crit_cal_wet <- GetCrit(cal_wet)

# Evaluation over the wet and the dry periods
eva_wet <- SimGR(PrepGR  = prep, 
                 Param   = cal_dry,
                 SimPer  = per_wet,
                 EffCrit = "KGE",
                 verbose = TRUE)
eva_dry <- SimGR(PrepGR  = prep, 
                 Param   = cal_wet,
                 SimPer  = per_dry,
                 EffCrit = "KGE",
                 verbose = TRUE)

# Get criteria values
crit_eva_dry <- GetCrit(eva_dry)
crit_eva_wet <- GetCrit(eva_wet)

# Cleveland dot plot of the criteria
dotchart(c(crit_eva_dry, crit_cal_dry, crit_eva_wet, crit_cal_wet),
         labels =  c("eva (dry)", "cal (dry)", "eva (wet)", "cal (wet)"),
         col = rep(c("darkorange", "deepskyblue3"), each = 2), pch = 19,
         xlab = "KGE [-]")

