.onAttach <- function(libname, pkgname) {
  if (packageVersion("htmlwidgets") <= "1.5.2") {
    base::packageStartupMessage("\n---------------------------\n")
    base::packageStartupMessage("This version of 'airGRteaching' is designed to work with 'htmlwidgets' >= 1.5.2.9000 (troubles with 'dygraphs')")
    base::packageStartupMessage("Install the latest version of 'htmlwidgets' from GitHub with the following command lines:")
    base::packageStartupMessage("\tinstall.packages(\"remotes\")\n\tremotes::install_github(\"ramnathv/htmlwidgets\")")
    base::packageStartupMessage("\n---------------------------\n")
  }
  # if (.Platform$GUI == "RStudio") {
  #   base::packageStartupMessage("\n---------------------------\n")
  #   base::packageStartupMessage("The Shiny graphical user interface launched by the 'ShinyGR' function is sometimes unstable on latest versions of RStudio")
  #   base::packageStartupMessage("It is better to launch the GUI from the Rgui or from R into a terminal")
  #   base::packageStartupMessage("\n---------------------------\n")
  # }
}




## =================================================================================
## commands to avoid warnings during package checking when global variables are used
## =================================================================================

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".ShinyGR.args"))
  utils::suppressForeignCheck(c(".ShinyGR.args"))
  utils::globalVariables(c(".ShinyGR.hist"))
  utils::suppressForeignCheck(c(".ShinyGR.hist"))
}




## =================================================================================
## function to test if a remote url exists
## =================================================================================

.CheckUrl <- function(url, timeout = 2) {
  con <- url(description = url)
  check <- suppressWarnings(try(open.connection(con = con, open = "rt", timeout = timeout),
                                silent = TRUE)[1])
  suppressWarnings(try(close.connection(con), silent = TRUE))
  is.null(check)
}




## =================================================================================
## function to compute the start and stop id of equal values in a vector
## =================================================================================

.StartStop <- function(x, FUN) {
  naQ_rle <- rle(FUN(x))
  naQ_ide <- cumsum(naQ_rle$lengths)[naQ_rle$values]   + 1
  naQ_ids <- naQ_ide - naQ_rle$lengths[naQ_rle$values] - 1
  idNA <- data.frame(start = naQ_ids, stop = naQ_ide)
  idNA$start <- ifelse(idNA$start < 1        , 1        , idNA$start)
  idNA$stop  <- ifelse(idNA$stop  > length(x), length(x), idNA$stop )
  idNA
}




## =================================================================================
## function for drawing several shadows of dygraphic regions simultaneously
## =================================================================================

.DyShadingMulti <- function(dygraph, ts, idStart, IdStop, ...) {
  for (i in seq_along(idStart)) {
    dygraph <- dygraphs::dyShading(dygraph = dygraph,
                                   from    = as.character(ts)[idStart[i]],
                                   to      = as.character(ts)[IdStop[i]],
                                   ...)
  }
  dygraph
}


## =================================================================================
## function to manage the model units
## =================================================================================

.TypeModelGR <- function(x) {

  if (!is.list(x)) {
    x <- list(TypeModel = x)
  }
  if (inherits(x, c("PrepGR", "CalGR", "SimGR")) || names(x) %in% "TypeModel") {
    x <- x$TypeModel
  }

  StrName    <- "(.*)(GR)(\\d{1})(\\D{1})"
  NameModel  <- gsub(StrName, "\\2\\3\\4", x)
  TimeUnitFR <- gsub(StrName, "\\4", x)
  # TimeUnit   <- switch(TimeUnitFR, H = "hour", J = "day", M = "month", A = "year")
  # TimeLag    <- switch(TimeUnit, "hour" = 3600, "day" = 3600*24, "month" = 3600*24*31, "year" = 366)
  TimeUnit   <- switch(TimeUnitFR, H = "h", J = "d", M = "month", A = "y")
  TimeLag    <- switch(TimeUnit, "h" = 3600, "d" = 3600*24, "month" = 3600*24*31, "y" = 366)
  NbParam    <- gsub(StrName, "\\3", x)
  isCN       <- grepl("CemaNeige"  , x)

  res <- list(TypeModel = x, NameModel = NameModel, CemaNeige = isCN,
              NbParam = as.numeric(NbParam),
              TimeUnit = TimeUnit, TimeLag = TimeLag)
  return(res)
}




## =================================================================================
## function to plot the gr models diagrams (only GR4J and GR5J)
## =================================================================================

.DiagramGR <- function(OutputsModel, Param, SimPer, EventDate, HydroModel, CemaNeige, Theme = NULL) {


  # --------------------------------------------------------------------------------
  # PARAMETRES
  # --------------------------------------------------------------------------------

  # Parametres
  mgp             <- c(0, 0.75, 0)
  col_P           <- rgb(066, 139, 202, maxColorValue = 255) #"royalblue"
  col_E           <- rgb(164, 196, 000, maxColorValue = 255) #"forestgreen"
  col_Q           <- "orangered"
  col_SP          <- adjustcolor("cyan4"   , alpha.f = 0.60)
  col_SR          <- adjustcolor("darkblue", alpha.f = 0.60)
  col_R           <- rgb(066, 139, 202, maxColorValue = 255) #rgb(037, 155, 210, maxColorValue = 255)
  col_mod_bg      <- rgb(245, 245, 245, maxColorValue = 255)
  col_mod_bd      <- rgb(231, 231, 231, maxColorValue = 255)
  xy_E            <- c(250, 980)
  xy_PE           <- c(250, 940)
  xy_AE           <- c(250, 860)
  xy_P            <- c(600, 980)
  xy_Precip       <- c(600, 950)
  xy_Q            <- c(700,  30)
  x_Ps            <- 440
  x_PnPs          <- 700
  y_interception  <- 900
  y_rendement     <- 815
  y_percolation   <- 575
  xy_Q9           <- c(400, 310)
  xy_Q1           <- c(800, 310)
  y_routage       <- 100
  fact_res        <- 200/800/3
  fact_resExp     <- 1
  base_res        <- 300
  NH              <- 10
  xy_min_PROD     <- c(200, 610)
  xy_min_ROUT     <- c(250, 150)
  xy_min_EXPO     <- c(200, 250)
  y_entreeUH      <- 500
  xy_UH1          <- c(500, 420)
  xy_UH2          <- c(900, 420)
  y_Ech_Q1        <- 170 #200
  y_Ech_Q9        <- 150 #180
  D               <- 5/2
  xpad            <- 1.5
  ypad            <- 1.5
  max_triangle    <- max(unlist(OutputsModel[c("Perc", "PR", "Q9", "Q1", "QR", "QD", "Pn", "Ps",
                                               "AE", "Precip", "PotEvap", "AExch1", "AExch2")]))
  fact_var        <- 40
  fact_triangle   <- 100#25
  cex_max_poly    <- 2 # 0.005
  cex_tri         <- function(cex, fact = 25, max) suppressWarnings(log(abs(cex) * fact + 1) / max)
  radius1         <- 0
  radius2         <- 60
  tri_R           <- -0x25BA
  tri_B           <- -0x25BC
  tri_L           <- -0x25C4
  tri_T           <- -0x25B2

  par(col.axis = par("fg"), cex.axis = 1.3, cex.lab = 1.3, cex = 0.7, mgp = mgp)

  if (!is.null(Theme)) {
    if (Theme == "Cyborg") {
      col_mod_bg    <- rgb(255-245, 255-245, 255-245, maxColorValue = 255)
      col_mod_bd    <- rgb(255-231, 255-231, 255-231, maxColorValue = 255)
    }
    if (Theme == "Flatly") {
      col_mod_bg    <- "#ECF0F1"
      col_mod_bd    <- "#ECF0F1"
    }
  }

  # Pas de temps
  dates_deb       <- EventDate
  n_pdt           <- length(which(OutputsModel$DatesR >= EventDate & OutputsModel$DatesR <= SimPer[2L]))
  i_pdt           <- which(format(OutputsModel$DatesR, "%Y%m%d") == format(EventDate, "%Y%m%d"))


  # --------------------------------------------------------------------------------
  # UH 1 & 2
  # --------------------------------------------------------------------------------

  if (HydroModel %in% c("GR4J", "GR6J")) {
    # Calcul des ordonnees SH1 de l' "hydrogramme unitaire cumule" UH1
    SH1     <- array(NA, NH)
    for (i in 1:NH) {
      if (i <= 0)                  SH1[i] <- 0
      if (i > 0 & i < Param[4])    SH1[i] <- (i/Param[4])^(D)
      if (i >= Param[4])           SH1[i] <- 1
    }

    # Calcul des ordonnees UH1 de l' "hydrogramme unitaire discret" UH1
    UH1     <- array(NA, NH)
    for (j in 1:NH) {
      if (j == 1) {
        UH1[j] <- SH1[j]
      } else {
        UH1[j] <- SH1[j] - SH1[j-1]
      }
    }

    # Parametres
    max_UH1 <- log(sqrt(max(max(UH1)*OutputsModel$PR*0.9))+1)

  }

  if (HydroModel != "GR2M") {
    # Calcul des ordonnees SH2 de l' "hydrogramme unitaire cumule" UH2
    SH2     <- array(NA, 2*NH)
    for (i in 1:(2*NH)) {
      if (i <= 0)                           SH2[i] <- 0
      if (i > 0 & i < Param[4])             SH2[i] <- 0.5*(i/Param[4])^(D)
      if (i >= Param[4] & i < 2*Param[4])   SH2[i] <- 1 - (0.5*(2-i/Param[4])^(D))
      if (i >= 2*Param[4])                  SH2[i] <- 1
    }

    # Calcul des ordonnees UH2 de l' "hydrogramme unitaire discret" UH2
    UH2     <- array(NA, 2*NH)
    for (j in 1:(2*NH)) {
      if (j == 1) {
        UH2[j] <- SH2[j]
      } else {
        UH2[j] <- SH2[j] - SH2[j-1]
      }
    }

    # Parametres
    max_UH2 <- log(sqrt(max(max(UH2)*OutputsModel$PR*0.1))+1)
  }

  # --------------------------------------------------------------------------------
  # PARTITIONNEMENT FENETRE GRAPHIQUE
  # --------------------------------------------------------------------------------

  # layout(matrix(c(1:4, 4, 4), nrow = 3, ncol = 2, byrow = FALSE), widths = c(1.0, 0.6))

  # --------------------------------------------------------------------------------
  # PLUIE ET ETP
  # --------------------------------------------------------------------------------

  # P
  # par(mar = c(2, 4, 1, 1), mgp = mgp)
  # plot(OutputsModel$Dates, OutputsModel$Precip, type = "h", col = col_P, ylim = rev(range(OutputsModel$Precip)), xaxt = "n", ylab = "precip. [mm/d]")
  # rect(xleft = EventDate, ybottom = par("usr")[3], xright =  par("usr")[2], ytop =  par("usr")[4], col = adjustcolor(par("bg"), alpha.f = 0.75), border = NA)
  # abline(v = EventDate, col = "grey", lwd = 2, lty = 2)
  # box()

  # ETP
  # par(mar = c(2, 4, 1, 1), mgp = mgp)
  # plot(OutputsModel$Dates, OutputsModel$PotEvap, pch = 19, col = col_E, xaxt = "n", ylab = "evapo. [mm/d]")
  # rect(xleft = EventDate, ybottom = par("usr")[3], xright =  par("usr")[2], ytop =  par("usr")[4], col = adjustcolor(par("bg"), alpha.f = 0.75), border = NA)
  # abline(v = EventDate, col = "grey", lwd = 2, lty = 2)
  # box()


  # --------------------------------------------------------------------------------
  # DEBIT
  # --------------------------------------------------------------------------------

  # Q
  # par(mar = c(2, 4, 1, 1), mgp = mgp)
  # plot(OutputsModel$Dates, OutputsModel$Qobs, type = "l", ylab = "flow [mm/d]")
  # lines(OutputsModel$Dates[1:i_pdt], OutputsModel$Qsim[1:i_pdt], type = "l", col = "orangered")
  # rect(xleft = EventDate, ybottom = par("usr")[3], xright =  par("usr")[2], ytop =  par("usr")[4], col = adjustcolor(par("bg"), alpha.f = 0.75), border = NA)
  # abline(v = EventDate, col = "grey", lwd = 2, lty = 2)
  # box()


  # --------------------------------------------------------------------------------
  # SCHEMAS MODELES
  # --------------------------------------------------------------------------------

  # Cadre
  par(mar = rep(0.2, 4))
  par(fg = par("fg"))
  plot(x = 0, type = "n", xlab = "", ylab = "", axes = FALSE, ylim = c(0, 1000), xlim = c(0, 1000))

  # Le modele
  rect(xleft = 0, xright = 1000, ybottom = 50, ytop = 970, col = col_mod_bg, border = col_mod_bd)


  # --------------------------------------------------------------------------------
  # ENTREES / SORTIES
  # --------------------------------------------------------------------------------

  # Entrees P et ETP
  if (CemaNeige) {
    text(x = xy_P[1]*1.65, y = xy_P[2]*0.98, labels = "+ CemaNeige", adj = c(1, 1), font = 2, col = "grey40", cex = 1.6)
  }
  text(x = xy_P[1], y = xy_P[2], labels = "P", pos = 3, font = 2, col = col_P, cex = 1.8)
  text(x = xy_E[1], y = xy_E[2], labels = "E", pos = 3, font = 2, col = col_E, cex = 1.8)

  # Sorties Q
  text(x = xy_Q[1], y = xy_Q[2], labels = "Q", pos = 1, font = 2, col = col_Q, cex = 1.8)

  # Parametres
  tmp_decal   <- 20


  # --------------------------------------------------------------------------------
  # NEUTRALISATION DE P
  # --------------------------------------------------------------------------------

  if (HydroModel != "GR2M") {
    # Interception
    segments(x0 = xy_E[1]-50, x1 = xy_P[1]+50,
             y0 = y_interception+tmp_decal, y1 = y_interception+tmp_decal)
    text(x = xy_P[1]+50, y = y_interception+20, labels = "Interception", pos = 4, font = 1, cex = 1.4)
  }

  # E vers Es et P vers Ps ou Pn
  y_Xs <- ifelse(HydroModel == "GR2M", y_rendement+2*tmp_decal, y_interception+tmp_decal)

  # P vers Pn
  segments(x0 = xy_P[1], x1 = xy_P[1], y0 = xy_P[2], y1 = y_Xs)

  # Pn vers Ps
  segments(x0 = xy_P[1], x1 = xy_P[1],
           y0 = y_interception, y1 = y_rendement+2*tmp_decal)
  segments(x0 = x_Ps, x1 = xy_P[1],
           y0 = y_rendement+2*tmp_decal, y1 = y_rendement+2*tmp_decal)
  segments(x0 = x_Ps, x1 = x_Ps,
           y0 = y_rendement+2*tmp_decal, y1 = y_rendement)

  # Pn vers Pn - Ps (P vers Pn si GR2M)
  segments(x0 = xy_P[1], x1 = x_PnPs,
           y0 = y_rendement+2*tmp_decal, y1 = y_rendement+2*tmp_decal)
  segments(x0 = x_PnPs , x1 = x_PnPs,
           y0 = y_rendement+2*tmp_decal, y1 = y_rendement)

  # Pn - Ps vers Pr (Pn vers Pr si GR2M)
  segments(x0 = x_PnPs, x1 = x_PnPs,
           y0 = y_rendement, y1 = y_percolation)

  # E vers En puis Es
  segments(x0 = xy_E[1], x1 = xy_E[1],
           y0 = xy_E[2], y1 = y_Xs)
  segments(x0 = xy_E[1], x1 = xy_E[1],
           y0 = y_interception, y1 = y_rendement)

  if (HydroModel != "GR2M") {
    # Ecriture
    plotrix::boxed.labels(x = xy_P[1], y = y_interception, labels = "Pn",
                          bg = col_mod_bg, border = NA, xpad = xpad, ypad = ypad)
    plotrix::boxed.labels(x = xy_E[1], y = y_interception, labels = "En",
                          bg = col_mod_bg, border = NA, xpad = xpad, ypad = ypad)
  }

  # ETP
  if (OutputsModel$PotEvap[i_pdt] != 0) {
    points(x = xy_PE[1], y =  xy_PE[2],
           type = "p", pch = tri_T, col = col_E,
           cex = cex_tri(OutputsModel$PotEvap[i_pdt], fact = fact_triangle, max = cex_max_poly))
  }

  # Precipitation
  if (OutputsModel$Precip[i_pdt] != 0) {
    points(x = xy_Precip[1], y =  xy_Precip[2],
           type = "p", pch = tri_B, col = col_P,
           cex = cex_tri(OutputsModel$Precip[i_pdt], fact = fact_triangle, max = cex_max_poly))
  }

  # Pn et Ps
  points(x = x_Ps, y = y_rendement+1.2*tmp_decal,
         type = "p", pch = tri_B, col = col_P,
         cex = cex_tri(OutputsModel$Ps[i_pdt], fact = fact_triangle, max = cex_max_poly))
  if (HydroModel != "GR2M") {
    points(x = x_PnPs, y = y_rendement+1.2*tmp_decal,
           type = "p", pch = tri_B, col = col_P,
           cex = cex_tri(OutputsModel$Pn[i_pdt] - OutputsModel$Ps[i_pdt], fact = fact_triangle, max = cex_max_poly))
  } else {
    points(x = x_PnPs, y = y_rendement+1.2*tmp_decal,
           type = "p", pch = tri_B, col = col_P,
           cex = cex_tri(OutputsModel$Pn[i_pdt], fact = fact_triangle, max = cex_max_poly))
  }


  # --------------------------------------------------------------------------------
  # FONCTION DE RENDEMENT
  # --------------------------------------------------------------------------------

  # Es
  plotrix::boxed.labels(x = xy_E[1], y = y_rendement, labels = "Es",
                        bg = col_mod_bg, border = NA, xpad = xpad, ypad = ypad)

  # Evaporation reelle
  if (OutputsModel$AE[i_pdt] != 0) {
    points(x = xy_AE[1], y =  xy_AE[2],
           type = "p", pch = tri_T, col = col_P,
           cex = cex_tri(OutputsModel$AE[i_pdt], fact = fact_triangle, max = cex_max_poly))
  }

  # Ps et Pn - Ps (Ps et Pn si GR2M)
  plotrix::boxed.labels(x = x_Ps  , y = y_rendement, labels = "Ps"   ,
                        bg = col_mod_bg, border = NA, xpad = xpad, ypad = ypad)
  plotrix::boxed.labels(x = x_PnPs, y = y_rendement, labels = ifelse(HydroModel != "GR2M", "Pn - Ps", "Pn"),
                        bg = col_mod_bg, border = NA, xpad = xpad, ypad = ypad)

  # Reservoir de production
  rect(xleft   = xy_min_PROD[1], xright = xy_min_PROD[1]+base_res,
       ybottom = xy_min_PROD[2], ytop   = xy_min_PROD[2]+OutputsModel$Prod[i_pdt]*fact_res,
       col = col_SP, border = NA)
  segments(x0 = xy_min_PROD[1], x1 = xy_min_PROD[1]+base_res,
           y0 = xy_min_PROD[2], y1 = xy_min_PROD[2])
  segments(x0 = xy_min_PROD[1], x1 = xy_min_PROD[1],
           y0 = xy_min_PROD[2], y1 = xy_min_PROD[2]+Param[1]*fact_res)
  segments(x0 = xy_min_PROD[1]+base_res, x1 = xy_min_PROD[1]+base_res,
           y0 = xy_min_PROD[2], y1 = xy_min_PROD[2]+Param[1]*fact_res)
  text(x = 30, y = xy_min_PROD[2]+15, labels = "Prod.\nstore", cex = 1.4, pos = 4)


  # --------------------------------------------------------------------------------
  # PERCOLATION
  # --------------------------------------------------------------------------------

  # Reservoir de production vers Pr
  segments(x0 = xy_min_PROD[1]+base_res/2, x1 = xy_min_PROD[1]+base_res/2,
           y0 = xy_min_PROD[2], y1 = y_percolation)
  segments(x0 = xy_min_PROD[1]+base_res/2, x1 = x_PnPs,
           y0 = y_percolation, y1 = y_percolation)

  # Perc
  plotrix::boxed.labels(x = xy_min_PROD[1]+base_res/2, y = y_percolation, labels = "Perc.",
                        bg = col_mod_bg, border = NA, xpad = xpad, ypad = ypad)

  # Valeur de Perc
  if (OutputsModel$Perc[i_pdt] != 0) {
    points(x = xy_min_PROD[1]+base_res+75, y = y_percolation,
           type = "p", pch = tri_R, col = col_P,
           cex = cex_tri(OutputsModel$Perc[i_pdt], fact = fact_triangle, max = cex_max_poly))
  }

  # parametres
  tmp_decal   <- (y_percolation - y_entreeUH) / 2

  # Pr vers UH (Pr vers reservoir de routage si GR2M)
  k <- ifelse(HydroModel == "GR2M", 0.5, 1)
  segments(x0 = x_PnPs, x1 = x_PnPs,
           y0 = y_percolation, y1 = (y_entreeUH*k) + tmp_decal/2)


  if (HydroModel == "GR2M") {
    plotrix::boxed.labels(x = x_PnPs, y = y_percolation, labels = "Pr",
                          bg = col_mod_bg, border = NA, xpad = xpad, ypad = ypad)

    if (OutputsModel$PR[i_pdt] != 0) {
      points(x = x_PnPs[1], y = y_entreeUH+tmp_decal,
             type = "p", pch = tri_B, col = col_P,
             cex = cex_tri(OutputsModel$PR[i_pdt], fact = fact_triangle, max = cex_max_poly))
    }
  }


  if (HydroModel %in% c("GR4J", "GR6J")) {

    # --------------------------------------------------------------------------------
    # SEPARATION DE PR
    # --------------------------------------------------------------------------------

    # Pr vers UH1
    segments(x0 = xy_Q9[1], x1 = x_PnPs,
             y0 = y_entreeUH+tmp_decal/2, y1 = y_entreeUH+tmp_decal/2)
    segments(x0 = xy_Q9[1], x1 = xy_Q9[1],
             y0 = y_entreeUH+tmp_decal/2, y1 = xy_Q9[2])

    # Pr vers UH2
    segments(x0 = x_PnPs, x1 = xy_Q1[1],
             y0 = y_entreeUH+tmp_decal/2, y1 = y_entreeUH+tmp_decal/2)
    segments(x0 = xy_Q1[1], x1 = xy_Q1[1],
             y0 = y_entreeUH+tmp_decal/2, y1 = y_routage)

    # Pr
    plotrix::boxed.labels(x = x_PnPs, y = y_percolation, labels = "Pr",
                          bg = col_mod_bg, border = NA, xpad = xpad, ypad = ypad)

    # Pr
    if (OutputsModel$PR[i_pdt] != 0) {
      points(x = x_PnPs[1], y = y_entreeUH+tmp_decal,
             type = "p", pch = tri_B, col = col_P,
             cex = cex_tri(OutputsModel$PR[i_pdt], fact = fact_triangle, max = cex_max_poly))
    }


    # --------------------------------------------------------------------------------
    # HYDROGRAMME UNITAIRE 1
    # --------------------------------------------------------------------------------

    # Entree de UH1
    if (OutputsModel$PR[i_pdt] != 0) {
      points(x = xy_Q9[1], y =y_entreeUH,
             type = "p", pch = tri_B, col = col_P,
             cex =  cex_tri(OutputsModel$PR[i_pdt]*0.9, fact = fact_triangle, max = cex_max_poly))
    }

    # Remplissage de UH1
    PR_mat_UH1_lg <- ceiling(Param[4])
    PR_mat_UH1_id <- max(i_pdt-PR_mat_UH1_lg+1, 1):i_pdt
    PR_mat_UH1 <- matrix(rep(c(rep(0, times = PR_mat_UH1_lg-length(PR_mat_UH1_id)+1),
                               OutputsModel$PR[PR_mat_UH1_id]), times = PR_mat_UH1_lg),
                         ncol = PR_mat_UH1_lg+1)[, -1L]
    PR_mat_UH1[lower.tri(PR_mat_UH1)] <- 0


    # --------------------------------------------------------------------------------
    # HYDROGRAMME UNITAIRE 2
    # --------------------------------------------------------------------------------

    # Entree de UH2
    if (OutputsModel$PR[i_pdt] != 0) {
      points(x = xy_Q1[1], y = y_entreeUH,
             type = "p", pch = tri_B, col = col_P,
             cex = cex_tri(OutputsModel$PR[i_pdt]*0.1, fact = fact_triangle, max = cex_max_poly))
    }

    # Remplissage de UH2
    PR_mat_UH2_lg <- ceiling(Param[4]*2)
    PR_mat_UH2_id <- max(i_pdt-PR_mat_UH2_lg+1, 1):i_pdt
    PR_mat_UH2 <- matrix(rep(c(rep(0, times = PR_mat_UH2_lg-length(PR_mat_UH2_id)+1),
                               OutputsModel$PR[PR_mat_UH2_id]), times = PR_mat_UH2_lg),
                         ncol = PR_mat_UH2_lg+1)[, -1L]
    PR_mat_UH2[lower.tri(PR_mat_UH2)] <- 0

  }

  if (HydroModel == "GR5J") {

    # --------------------------------------------------------------------------------
    # SEPARATION DE PR
    # --------------------------------------------------------------------------------

    # sortie UH
    segments(x0 = x_PnPs, x1 = x_PnPs,
             y0 = y_entreeUH-2*tmp_decal, y1 = y_entreeUH-3*tmp_decal)

    # sortie UH vers branche 1
    segments(x0 = xy_Q9[1], x1 = x_PnPs,
             y0 = y_entreeUH-3*tmp_decal, y1 = y_entreeUH-3*tmp_decal)
    segments(x0 = xy_Q9[1], x1 = xy_Q9[1],
             y0 = y_entreeUH-3*tmp_decal, y1 = xy_Q9[2])

    # sortie UH vers branche 2
    segments(x0 = x_PnPs, x1 = xy_Q1[1],
             y0 = y_entreeUH-3*tmp_decal, y1 = y_entreeUH-3*tmp_decal)
    segments(x0 = xy_Q1[1], x1 = xy_Q1[1],
             y0 = y_entreeUH-3*tmp_decal, y1 = y_routage)

    # Pr
    plotrix::boxed.labels(x = x_PnPs, y = y_percolation, labels = "Pr",
                          bg = col_mod_bg, border = NA, xpad = xpad, ypad = ypad)


    # --------------------------------------------------------------------------------
    # HYDROGRAMME UNITAIRE
    # --------------------------------------------------------------------------------

    # Entree de UH (PR)
    if (OutputsModel$PR[i_pdt] != 0) {
      points(x = x_PnPs[1], y = y_entreeUH+tmp_decal,
             type = "p", pch = tri_B, col = col_P,
             cex = cex_tri(OutputsModel$PR[i_pdt], fact = fact_triangle, max = cex_max_poly))
    }


    # Remplissage de UH2
    PR_mat_UH2_lg <- ceiling(Param[4]*2)
    PR_mat_UH2_id <- max(i_pdt-PR_mat_UH2_lg+1, 1):i_pdt
    PR_mat_UH2 <- matrix(rep(c(rep(0, times = PR_mat_UH2_lg-length(PR_mat_UH2_id)+1),
                               OutputsModel$PR[PR_mat_UH2_id]), times = PR_mat_UH2_lg),
                         ncol = PR_mat_UH2_lg+1)[, -1L]
    PR_mat_UH2[lower.tri(PR_mat_UH2)] <- 0


    # Sorties de UH
    if (PR_mat_UH2[1] != 0) {
      points(x = x_PnPs[1], y =  y_entreeUH-5*tmp_decal/2,
             type = "p", pch = tri_B, col = col_P,
             cex = cex_tri(PR_mat_UH2[1], fact = fact_triangle, max = cex_max_poly))
    }

  }

  # sortie de UH1 vers reservoirs exponentiel et de routage
  if (HydroModel == "GR6J") {
    segments(x0 = xy_Q9[1], x1 = xy_Q9[1],
             y0 = y_entreeUH-3*tmp_decal, y1 = xy_Q9[2])
    segments(x0 = xy_Q9[1]*0.80, x1 = xy_Q9[1]*1.30,
             y0 = xy_Q9[2], y1 = xy_Q9[2])
    segments(x0 = xy_Q9[1]*1.30, x1 = xy_Q9[1]*1.30,
             y0 = xy_Q9[2], y1 = xy_Q9[2]*0.65)
    segments(x0 = xy_Q9[1]*0.80, x1 = xy_Q9[1]*0.80,
             y0 = xy_Q9[2], y1 = xy_Q9[2]*0.90)
    segments(x0 = xy_Q9[1]*0.55, x1 = xy_Q9[1]*0.55,
             y0 = xy_Q9[2]*0.70, y1 = y_routage)
    segments(x0 = xy_Q9[1]*0.55, x1 = xy_min_ROUT[1]+base_res/2,
             y0 = y_routage, y1 = y_routage)
  }

  if (HydroModel != "GR2M") {
    # Q9
    if (OutputsModel$Q9[i_pdt] != 0) {
      points(x = xy_Q9[1], y = xy_Q9[2]+tmp_decal,
             type = "p", pch = tri_B, col = col_P,
             cex = cex_tri(OutputsModel$Q9[i_pdt], fact = fact_triangle, max = cex_max_poly))
      if (HydroModel == "GR6J") {
        # Q9 exp
        points(x = xy_Q9[1]*0.80, y = xy_Q9[1]*0.73,
               type = "p", pch = tri_B, col = col_P,
               cex = cex_tri(OutputsModel$Q9[i_pdt]*0.4, fact = fact_triangle, max = cex_max_poly))
        # Q9 rout
        points(x = xy_Q9[1]*1.30, y = xy_Q9[1]*0.73,
               type = "p", pch = tri_B, col = col_P,
               cex = cex_tri(OutputsModel$Q9[i_pdt]*0.6, fact = fact_triangle, max = cex_max_poly))
        # QrExp
        plotrix::boxed.labels(x = xy_Q9[1]*0.55, y = y_routage, labels = "QrExp", bg = col_mod_bg, border = NA, xpad = xpad, ypad = ypad)
      }
    }
    plotrix::boxed.labels(x = xy_Q9[1], y = xy_Q9[2], labels = "Q9",
                          bg = col_mod_bg, border = NA, xpad = xpad, ypad = ypad)


    # Q1
    if (OutputsModel$Q1[i_pdt] != 0) {
      points(x = xy_Q1[1], y =  xy_Q1[2]+tmp_decal,
             type = "p", pch = tri_B, col = col_P,
             cex = cex_tri(OutputsModel$Q1[i_pdt], fact = fact_triangle, max = cex_max_poly))
      segments(x0 = xy_Q[1], x1 = xy_Q1[1], y0 = y_routage, y1 = y_routage)
    }

    plotrix::boxed.labels(x = xy_Q1[1], y = xy_Q1[2], labels = "Q1", bg = col_mod_bg, border = NA, xpad = xpad, ypad = ypad)

    # Valeur de Qd
    if (OutputsModel$QD[i_pdt] != 0) {
      points(x = xy_Q[1]+30, y =  y_routage,
             type = "p", pch = tri_L, col = col_P,
             cex = cex_tri(OutputsModel$QD[i_pdt], fact = fact_triangle, max = cex_max_poly))
    }

    # Qd
    plotrix::boxed.labels(x = xy_Q1[1], y = y_routage, labels = "Qd", bg = col_mod_bg, border = NA, xpad = xpad, ypad = ypad)

  }

  # --------------------------------------------------------------------------------
  # RESERVOIR DE ROUTAGE
  # --------------------------------------------------------------------------------

  # Triche pour la taille du reservoire de routage
  tmp_triche   <- 0#80

  # Reservoir de routage
  if (HydroModel == "GR2M") {
    xy_min_ROUT[1] <- x_PnPs - base_res/2
    Param[3] <- 600
  }
  rect(xleft = xy_min_ROUT[1], xright = xy_min_ROUT[1]+base_res,
       ybottom = xy_min_ROUT[2], ytop = xy_min_ROUT[2]+OutputsModel$Rout[i_pdt]*fact_res+tmp_triche,
       col = col_SR, border = NA)
  segments(x0 = xy_min_ROUT[1], x1 = xy_min_ROUT[1]+base_res,
           y0 = xy_min_ROUT[2], y1 = xy_min_ROUT[2])
  segments(x0 = xy_min_ROUT[1], x1 = xy_min_ROUT[1],
           y0 = xy_min_ROUT[2], y1 = xy_min_ROUT[2]+Param[3]*fact_res+tmp_triche)
  segments(x0 = xy_min_ROUT[1]+base_res, x1 = xy_min_ROUT[1]+base_res,
           y0 = xy_min_ROUT[2], y1 = xy_min_ROUT[2]+Param[3]*fact_res+tmp_triche)
  text(x = 30, y = xy_min_ROUT[2]+15, labels = "Routing\nstore", cex = 1.4, pos = 4)

  # Sorties du reservoir
  segments(x0 = xy_min_ROUT[1]+base_res/2, x1 = xy_min_ROUT[1]+base_res/2,
           y0 = xy_min_ROUT[2], y1 = y_routage)
  segments(x0 = xy_min_ROUT[1]+base_res/2, x1 = xy_Q[1],
           y0 = y_routage, y1 = y_routage)


  if (HydroModel != "GR2M") {
    # Qr
    if (HydroModel != "GR6J") {
      plotrix::boxed.labels(x = xy_min_ROUT[1]+base_res/2, y = y_routage, labels = "Qr",
                            bg = col_mod_bg, border = NA, xpad = xpad, ypad = ypad)
    }
    if (HydroModel == "GR6J") {
      plotrix::boxed.labels(x = xy_min_ROUT[1]+base_res/1.5, y = (xy_min_ROUT[2]+y_routage)/2, labels = "Qr",
                            bg = col_mod_bg, border = NA, xpad = xpad, ypad = ypad)
    }

    # Valeur de Qr
    if (OutputsModel$QR[i_pdt] != 0) {
      if (HydroModel != "GR6J") {
        points(x = xy_Q[1]-100, y = y_routage,
               type = "p", pch = tri_R, col = col_P,
               cex = cex_tri(OutputsModel$QR[i_pdt], fact = fact_triangle, max = cex_max_poly))
      } else {
        points(x = xy_min_ROUT[1]+base_res/2, y = (xy_min_ROUT[2]+y_routage)/2,
               type = "p", pch = tri_B, col = col_P,
               cex = cex_tri(OutputsModel$QR[i_pdt], fact = fact_triangle, max = cex_max_poly))
      }
    }
  }


  # --------------------------------------------------------------------------------
  # RESERVOIR EXPONENTIEL
  # --------------------------------------------------------------------------------

  if (HydroModel == "GR6J") {

    # Triche pour la taille du reservoire exponentiel
    tmp_triche   <- 0#80

    # Exp en log
    signExp <- ifelse(OutputsModel$Exp[i_pdt] > 0, +1, -1)
    logExpIpdt <- log(abs(OutputsModel$Exp[i_pdt])+1e-6) * signExp
    logExpMax  <- log(max(abs(OutputsModel$Exp  ))+1e-6)

    # Reservoir exponentiel
    rect(xleft = xy_min_EXPO[1], xright = xy_min_EXPO[1]+base_res,
         ybottom = xy_min_EXPO[2], ytop = xy_min_EXPO[2]+logExpIpdt*fact_resExp+tmp_triche,
         col = ifelse(OutputsModel$Exp[i_pdt] > 0, "#10B510", "#FF0303"), border = NA)
    segments(x0 = xy_min_EXPO[1], x1 = xy_min_EXPO[1]+base_res,
             y0 = xy_min_EXPO[2], y1 = xy_min_EXPO[2])
    segments(x0 = xy_min_EXPO[1], x1 = xy_min_EXPO[1],
             y0 = xy_min_EXPO[2], y1 = xy_min_EXPO[2]+logExpMax*fact_resExp+tmp_triche)
    segments(x0 = xy_min_EXPO[1]+base_res, x1 = xy_min_EXPO[1]+base_res,
             y0 = xy_min_EXPO[2], y1 = xy_min_EXPO[2]+logExpMax*fact_resExp+tmp_triche)
    segments(x0 = xy_min_EXPO[1], x1 = xy_min_EXPO[1],
             y0 = xy_min_EXPO[2], y1 = xy_min_EXPO[2]-logExpMax*fact_resExp-tmp_triche)
    segments(x0 = xy_min_EXPO[1]+base_res, x1 = xy_min_EXPO[1]+base_res,
             y0 = xy_min_EXPO[2], y1 = xy_min_EXPO[2]-logExpMax*fact_resExp-tmp_triche)
    text(x = 30, y = xy_min_EXPO[2]+00, labels = "Exp.\nstore", cex = 1.4, pos = 4)
    points(x = 180, y = xy_min_EXPO[2]+20, pch = 43, # +
           cex = 2.0, col = "#10B510")
    points(x = 178, y = xy_min_EXPO[2]-20, pch = 95, # -
           cex = 1.6, col = "#FF0303")

    # Valeur de QrExp
    if (OutputsModel$QR[i_pdt] != 0) {
      points(x = xy_Q[1]-350, y = y_routage,
             type = "p", pch = tri_R, col = col_P,
             cex = cex_tri(OutputsModel$QRExp[i_pdt], fact = fact_triangle, max = cex_max_poly))
    }

    # Valeur de Qr + QrExp
    if (OutputsModel$QR[i_pdt] != 0) {
      points(x = xy_Q[1]-100, y = y_routage,
             type = "p", pch = tri_R, col = col_P,
             cex = cex_tri(OutputsModel$QR[i_pdt]+OutputsModel$QRExp[i_pdt], fact = fact_triangle, max = cex_max_poly))
    }
  }


  # --------------------------------------------------------------------------------
  # Q FINAL
  # -------------------------------------------------------------------------------

  # Q final
  segments(x0 = xy_Q[1], x1 = xy_Q[1], y0 = y_routage, y1 = xy_Q[2]+10)
  if (OutputsModel$Qsim[i_pdt] != 0) {
    points(x = xy_Q[1], y =  mean(c(y_routage, xy_Q[2]+10)),
           type = "p", pch = tri_B, col = col_Q,
           cex = cex_tri(OutputsModel$Qsim[i_pdt], fact = fact_triangle, max = cex_max_poly))
  }


  # --------------------------------------------------------------------------------
  # EXCHANGE
  # --------------------------------------------------------------------------------

  if (HydroModel != "GR2M") {
    # Actual exchange Q9
    arrows(x0 = xy_min_ROUT[1]+base_res, x1 = 1025,
           y0 = y_Ech_Q9               , y1 = y_Ech_Q9,
           length = 0.075, angle = 20, code = 3)
    pch <- ifelse(OutputsModel$AExch1[i_pdt] < 0, tri_R, tri_L)
    points(x = xy_min_ROUT[1]+base_res+130, y = y_Ech_Q9,
           type = "p", pch = pch, col = col_P,
           cex = cex_tri(OutputsModel$AExch1[i_pdt], fact = fact_triangle, max = cex_max_poly))

    # Actual exchange Q1
    arrows(x0 = xy_Q1[1], x1 = 1025,
           y0 = y_Ech_Q1, y1 = y_Ech_Q1,
           length = 0.075, angle = 20, code = 3)
    pch <- ifelse(OutputsModel$AExch2[i_pdt] < 0, tri_R, tri_L)
    points(x = xy_Q1[1]+100, y = y_Ech_Q1,
           type = "p", pch = pch, col = col_P,
           cex = cex_tri(OutputsModel$AExch2[i_pdt], fact = fact_triangle, max = cex_max_poly))
  }

  if (HydroModel == "GR2M") {
    # Actual exchange
    arrows(x0 = xy_min_ROUT[1]+base_res, x1 = 1025,
           y0 = y_Ech_Q9               , y1 = y_Ech_Q9,
           length = 0.075, angle = 20, code = 3)
    pch <- ifelse(OutputsModel$AExch[i_pdt] < 0, tri_R, tri_L)
    points(x = xy_min_ROUT[1]+base_res+80, y = y_Ech_Q9,
           type = "p", pch = pch, col = col_P,
           cex = cex_tri(OutputsModel$AExch[i_pdt], fact = fact_triangle, max = cex_max_poly))
  }

  # Actual exchange Q9 exp.
  if (HydroModel %in% c("GR6J")) {
    arrows(x0 = xy_min_EXPO[1]+base_res[1], x1 = 1025,
           y0 = xy_min_EXPO[2], y1 = xy_min_EXPO[2],
           length = 0.075, angle = 20, code = 3)
    pch <- ifelse(OutputsModel$Exch[i_pdt] < 0, tri_R, tri_L)
    points(x = xy_Q1[1]+100, y = xy_min_EXPO[2],
           type = "p", pch = pch, col = col_P,
           cex = cex_tri(OutputsModel$Exch[i_pdt], fact = fact_triangle, max = cex_max_poly))
  }

  if (HydroModel %in% c("GR4J", "GR6J")) {

    # --------------------------------------------------------------------------------
    # UH 1 & 2 PLOT
    # --------------------------------------------------------------------------------

    tmpUH1 <- PR_mat_UH1 %*% UH1[seq_len(PR_mat_UH1_lg)] * 0.75 # 0.75 au lieu de 0.90 pour reduire dif. visuelle
    tmpUH2 <- PR_mat_UH2 %*% UH2[seq_len(PR_mat_UH2_lg)] * 0.25 # 0.25 au lieu de 0.10 pour reduire dif. visuelle
    palUH0 <- colorRampPalette(c("#428BCA", "#B5D2EA"))
    par(mar = rep(1, 4))
    par(new = TRUE, plt = c(0.35, 0.46,  0.40, 0.45), yaxt = "n", bg = "red")
    rect(xleft = 325, xright = 475,  ybottom = 390, ytop = 445, col = col_mod_bg, border = NA)
    par(new = TRUE)
    barplot(tmpUH1, beside = TRUE, space = 0, col = palUH0(length(tmpUH1)),
            ylim = range(c(0, 5/length(PR_mat_UH2_lg)), na.rm = TRUE))
    par(new = TRUE, plt = c(0.67, 0.89,  0.40, 0.45), yaxt = "n")
    rect(xleft = 0, xright = 900,  ybottom = 0, ytop = 470, col = col_mod_bg, border = NA)
    par(new = TRUE)
    barplot(tmpUH2, beside = TRUE, space = 0, col = palUH0(length(tmpUH2)),
            ylim = range(c(0, 5/length(PR_mat_UH2_lg)), na.rm = TRUE))

  }


  if (HydroModel == "GR5J") {

    # --------------------------------------------------------------------------------
    # UH PLOT
    # --------------------------------------------------------------------------------

    tmpUH2 <- PR_mat_UH2 %*% UH2[seq_len(PR_mat_UH2_lg)] * 1 / 2 # * 1/2 car seul HU et pour que ca rentre
    palUH0 <- colorRampPalette(c("#428BCA", "#B5D2EA"))
    par(mar = rep(1, 4))
    par(new = TRUE, plt = c(0.57, 0.79,  0.45, 0.5), yaxt = "n")
    par(new = TRUE)
    barplot(tmpUH2, beside = TRUE, space = 0, col = palUH0(length(tmpUH2)),
            ylim = range(c(0, 5/length(PR_mat_UH2_lg)), na.rm = TRUE))

  }

}


