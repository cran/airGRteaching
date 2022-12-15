context("SimGR")

# --------

data(L0123001, package = "airGR")

BasinObsUsual <- BasinObs[, c("DatesR", "P", "E", "Qmm", "T")]
BasinObsSubNA <- BasinObsUsual
BasinObsSubNA[BasinObsSubNA$DatesR >= as.POSIXct("1994-01-01", tz = "UTC"), ]$Qmm <- NA
BasinObsTotNA <- BasinObsSubNA[BasinObsSubNA$DatesR >= as.POSIXct("1994-01-01", tz = "UTC"), ]

PrepUsual <- PrepGR(ObsDF = BasinObsUsual, HydroModel = "GR4J", CemaNeige = FALSE)
PrepSubNA <- PrepGR(ObsDF = BasinObsSubNA, HydroModel = "GR4J", CemaNeige = FALSE)
PrepTotNA <- PrepGR(ObsDF = BasinObsTotNA, HydroModel = "GR4J", CemaNeige = FALSE)

# --------

test_that("NA on a sub-period", {
  expect_message(
    object = SimGR(PrepGR = PrepSubNA, Param = c(270.426, 0.984, 108.853, 2.149), EffCrit = "KGE2",
                   WupPer = c("1994-01-01", "1994-12-31"),
                   SimPer = c("1995-01-01", "1998-12-31")),
    regexp = "\"PrepGR\" does not contain any Qobs values on \"SimPer\". The efficiency criterion is not computed",
  )
})

# ----

test_that("NA on the total period", {
  expect_warning(
    object = SimGR(PrepGR = PrepTotNA, Param = c(270.426, 0.984, 108.853, 2.149), EffCrit = "KGE2",
                   WupPer = c("1994-01-01", "1994-12-31"),
                   SimPer = c("1995-01-01", "1998-12-31")),
    regexp = "\"PrepGR\" does not contain any Qobs values. The efficiency criterion is not computed",
  )
})

# ----

test_that("Disable warm-up period", {
  expect_message(
    object = SimGR(PrepGR = PrepUsual, Param = c(270.426, 0.984, 108.853, 2.149), EffCrit = "KGE2",
                   WupPer = 0L,
                   SimPer = c("1995-01-01", "1998-12-31")),
    regexp = "No warm up period is used",
  )
})
