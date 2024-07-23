context("plot")

# --------

## data.frame of observed data
data(L0123001, package = "airGR")
BasinObs2 <- BasinObs[, c("DatesR", "P", "E", "Qmm", "T")]

## Preparation of observed data for an ungauged catchment (i.e. no observed discharge available)
PREP <- PrepGR(DatesR = BasinObs2$DatesR, Precip = BasinObs2$P,
               PotEvap = BasinObs2$E, Qobs = NA,
               HydroModel = "GR4J", CemaNeige = FALSE)

## Simulation step using model parameter set by the user
SIM <- SimGR(PrepGR = PREP, Param = c(270.426, 0.984, 108.853, 2.149), EffCrit = "KGE2",
             WupPer = NULL, SimPer = c("1994-01-01", "1998-12-31"))

# --------

# temporarily needed for the functions used below
local_edition(3)

# ----

test_that("No 'Error' graphic produced beacause no 'Qobs'", {
  expect_snapshot_error(plot(SIM, which = "Error"))
})

# ----

test_that("No 'CorQQ' graphic produced beacause no 'Qobs'", {
  expect_snapshot_error(plot(SIM, which = "CorQQ"))
})

# ----

test_that("No 'Error' and 'CorQQ' graphic produced beacause no 'Qobs'", {
  expect_snapshot_error(plot(SIM, which = c("Error", "CorQQ")))
})

# ----

test_that("No 'Error' graphic produced beacause no 'Qobs'", {
  expect_snapshot_warning(plot(SIM, which = c("Error", "Flows")))
})

# ----

