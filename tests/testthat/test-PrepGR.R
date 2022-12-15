context("PrepGR")

# --------

library(tibble)

# --------

data(L0123001, package = "airGR")

BasinObs <- BasinObs[, c("DatesR", "P", "E", "Qmm", "T")]
BasinObs2 <- BasinObs
colnames(BasinObs2) <- letters[seq_along(colnames(BasinObs2))]


# --------

test_that("ObsDF with custom column names", {
  expect_equal(
    object   = PrepGR(ObsDF = BasinObs, HydroModel = "GR4J", CemaNeige = FALSE),
    expected = PrepGR(ObsDF = BasinObs2, HydroModel = "GR4J", CemaNeige = FALSE),
  )
})

# ----

test_that("ObsDF is a tibble data.frame", {
  expect_equal(
    object   = PrepGR(ObsDF = BasinObs, HydroModel = "GR4J", CemaNeige = FALSE),
    expected = PrepGR(ObsDF = as_tibble(BasinObs), HydroModel = "GR4J", CemaNeige = FALSE),
  )
})
