library(testthat)

context('functions')
test_that("Plot returns 10 cities & 20 bars",{
  p <- plotCity()
  prow <- as.data.frame(p$data)
  expect_equal(nrow(prow), 20)
})

test_that("Plot returns 5 Provinces & 10 bars",{
  p <- plotProvinces()
  prow <- as.data.frame(p$data)
  expect_equal(nrow(prow), 10)
})

test_that("Plot returns 2 Divisions & 4 bars",{
  filterDivision <- c("Faisalabad","Lahore")
  p <- plotDivisions(filterDivision)
  prow <- as.data.frame(p$data)
  expect_equal(nrow(prow), 4)
})

test_that("Plot returns 2 Districtss & 4 bars",{
  sampleData <- c("Faisalabad","Lahore")
  p <- plotDistricts(sampleData)
  prow <- as.data.frame(p$data)
  expect_equal(nrow(prow), 4)
})

test_that("Plot returns 2 Tehsils & 4 bars",{
  sampleData <- c("Chiniot","Lalian")
  p <- plotTehsils(sampleData)
  prow <- as.data.frame(p$data)
  expect_equal(nrow(prow), 4)
})
