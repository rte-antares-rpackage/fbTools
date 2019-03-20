context("Function getCnesFb")

test_that("getCnesFb", {
  library(data.table)
  library(testthat)
  library(parallel)

  DT <- getCnesFb(list.files(system.file("testdata/FB",package = "fbTools"),
                       recursive = TRUE, full.names = TRUE), 2)

  expect_true("data.table"%in%class(DT))
  expect_true(nrow(DT)==19655)
})



