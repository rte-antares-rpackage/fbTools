context("Function getVertices")

test_that("getVertices", {
  library(data.table)
  library(testthat)
  library(parallel)

  DT <- getFbDomain(list.files(system.file("testdata/FB",package = "fbTools"),
                             recursive = TRUE, full.names = TRUE), 2)
  DT <- DT[presolve == TRUE]
  VT <- getVertices(DT)
  expect_true("ptdfAT" %in% names(VT))
  expect_true("ptdfBE" %in% names(VT))
  expect_true("ptdfDE" %in% names(VT))
  expect_true("ptdfFR" %in% names(VT))


  VT2 <- getVertices(DT, ctrdel = "FR")
  expect_true("ptdfAT" %in% names(VT2))
  expect_true("ptdfBE" %in% names(VT2))
  expect_true("ptdfDE" %in% names(VT2))
  expect_true("ptdfNL" %in% names(VT2))


})



