context("rAPI internals")
source("../../R/rAPI.R")

test_that("rAPI remembers the base url it was initialized with", {
  baseUrl <- "http://api.ccexchange.com/public"
  rAPI <- init_rAPI(baseUrl = baseUrl)
  expect_equal(rAPI$base, baseUrl)
})

test_that("rAPI$shared$getCallingFunctionsName, returns the name of the calling function", {
  rAPI <- init_rAPI(baseUrl = "")
  testFunctionName <- function() {
    returnedFunctionName <- rAPI$shared$getCallingFunctionsName()
    expect_equal(returnedFunctionName, "testFunctionName")
  }
  testFunctionName()
})
