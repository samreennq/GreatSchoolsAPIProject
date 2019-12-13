library(testthat)
library(GreatSchoolsAPIProject)
context("Browsing Schools")

test_that("Browse.Schools is going to genera a dataframe of schools", {
  expect_equal(Browse.Schools("NY", "Albany", Sys.getenv("GreatSchoolsAPI")), Browse.Schools.NY)
})
