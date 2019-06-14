context("prodes")
library(sits.prodes)

test_that("convert PRODES to Landsat scene ids", {
    expect_true(is.na(prodes2scene(NA)))
    expect_true(is.na(prodes2scene(NULL)))
    expect_equal(prodes2scene("23367"), "233067")
    expect_equal(as.vector(prodes2scene(c("23367", "23367"))), c("233067", "233067"))
})

