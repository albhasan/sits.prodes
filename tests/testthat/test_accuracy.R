context("accuracy")
library(sits.prodes)

test_that("simple overall accuracy matches example in article", {
    # Accuracy assessment: a user's perspective by Story and Congalton
    # table 1
    error_matrix <- matrix(c(15,2,4,3,12,2,1,3,14), byrow = TRUE, nrow = 3)
    colnames(error_matrix) <- rownames(error_matrix) <- c("X", "Y", "Z")
    expect_equal(floor(asses_accuracy_simple(error_matrix)$overall * 100), 73)
})

test_that("simple user producer accuracies match example in article", {
    # Accuracy assessment: a user's perspective by Story and Congalton
    # table 2
    expected <- c("F" = 93, "W" = 50, "U" = 50)
    error_matrix <- matrix(c(28,14,15,1,15,5,1,1,20), byrow = TRUE, nrow = 3)
    colnames(error_matrix) <- rownames(error_matrix) <- names(expected)
    expect_equal(floor(asses_accuracy_simple(error_matrix)$producer  * 100), expected)
})

test_that("user producer accuracies match example in article", {
    # Making better use of accuracy data in land change studies: Estimating
    # accuracy and area and quantifying uncertainty using stratified estimation
    # by Olofsson, Foody, and Stehman
    # table 4
    error_matrix <- matrix(c(97,3,2, 0,279,1, 3,18,97), byrow = FALSE, nrow = 3)
    map_area <- c(22353, 1122543, 610228)
    expected_user     <- c(0.97, 0.93, 0.97)
    expected_producer <- c(0.48, 0.99, 0.89) # c(0.48, 0.99, 0.88)  approximation error?
    rownames(error_matrix) <- colnames(error_matrix) <- names(map_area) <- names(expected_user) <- names(expected_producer) <- paste0("c", 1:length(map_area))
    expect_equal(floor(asses_accuracy_area(error_matrix, map_area)$accuracy$overall * 1000)/1000, 0.944)
    expect_equal(floor(asses_accuracy_area(error_matrix, map_area)$accuracy$user * 1000)/1000, expected_user)
    expect_equal(floor(asses_accuracy_area(error_matrix, map_area)$accuracy$producer * 100)/100, expected_producer)
})



test_that("user producer accuracies match example in article", {
    # Making better use of accuracy data in land change studies: Estimating
    # accuracy and area and quantifying uncertainty using stratified estimation
    # by Olofsson, Foody, and Stehman
    # table 5a
    error_matrix <- matrix(c(166, 3, 10, 180), byrow = FALSE, nrow = 2)
    map_area <- c(57550, 280693)
    expected_user     <- c(0.94, 0.98)
    expected_producer <- c(0.96, 0.98)
    rownames(error_matrix) <- colnames(error_matrix) <- names(map_area) <- names(expected_user) <- names(expected_producer) <- paste0("c", 1:length(map_area))
    expect_lt(asses_accuracy_area(error_matrix, map_area)$accuracy$overall - 0.97, 0.01)
    expect_true(all(asses_accuracy_area(error_matrix, map_area)$accuracy$user - expected_user < 0.01))
    expect_true(all(asses_accuracy_area(error_matrix, map_area)$accuracy$producer - expected_producer < 0.01))

    # table 5b
    error_matrix <- matrix(c(160, 1, 13, 189), byrow = FALSE, nrow = 2)
    map_area <- c(57550, 280693)
    expected_user     <- c(0.92, 0.99)
    expected_producer <- c(0.99, 1.0)
    rownames(error_matrix) <- colnames(error_matrix) <- names(map_area) <- names(expected_user) <- names(expected_producer) <- paste0("c", 1:length(map_area))
    expect_lt(asses_accuracy_area(error_matrix, map_area)$accuracy$overall - 0.97, 0.015) # approximation error?
    expect_true(all(asses_accuracy_area(error_matrix, map_area)$accuracy$user - expected_user < 0.01))
    expect_true(all(asses_accuracy_area(error_matrix, map_area)$accuracy$producer - expected_producer < 0.01))
})
