sits.env <- new.env()
utils::globalVariables(c(
    ".",
    "%>%",
    "blue",
    "lm",
    "nir",
    "red",
    "runif",
    "swir2"
))


#' Pipe
#'
#' Magrittr compound assignment pipe-operator.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @param lhs,rhs A visualisation and a function to apply to it.
#' @export
NULL
