sits.env <- new.env()
utils::globalVariables(c(
    ".",
    "%>%",
    "area",
    "band",
    "blue",
    "count",
    "cov",
    "cov_r",
    "dif_time",
    "end_date",
    "GDALinfo",
    "id",
    "label",
    "label_id",
    "label_ref",
    "label_res",
    "latitude",
    "longitude",
    "lm",
    "nir",
    "red",
    "runif",
    "path",
    "start_date",
    "swir2",
    "sum",
    "time_serie",
    "time_series",
    "tmp_area",
    "tmp_label",
    "value",
    "year",
    "zone"
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
