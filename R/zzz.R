sits.env <- new.env()
utils::globalVariables(c(
    ".",
    "%>%",
    "ano",
    "area",
    "band",
    "blue",
    "class_name",
    "class_name_filter",
    "count",
    "cov",
    "cov_r",
    "desc",
    "dif_time",
    "end_date",
    "GDALinfo",
    "id",
    "lab_ref",
    "lab_ref_pt",
    "label",
    "label_id",
    "label_ref",
    "label_res",
    "latitude",
    "longitude",
    "lm",
    "mainclass",
    "nir",
    "red",
    "runif",
    "path",
    "sample_id",
    "start_date",
    "swir2",
    "sum",
    "time_serie",
    "time_series",
    "tmp_area",
    "tmp_label",
    "value",
    "valid",
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
