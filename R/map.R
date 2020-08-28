#' @title Process the classified map labels. Build the key for encoding
#' PRODES's SHP into a TIF.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Process the classified map labels.
#'
#' @param label_file A path to a CSV file with the labels and codes in in the results of the classification.
#' @return           A tibble with of PRODES' labels, their reference codes, and their codes in the classification results.
#' @export
get_prodes_map_key <- function(label_file){
    # label_file <- "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_40/results_dl/int_labels.csv"
    data(prodes_labels, package = "sits.prodes")

    # Build a list for dplyr::recode
    prodes_labels_ls <- prodes_labels %>%
      dplyr::pull(label_pd) %>%
      as.list() %>%
      magrittr::set_names(prodes_labels$label_pd_pt)

    # get classification labels
    labels_csv <- label_file %>%
      read.csv(stringsAsFactors = FALSE)
    int_labels <- labels_csv %>%
      dplyr::pull(Code) %>%
      magrittr::set_names(labels_csv$Label)

    # when the classes are clustered
    if (sum(stringr::str_detect(names(int_labels), "_[0-9]+$")) > 0) {
      names(int_labels) <- names(int_labels) %>%
        stringr::str_replace_all(pattern = "_[0-9]+$", replacement = "")
    }

    # match reference and results keys
    unique_prodes_labels <- prodes_labels_ls %>%
      unlist() %>%
      unique() %>%
      sort()

    dplyr::full_join(
      dplyr::tibble(key_ref = seq_along(unique_prodes_labels),
                    label = unique_prodes_labels),
      dplyr::tibble(key_res = as.character(int_labels),
                    label = names(int_labels)), by = "label") %>%
      ensurer::ensure_that(sum(rowSums(is.na(.)) == 0) > 0,
                           err_desc = "No match between the reference and result labels!") %>%
      return()
}


#' @title Get the water masks.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get the water masks.
#'
#' @param dir_path A length-one character. A path to a directory of tif filles
#' @return         A tibble.
#' @export
get_water_masks <- function(dir_path){
    dir_path %>%
        list.files(pattern = "extent_[0-9]{6}[.]tif$", full.names = TRUE) %>%
        ensurer::ensure_that(all(is_raster_valid(.)),
            err_desc = "Invalid water masks.") %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename(file_path_water = "value") %>%
        dplyr::mutate(scene = file_path_water %>%
                          basename() %>%
                          stringr::str_extract(pattern = "_[0-9]{6}[.]") %>%
                          stringr::str_sub(2, -2)) %>%
        ensurer::ensure_that(nrow(.) > 0,
                             err_desc = "Water masks not found.") %>%
        return()
}


#' @title Get the corner masks.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get the water masks.
#'
#' @param dir_path A length-one character. A path to a directory of tif filles
#' @return         A tibble.
#' @export
get_corner_masks <- function(dir_path){
    dir_path %>%
        list.files(pattern = "LC08_CORNERMASK_[0-9]{6}.tif",
                   full.names = TRUE) %>%
        ensurer::ensure_that(all(is_raster_valid(.)),
            err_desc = "Invalid corner masks.") %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename(file_path = "value") %>%
        dplyr::mutate(scene = file_path %>%
                        basename() %>%
                        stringr::str_extract(pattern = "_[0-9]{6}[.]") %>%
                        stringr::str_sub(2, -2)) %>%
        ensurer::ensure_that(nrow(.) > 0,
                             err_desc = "Corner masks not found.") %>%
        return()
}


#' @title Get the cloud quantile maps.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get the cloud quantile maps.
#'
#' @param dir_path A length-one character. A path to a directory of tif filles
#' @return         A tibble.
#' @export
get_quantile_maps <- function(dir_path){
    dir_path %>%
        list.files(pattern = "*quantiles.tif", full.names = TRUE) %>%
        ensurer::ensure_that(all(is_raster_valid(.)),
                             err_desc = "Invalid quantile maps.") %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename(file_path = "value") %>%
        dplyr::mutate(scene = file_path %>%
                        basename() %>%
                        stringr::str_extract(pattern = "_[0-9]{6}_") %>%
                        stringr::str_sub(2, -2),
                      pyear = file_path %>%
                        basename() %>%
                        stringr::str_extract(pattern = "_[0-9]{4}_") %>%
                        stringr::str_sub(2, -2)) %>%
        ensurer::ensure_that(nrow(.) > 0,
                             err_desc = "Quantile maps not found.") %>%
        return()
}


#' @title Get the PRODES maps.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get the PRODES maps.
#'
#' @param dir_path A length-one character. A path to a directory of shapefilles.
#' @return         A tibble.
#' @export
get_prodes_maps <- function(dir_path){
    dir_path %>%
        list.files(pattern = "PDigital2017_AMZ_pol_[0-9]{3}_[0-9]{3}[.]shp",
            full.names = TRUE) %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename(file_path_prodes = "value") %>%
        dplyr::mutate(scene = file_path_prodes %>%
                        basename() %>%
                        stringr::str_extract(pattern = "_[0-9]{3}_[0-9]{3}[.]") %>%
                        stringr::str_sub(2, -2) %>%
                        stringr::str_replace('_', '')) %>%
        ensurer::ensure_that(nrow(.) > 0,
                             err_desc = "Prodes maps not found.") %>%
        return()
}


#' @title Get the DETER maps.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get the DETER maps.
#'
#' @param dir_path A length-one character. A path to a directory of shapefilles.
#' @return         A tibble.
#' @export
get_deter_maps <- function(dir_path){
    dir_path %>%
        list.files(pattern = "deter_[0-9]+[.]shp$", full.names = TRUE) %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename(file_path_deter = "value") %>%
        dplyr::mutate(file_name = tools::file_path_sans_ext(basename(file_path_deter))) %>%
        tidyr::separate(col = file_name, into = c(NA, "scene")) %>%
        return()
}


#' @title Get the maps resulting from a classification.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get the maps resulting from a classification.
#'
#' @param dir_path A length-one character. A path to a directory of tif filles
#' @return         A tibble.
#' @export
get_classified_maps <- function(dir_path){
    res_map_pattern <- "^l8_(simple|maskcloud)_[0-9]{6}_[0-9]{4}_(dl|rf|svm|dl-rf-svm)_[0-9]{4}_[0-9]_[0-9]{4}_[0-9](.tif|_vote.tif)"
    dir_path %>%
        list.files(pattern = res_map_pattern, full.names = TRUE,
                   include.dirs = FALSE) %>%
        ensurer::ensure_that(length(.) > 0,
                             err_desc = sprintf("No classified images found at %s",
                                                in_dir)) %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename(file_path_res = "value") %>%
        dplyr::mutate(scene = file_path_res %>%
                        basename() %>%
                        stringr::str_extract(pattern = "_[0-9]{6}_") %>%
                        stringr::str_sub(2, -2),
                      pyear = file_path_res %>%
                        basename() %>%
                        stringr::str_extract_all(pattern = "_[0-9]{4}_") %>%
                        vapply(., dplyr::last, character(1)) %>%
                        stringr::str_sub(2, -2)) %>%
        return()
}

