#' @title Helper for rasterizing a PRODES shapefile.
#' PRODES's SHP into a TIF.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Rasterize the given PRODES shapfile to match the given
#'              reference raster. The resulting raster is encoded according to
#'              the given PRODES label key and the label deforesatation
#'              includes all the PRODES years until the given PRODES year.
#'
#' @param file_path_prodes A path to a PRODES shapefile.
#' @param file_path_res    A path to the results of a classification.
#' @param pyear            A PRODES year.
#' @param prodes_label_key A tibble with the mapping of the PRODES labels from
#'                         Portuguese to English and an ID.
#' @return                 A path to a raster file.
#' @export
prodes2raster_helper<- function(file_path_prodes, file_path_res, pyear,
                                prodes_label_key){

    class_name_filter <- c(paste0('d', 2000:as.integer(pyear)), "FLORESTA")
    pyear <- as.integer(pyear)
    stopifnot(c("label_pd_pt", "label_pd", "id_pd") %in%
              colnames(prodes_label_key))

    file_path_prodes %>%
        sf::read_sf() %>%
        dplyr::select(class_name, mainclass) %>%
        dplyr::filter(class_name %in% class_name_filter) %>%
        ensurer::ensure_that(nrow(.) > 0,
                             "FLORESTA" %in% unique(.$class_name),
                             err_des =  "Labels not found!") %>%
        dplyr::left_join(prodes_label_key,
                         by = c("mainclass" = "label_pd_pt")) %>%
        dplyr::select(id_pd) %>%
        vector2raster(raster_r = raster::raster(file_path_res),
                      vector_field = "id_pd",
                      raster_path = tempfile(pattern = "prodes2raster_",
                                             fileext = ".tif")) %>%
        return()
}
