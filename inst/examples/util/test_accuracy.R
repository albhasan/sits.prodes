library(dplyr)
library(sf)
library(raster)
library(gdalUtils)
library(devtools)

stop()
# VALIDATE MAPS DICOUNTING DEGRADES AND BURNED AREAS.
# THE IMPROVEMENTS ARE TOO SMALL TO BE SIGNIFICANT.
# The code takes too long to run.

# TODO: add code to validate_map.R

setwd("/home/alber/Documents/ghProjects/sits.prodes")
devtools::load_all()

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"

in_dir     <- "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_40/results_dl"
label_file <- "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_40/results_dl/int_labels.csv"
out_dir    <- "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_40/results_dl/validation_v3"
data(prodes_labels, package = "sits.prodes")

# Taken from sits.prodes
#' @title Rasterize vectors
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Rasterize a sf object to match the given raster
#'
#' @param vector_sf    A sf object.
#' @param raster_r     A raster object. This raster is the reference for coordinate sytem, spatial resolution, and dimensions.
#' @param vector_field A length-one character. The name of an integer column in vector_sf.
#' @param raster_path  A length-one character. An optional file path to store the resulting raster.
#' @return             A raster layer.
#' @export
vector2raster <- function(vector_sf, raster_r, vector_field,
                          raster_path = tempfile(pattern = "vector2raster",
                                                 fileext = ".tif")){
    stopifnot(lapply(vector_sf, class)[[vector_field]] == "integer")
    raster::writeRaster(raster::setValues(raster_r, NA),
                        filename = raster_path,
                        overwrite = TRUE,
                        options = c("BIGTIFF=YES"))
    tmp_vector_path <- tempfile(pattern = "vector2raster", fileext = ".shp")
    suppressWarnings(
        vector_sf %>%
            sr::st_transform(crs = raster::projection(raster_r, asText = TRUE)) %>%
            sf::st_write(dsn = tmp_vector_path,
                         delete_dsn = TRUE,
                         quiet = TRUE,
                         delete_layer = TRUE)
    )
    gdalUtils::gdal_rasterize(src_datasource = tmp_vector_path,
                              dst_filename = raster_path,
                              a = vector_field,
                              l = tools::file_path_sans_ext(basename(tmp_vector_path)),
                              output_Raster = TRUE) %>%
        .[[1]] %>% # Cast to raster layer, why? Because fuck you R! That's why
        return()
}

# Read, project, filter, and rasterize DETER to build a mask to exclude from PRODES.
build_deter_mask <- function(deter_file_path, r, pyear){
   end_date <- pyear %>%
        paste0("-08-31") %>%
        as.Date()
    antimask_path <- tempfile(pattern = "deter_antimask_", fileext = ".tif")
    deter <- deter_file_path %>%
        sf::read_sf(stringsAsFactors = FALSE) %>%
        dplyr::filter(CLASSNAME %in% c("CICATRIZ_DE_QUEIMADA", "DEGRADACAO")) %>%
        dplyr::filter(VIEW_DATE < end_date) %>%
        dplyr::mutate(deter = 1L) %>%
        dplyr::select(deter) %>%
        vector2raster(raster_r = r, vector_field = "deter") %>%
        return()
}

mask_deter <- function(r, deter_mask){
    return(raster::mask(r, mask = deter_mask, inverse = TRUE))
}

#---- Script ----


# key for encoding PRODES's SHP into a TIF
prodes_labels_ls <- prodes_labels %>%
  dplyr::pull(label_pd) %>%
  as.list() %>%
  magrittr::set_names(prodes_labels$label_pd_pt)

# get classification labels
labels_csv <- label_file %>%
  read.csv(stringsAsFactors = FALSE)
int_labels        <- labels_csv %>%
  dplyr::pull(Code) %>%
  magrittr::set_names(labels_csv$Label)
rm(labels_csv)

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
kv_ref_res <- dplyr::full_join(
  dplyr::tibble(key_ref = seq_along(unique_prodes_labels),
                label = unique_prodes_labels),
  dplyr::tibble(key_res = as.character(int_labels),
                label = names(int_labels)), by = "label") %>%
  ensurer::ensure_that(sum(rowSums(is.na(.)) == 0) > 0,
                       err_desc = "No match between the reference and result labels!")

# create the definitive key
available_keys <- kv_ref_res %>%
  dplyr::select(key_res) %>%
  tidyr::drop_na() %>%
  unlist() %>%
  setdiff(as.character(1:nrow(kv_ref_res)), .)
kv_ref_res <- kv_ref_res %>%
  dplyr::mutate(key = key_res)
kv_ref_res$key[is.na(kv_ref_res$key)] <- available_keys
kv_ref_res <- kv_ref_res %>%
  dplyr::arrange(key) %>%
  ensurer::ensure_that(sum(is.na(kv_ref_res$key)) == 0,
                       err_desc = "There are missing keys!")
key_labels <- kv_ref_res %>%
  dplyr::select(label) %>%
  unlist() %>%
  as.list() %>%
  magrittr::set_names(dplyr::pull(kv_ref_res, key))
key_labels_rev <- key_labels %>%
  names() %>%
  as.list() %>%
  magrittr::set_names(as.vector(unlist(key_labels)))
rm(available_keys, kv_ref_res)
stopifnot(all(key_labels == names(key_labels_rev)),
          all(names(key_labels) == key_labels_rev))

# Create a CSV with the confusion keys.
key_tb <- tibble::tibble(label = unlist(key_labels),
                         id_label = as.integer(names(key_labels)))
key_tb %>%
  dplyr::rename(label_ref = "label", id_ref = "id_label") %>%
  tidyr::crossing(.$label_ref) %>%
  dplyr::rename(label_res = `.$label_ref`) %>%
  dplyr::left_join(key_tb, by = c("label_res" = "label")) %>%
  dplyr::rename(id_res = id_label) %>%
  dplyr::mutate(id_confusion = (id_ref * 100) + id_res) %>%
  write.csv(file = file.path(out_dir, "confusion_key.csv"))

deter_maps <- base_path %>%
    file.path("data", "vector", "deter", "tiled") %>%
    list.files(pattern = "deter_[0-9]+[.]shp$", full.names = TRUE) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(deter_file_path = "value") %>%
    dplyr::mutate(file_name = tools::file_path_sans_ext(basename(deter_file_path))) %>%
    tidyr::separate(col = file_name, into = c(NA, "scene"))

prodes_maps <- base_path %>%
  file.path("data", "vector","prodes", "tiled") %>%
  list.files(pattern = "PDigital2017_AMZ_pol_[0-9]{3}_[0-9]{3}[.]shp",
             full.names = TRUE) %>%
  tibble::enframe(name = NULL) %>%
  dplyr::rename(file_path = "value") %>%
  dplyr::mutate(scene = file_path %>%
                  basename() %>%
                  stringr::str_extract(pattern = "_[0-9]{3}_[0-9]{3}[.]") %>%
                  stringr::str_sub(2, -2) %>%
                  stringr::str_replace('_', '')) %>%
  ensurer::ensure_that(nrow(.) > 0, err_desc = "Prodes maps not found.")

validation_tb <- "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_40/results_dl/validation_v2/validation_tb.rds" %>%
    readRDS() %>%
    #dplyr::select(scene, pyear, out_res, validation_data) %>%
    #-------
    # TODO: Include older data from DETER
    #dplyr::filter(pyear %in% as.character(2015:2019)) %>%
    dplyr::filter(pyear == "2017") %>%
    #-------
    dplyr::left_join(deter_maps, by = "scene") %>%
    #dplyr::rename(file_path = out_res) %>%
    dplyr::mutate(r = purrr::map(out_res, raster::raster)) %>%
    dplyr::mutate(deter_mask = purrr::pmap(dplyr::select(., deter_file_path, r, pyear),
                                   build_deter_mask)) %>%
    # Mask classification results using DETER
    dplyr::mutate(r_masked = purrr::map2(r, deter_mask, mask_deter))

validation_tb <- validation_tb %>%
    # Rasterize PRODES to match the result map.
    dplyr::mutate(prodes_rasterized = purrr::pmap_chr(
        dplyr::select(., file_pd = file_path_prodes, file_rt = file_path_res,
                      tile = scene, year_pd = pyear),
        function(file_pd, file_rt, tile, year_pd, prodes_lbl){
            raster_path = tempfile(pattern = "prodes2raster_", fileext = ".tif")
            prodes2raster(file_pd = file_pd, file_rt = file_rt,
                          raster_path = raster_path, tile = tile,
                          year_pd = year_pd, prodes_lbl = prodes_lbl)
        },
        prodes_lbl = prodes_labels %>%
            dplyr::filter(label_pd %in% names(int_labels))))

validation_tb <- validation_tb %>%
    # Build a reference-result data.frame.
    dplyr::mutate(validation_data_new = purrr::pmap(
        dplyr::select(., prodes_rasterized, r_masked),
        function(prodes_rasterized, r_masked, key_labels, prodes_labels_ls){
            if (any(vapply(c(prodes_rasterized), is.na, logical(1))))
                return(NA)
            r_res <- r_masked
            r_prodes <- prodes_rasterized %>%
                raster::raster() %>%
                raster::projectRaster(crs = raster::crs(r_res, asText = TRUE),
                                      method = "ngb") %>%
                raster::resample(y = r_res, method = "ngb") %>%
                raster::crop(y = raster::extent(r_res))
            ref_res_df <- raster::stack(r_prodes, r_res, quick = FALSE)[] %>%
                as.data.frame() %>%
                tidyr::drop_na() %>%
                dplyr::rename("lab_ref_num" = 1, "lab_res_num" = 2) %>%
                dplyr::mutate(lab_ref = dplyr::recode(lab_ref_num,
                                                      !!!key_labels),
                              lab_res = dplyr::recode(lab_res_num,
                                                      !!!key_labels)) %>%
                dplyr::filter(lab_res %in% unlist(prodes_labels_ls))
            flevels <- ref_res_df %>%
                dplyr::select(lab_ref, lab_res) %>%
                unlist() %>%
                unique()
            if (length(flevels) < 2) {
                warning("Found less than 2 labels!")
                return(NA)
            }
            con_mat <- caret::confusionMatrix(data = factor(ref_res_df[["lab_res"]],
                                                            flevels),
                                              reference = factor(ref_res_df[["lab_ref"]],
                                                                 flevels))
            # computing reference areas
            lab_ref <- NULL
            ref_areas <- cov_get_areas(r_prodes, label = lab_ref) %>%
                dplyr::mutate(lab_ref = dplyr::recode(label, !!!key_labels)) %>%
                tidyr::drop_na()
            rm(r_res, r_prodes, ref_res_df)
            # asses accuracy
            class_areas <- as.vector(ref_areas$area)
            names(class_areas) <- ref_areas$lab_ref
            asses_accuracy_area(error_matrix = as.matrix(as.data.frame.matrix(con_mat$table)),
                                class_areas) %>%
                return()
        }, key_labels = key_labels, prodes_labels_ls = prodes_labels_ls))




# > validation_tb$validation_data[[1]]$accuracy
# $overall
# [1] 0.993866
#
# $user
# forest deforestation
# 0.9994030     0.1321973
#
# $producer
# forest deforestation
# 0.9944512     0.5872641
#
# > validation_tb$validation_data_new[[1]]$accuracy
# $overall
# [1] 0.9938508
#
# $user
# forest deforestation
# 0.9994149     0.1279546
#
# $producer
# forest deforestation
# 0.9944243     0.5842497
#
# > validation_tb$validation_data[[2]]$accuracy
# $overall
# [1] 0.9942855
#
# $user
# forest deforestation
# 0.9991595     0.1399099
#
# $producer
# forest deforestation
# 0.9951132     0.4870932
#
# > validation_tb$validation_data_new[[2]]$accuracy
# $overall
# [1] 0.9943526
#
# $user
# forest deforestation
# 0.9992629     0.1336273
#
# $producer
# forest deforestation
# 0.9950782     0.5083962
#
# > validation_tb$validation_data[[3]]$accuracy
# $overall
# [1] 0.9810424
#
# $user
# forest deforestation
# 0.9969592     0.2358307
#
# $producer
# forest deforestation
# 0.9838922     0.6235665
#
# > validation_tb$validation_data_new[[3]]$accuracy
# $overall
# [1] 0.9811134
#
# $user
# forest deforestation
# 0.9970882     0.2331866
#
# $producer
# forest deforestation
# 0.9838394     0.6310622

