#!/usr/bin/Rscript

# validate the results of a classification using MAPBIOMAS

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ensurer))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(sits.prodes))

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"
mapbiomas_path <- file.path(base_path, c("data/raster/mapbiomas/amazonia/mapbiomas_tiled"))
mapbiomas_pattern = "^Classification_[0-9]{4}_[0-9]{5}.tif$"
res_pattern <- "^l8_(simple|maskcloud)_[0-9]{6}_[0-9]{4}_(dl|rf|svm)_[0-9]{4}_[0-9]_[0-9]{4}_[0-9]_masked.tif"
img_dtype = "INT4S" # data type for rasterization using the raster package
#img_type = "Int32"  # data type for rasterization using gdal_rasterize

data("prodes_labels", package = "sits.prodes")
data("mapbiomas_labels", package = "sits.prodes")
stopifnot(exists(c("prodes_labels", "mapbiomas_labels")))

in_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_40/results_dl"

# get mapbiomas
mapbiomas_files <- mapbiomas_path %>%
    list.files(full.names = TRUE, pattern = "tif$") %>%
    tibble::enframe(name = NULL, value = "file_path_mb") %>%
    dplyr::mutate(
        tile = file_path_mb %>% basename() %>%
            stringr::str_extract(pattern = "_[0-9]{5}\\.") %>%
            stringr::str_sub(2, -2) %>%
            prodes2scene(),
        pyear = file_path_mb %>% basename() %>%
            stringr::str_extract(pattern = "_[0-9]{4}_") %>%
            stringr::str_sub(2, -2)
    ) %>% ensurer::ensure_that(all(nchar(.$tile) == 6), all(nchar(.$pyear) == 4),
                               all(castable2numeric(.$tile)),
                               all(castable2numeric(.$pyear)),
                               nrow(dplyr::distinct(., tile, pyear)) == nrow(.))

# build a reclassification matrix for mapbiomas
prodes_lbl <- prodes_labels %>% dplyr::full_join(mapbiomas_labels, by = "label_pd") %>%
    ensurer::ensure_that(!all(sapply(., is.na)),
                         all.equal(as.vector(table(.$label_pd)),
                                   as.vector(table(.$id_pd))))
rcl_mat <- prodes_lbl %>% dplyr::select(id_mb, id_pd) %>% as.matrix()

# get classification files, join to mapbiomas, and reclassify the latter
res_files <- in_dir %>% 
    list.files(pattern = res_pattern, full.names = TRUE, include.dirs = FALSE) %>%
    ensurer::ensure_that(length(.) > 0, err_desc = sprintf("No classified images found at %s", in_dir)) %>%
    tibble::enframe(name = NULL, value = "file_path_res") %>%
    dplyr::mutate(
        tile = file_path_res %>% basename() %>%
            stringr::str_extract(pattern = "_[0-9]{6}_") %>%
            stringr::str_sub(2, -2),
        pyear = file_path_res %>% basename() %>%
            stringr::str_extract_all(pattern = "_[0-9]{4}_") %>%
            vapply(dplyr::last, character(1)) %>%
            stringr::str_sub(2, -2)
    ) %>% ensurer::ensure_that(all(nchar(.$tile) == 6), all(nchar(.$pyear) == 4),
                               all(castable2numeric(.$tile)),
                               all(castable2numeric(.$pyear)),
                               nrow( dplyr::distinct(., tile, pyear)) == nrow(.)
    ) %>%
    dplyr::left_join(mapbiomas_files, by = c("tile", "pyear")) %>%
    # re-code mapbiomas
    dplyr::mutate(mb_reclass = purrr::map_chr(.$file_path_mb, function(file_path){
        reclas_file <- file_path %>% basename() %>% tools::file_path_sans_ext()
        out_file <- tempfile(pattern = paste0(reclas_file, '_'), fileext = ".tif")
        file_path %>% raster::raster() %>%
            raster::reclassify(rcl = rcl_mat, filename = out_file,
                               overwrite = TRUE, datatype = img_dtype)
        return(out_file)
    }))

# TODO: ensure the match between raster codes
key_tb <- prodes_lbl %>% dplyr::select(label_pd, id_pd) %>%
    dplyr::group_by(label_pd) %>%
    dplyr::slice(1)
key_pd        <- key_tb %>% dplyr::pull(label_pd) %>% as.list()
names(key_pd) <- key_tb %>% dplyr::pull(id_pd)

# stack raster
res_files$conmat <- purrr::map2(res_files$mb_reclass,
                                   res_files$file_path_res,
                                   confusion_raster,
                                   key_ls = key_pd)



# compute confusion matrix

