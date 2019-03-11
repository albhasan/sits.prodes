#!/usr/bin/Rscript

# rasterize PRODES' shapefiles
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ensurer))
suppressPackageStartupMessages(library(optparse))

library(sits.prodes)

# TODO: remove
setwd("/home/alber/Documents/data/experiments/prodes_reproduction/Rpackage/sits.prodes")
library(devtools)
devtools::load_all()
# - - - 

base_path  <- "/home/alber/Documents/data/experiments/prodes_reproduction"
in_dir     <- "/net/150.163.2.206/disks/d6/shared/alber/prodes_reproduction/03_classify/rep_prodes_40/results_vote/smooth_3x3_n10"
out_dir    <- "/home/alber/shared/prodes_raster_2014-2017"
label_file <- "/net/150.163.2.206/disks/d6/shared/alber/prodes_reproduction/03_classify/rep_prodes_40/results_dl/int_labels.csv"
img_pattern <- "^l8_(simple|maskcloud)_[0-9]{6}_[0-9]{4}_dl-rf-svm_[0-9]{4}_[0-9]_2017_[0-9]_vote.tif"

stopifnot(vapply(c(base_path, in_dir, out_dir), dir.exists, logical(1)))
stopifnot(file.exists(label_file))

# key for encoding PRODES's SHP into a TIF
prodes_labels <- list(
    DESMATAMENTO  = "deforestation",
    RESIDUO       = "deforestation",
    FLORESTA      = "forest",
    NAO_FLORESTA  = "no forest",
    NAO_FLORESTA2 = "no forest",
    HIDROGRAFIA   = "water"
)

prodes_maps <- c(
    "225063" = file.path(base_path, "data/vector/prodes_tiled/prodes_225_063.shp"),
    "226064" = file.path(base_path, "data/vector/prodes_tiled/prodes_226_064.shp"),
    "232066" = file.path(base_path, "data/vector/prodes_tiled/prodes_232_066.shp"),
    "233067" = file.path(base_path, "data/vector/prodes_tiled/prodes_233_067.shp")
)

# get classification labels
labels_csv <- label_file %>%
    read.csv(stringsAsFactors = FALSE)
int_labels        <- labels_csv$Code
names(int_labels) <- labels_csv$Label
rm(labels_csv)

# match reference and results keys
unique_prodes_labels <- prodes_labels %>% unlist() %>% unique() %>% sort()
kv_ref_res <- dplyr::full_join(
    dplyr::tibble(key_ref = seq_along(unique_prodes_labels),
                  label = unique_prodes_labels),
    dplyr::tibble(key_res = as.character(int_labels),
                  label = names(int_labels)),by = "label") %>%
    ensurer::ensure_that(sum(rowSums(is.na(.)) == 0) > 0,
                         err_desc = "No match between the reference and result labels!")

# create the definitive key
available_keys <- kv_ref_res %>% dplyr::select(key_res) %>% tidyr::drop_na() %>%
    unlist() %>% setdiff(as.character(1:nrow(kv_ref_res)), .)
kv_ref_res <- kv_ref_res %>% dplyr::mutate(key = key_res)
kv_ref_res$key[is.na(kv_ref_res$key)] <- available_keys
kv_ref_res <- kv_ref_res %>% dplyr::arrange(key) %>%
    ensurer::ensure_that(sum(is.na(kv_ref_res$key)) == 0,
                         err_desc = "There are missing keys!")
key_labels <- kv_ref_res %>% dplyr::select(label) %>% unlist() %>% as.list()
names(key_labels) <- kv_ref_res %>% dplyr::select(key) %>% unlist()
key_labels_rev <- key_labels %>% names() %>% as.list()
names(key_labels_rev) <- key_labels %>% unlist() %>% as.vector()
rm(available_keys, kv_ref_res)

# get raster templates
path_res_vec <- in_dir %>%
    list.files(pattern = img_pattern, full.names = TRUE,
               include.dirs = FALSE) %>%
    ensurer::ensure_that(length(.) > 0, err_desc = sprintf("No classified images found at %s", in_dir))

# rasterize PRODES
for(res_file in path_res_vec){
    pyear <- 2014:2017 
    scene <- res_file %>% basename() %>%
        stringr::str_extract(pattern = "_[0-9]{6}_") %>%
        stringr::str_sub(2, -2) %>%
        ensurer::ensure_that(nchar(.) == 6, err_desc = "Invalid scene identifier")
    cov_res <- cov_read(res_file)
    out_file <- file.path(out_dir, sprintf("prodes_%s.tif", scene))
    print(sprintf("Rasterizing PRODES to %s", out_file))
    prodes_rasterize(ref_path = prodes_maps[scene],
                                pyear = pyear, cov_res = cov_res,
                                level_key_pt = prodes_labels,
                                level_key = key_labels_rev) %>%
        ensurer::ensure_that(!is.null(.), err_desc = "Rasterization failed!") %>%
        raster::writeRaster(filename = out_file, format = "GTiff", datatype = "INT4S")
}


