#!/usr/bin/Rscript

# validate the result of a classification using PRODES

suppressPackageStartupMessages(library(optparse))

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"
stopifnot(dir.exists(base_path))

# get arguments ----
option_list = list(
  make_option("--in_dir",     type = "character", default = NULL, help = "Path to a directory with classification results.", metavar="character"),
  make_option("--label_file", type = "character", default = NULL, help = "Path to a csv file detailing the labels in the images.", metavar="character"),
  make_option("--out_dir",    type = "character", default = NULL, help = "Path to a directory where to store the validation results.", metavar="character")
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)
if (length(opt) != 4 || sum(sapply(opt, is.null)) != 0){
  print_help(opt_parser)
  stop("Wrong arguments!")
}
in_dir     <- opt$in_dir     # /home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_40/results_dl/smooth_5x5_n10
label_file <- opt$label_file # /home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_40/results_dl/int_labels.csv 
out_dir    <- opt$out_dir    # /home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_40/results_dl/smooth_5x5_n10/validation

if (!file.exists(label_file)) {
    print_help(opt_parser)
    stop("File not found!")
}
if (!all(vapply(c(in_dir), dir.exists, logical(1)))) {
    print_help(opt_parser)
    stop(sprintf("Directories not found %s", paste(c(in_dir), collapse = " ")))
}

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ensurer))
suppressPackageStartupMessages(library(sits.prodes))

data(prodes_labels, package = "sits.prodes")

print(sprintf("Starting to process %s using th labels at %s ", in_dir, label_file))

# key for encoding PRODES's SHP into a TIF
prodes_labels_ls <- prodes_labels %>% dplyr::pull(label_pd) %>% as.list()
names(prodes_labels_ls) <- prodes_labels %>% dplyr::pull(label_pd_pt) 

# masks
water_masks <- c(
    "225063" = file.path(base_path, "data/raster/mascaras/surface_water-Pekel_et_al_2016/tiled/extent_225063.tif"),
    "226064" = file.path(base_path, "data/raster/mascaras/surface_water-Pekel_et_al_2016/tiled/extent_226064.tif"),
    "233067" = file.path(base_path, "data/raster/mascaras/surface_water-Pekel_et_al_2016/tiled/extent_233067.tif")
)
prodes_maps <- c(
    "225063" = file.path(base_path, "data/vector/prodes/prodes_tiled/PDigital2017_AMZ_pol_225_063.shp"),
    "226064" = file.path(base_path, "data/vector/prodes/prodes_tiled/PDigital2017_AMZ_pol_226_064.shp"),
    "233067" = file.path(base_path, "data/vector/prodes/prodes_tiled/PDigital2017_AMZ_pol_233_067.shp")
)
corner_masks <- base_path %>% file.path("data", "raster", "mask_l8_corner") %>% 
    list.files(pattern = "LC08_CORNERMASK_[0-9]{6}.tif", full.names = TRUE) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(file_path = "value") %>%
    dplyr::mutate(fname = basename(file_path), 
                  scene   = stringr::str_extract(fname, pattern = "_[0-9]{6}[.]")   %>% stringr::str_sub(2, -2))
stopifnot(all(vapply(water_masks, file.exists, logical(1))))
stopifnot(all(vapply(prodes_maps, file.exists, logical(1))))

# get classification labels
labels_csv <- label_file %>%
    read.csv(stringsAsFactors = FALSE)
int_labels        <- labels_csv$Code
names(int_labels) <- labels_csv$Label
rm(labels_csv)

# when the classes are clustered
if(sum(stringr::str_detect(names(int_labels), "_[0-9]+$")) > 0){
    names(int_labels) <- names(int_labels) %>%
        stringr::str_replace_all(pattern = "_[0-9]+$", replacement = "")
}

# match reference and results keys
unique_prodes_labels <- prodes_labels_ls %>% unlist() %>% unique() %>% sort()
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
stopifnot(all(key_labels == names(key_labels_rev)),
          all(names(key_labels) == key_labels_rev))

img_pattern <- "^l8_(simple|maskcloud)_[0-9]{6}_[0-9]{4}_(dl|rf|svm|dl-rf-svm)_[0-9]{4}_[0-9]_[0-9]{4}_[0-9](.tif|_vote.tif)"
path_res_vec <- in_dir %>% 
    list.files(pattern = img_pattern, full.names = TRUE,
               include.dirs = FALSE) %>%
    ensurer::ensure_that(length(.) > 0, err_desc = sprintf("No classified images found at %s", in_dir))

# loop each file resulting from classification or smoothing
res_acc <- purrr::map(path_res_vec, function(res_file, out_dir = NULL){
    # handle directories
    if (is.null(out_dir))
        out_dir <- file.path(dirname(res_file), "validation")
    if (!dir.exists(out_dir)){
        message(sprintf("The output directory is missing. Creating one at %s", out_dir))
        dir.create(out_dir)
    }

    pyear <- res_file %>% basename() %>%
        stringr::str_extract_all(pattern = "_[0-9]{4}_") %>% unlist() %>%
        dplyr::last() %>% stringr::str_sub(2, -2) %>%
        ensurer::ensure_that(nchar(.) == 4, err_desc = "Invalid PRODES year")
    scene_file <- res_file %>% basename() %>%
        stringr::str_extract(pattern = "_[0-9]{6}_") %>%
        stringr::str_sub(2, -2) %>%
        ensurer::ensure_that(nchar(.) == 6, err_desc = "Invalid scene identifier")
    cov_res <- raster::raster(res_file)

    # rasterize PRODES
    cov_ref <- prodes2raster(file_pd = prodes_maps[scene_file],
                  file_rt = slot(cov_res@file, "name"),
                  raster_path = tempfile(pattern = "prodes2raster_",
                                         fileext = ".tif"),
                  tile = scene_file,
                  year_pd = pyear,
                  prodes_lbl = dplyr::filter(prodes_labels,
                                             label_pd %in% names(int_labels))) %>%
        raster::raster() %>%
        ensurer::ensure_that(!is.null(.), err_desc = "Rasterization failed!")

    # masking and storing results
    cov_res_masked <- raster::mask(cov_res, mask = cov_ref)
    water_mask <- water_masks[scene_file] %>% raster::raster()  %>%
        cov_proj(proj4string = raster::crs(cov_res_masked, asText = TRUE)) %>%
        raster::resample(y = cov_res_masked, method = "ngb") %>%
        raster::crop(y = raster::extent(cov_res_masked))
    cov_res_masked <- raster::mask(cov_res_masked, mask = water_mask, maskvalue = 1)
    corner_mask <- corner_masks %>% dplyr::filter(scene == scene_file) %>%
        ensurer::ensure_that(nrow(.) == 1, err_desc = "dplyr::filter failed") %>%
        dplyr::pull(file_path) %>%
        cov_read() %>%
        cov_proj(proj4string = raster::crs(cov_res_masked, asText = TRUE)) %>%
        raster::resample(y = cov_res_masked, method = "ngb") %>%
        raster::crop(y = raster::extent(cov_res_masked))
# NOTE: the masks are failing returning
    fuck_u_R <- raster::mask(cov_res_masked, mask = corner_mask)
    if (!is.na(mean(fuck_u_R[], na.rm = TRUE)))
       cov_res_masked <- fuck_u_R 
    #cov_res_masked <- raster::mask(cov_res_masked, mask = corner_mask) %>%
    #    ensurer::ensure_that(is.nan(mean(.[], na.rm = TRUE) ==  FALSE),
    #                         err_desc = "Masking failed, probably because R is a motherfucking piece of shit!")
    raster::writeRaster(cov_res_masked,
                        filename = file.path(out_dir, stringr::str_replace(basename(res_file), ".tif", "_masked.tif")),
                        overwrite=TRUE)

    # build reference-result data.frame
    ref_res_df <- raster::stack(cov_ref, cov_res_masked, quick = FALSE)[] %>%
        as.data.frame() %>% tidyr::drop_na() %>%
        dplyr::rename("lab_ref_num" = !!names(.[1]), "lab_res_num" = !!names(.[2])) %>%
        dplyr::mutate(lab_ref = dplyr::recode(lab_ref_num, !!!key_labels),
                      lab_res = dplyr::recode(lab_res_num, !!!key_labels)) %>%
        dplyr::filter(lab_res %in% unlist(prodes_labels_ls))                           # filter out classes not available in PRODES

    # computing and storing reference-result differences
    cov_diff <- cov_ref - cov_res_masked
    raster::writeRaster(cov_diff,
                        filename = file.path(out_dir, stringr::str_replace(basename(res_file), ".tif", "_masked_refdiff.tif")),
                        overwrite = TRUE)

    # computing confusion matrix
# Error in confusionMatrix.default(data = factor(ref_res_df[["lab_res"]],  :
#  The data must contain some levels that overlap the reference.

    flevels <- ref_res_df %>% dplyr::select(lab_ref, lab_res) %>%
        unlist() %>% unique()
    con_mat <- caret::confusionMatrix(data = factor(ref_res_df[["lab_res"]], flevels),
                                      reference = factor(ref_res_df[["lab_ref"]], flevels))

    # computing reference areas
    cov_areas <- cov_get_areas(cov_ref, label = lab_ref) %>%
        dplyr::mutate(lab_ref = dplyr::recode(label, !!!key_labels)) %>%
        tidyr::drop_na()

    # asses accuracy
    class_areas <- as.vector(cov_areas$area)
    names(class_areas) <- cov_areas$lab_ref

    return(asses_accuracy_area(error_matrix = as.matrix(as.data.frame.matrix(con_mat$table)),
                          class_areas))
}, out_dir = out_dir)
names(res_acc) <- path_res_vec

# storing accuracy results
res_file <- file.path(out_dir, "accuracy.rds")
saveRDS(res_acc, file = res_file)
print(sprintf("Saving results to %s", res_file))
print("Finished!")

