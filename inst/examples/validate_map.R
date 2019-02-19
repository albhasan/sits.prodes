# validate the result of a classification
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ensurer))
suppressPackageStartupMessages(library(optparse))
#suppressMessages(suppressPackageStartupMessages(library(sits)))

# TODO: remove
setwd("/home/alber/Documents/data/experiments/prodes_reproduction/Rpackage/sits.prodes")
library(devtools)
devtools::load_all()
# - - -

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"
stopifnot(dir.exists(base_path))

# get arguments ----
option_list = list(
  make_option("--experiment", type = "character", default = NULL,    help = "Name of an experiment e.g. 'rep_prodes_40'", metavar="character"),
  make_option("--algorithm",  type = "character", default = NULL,    help = "Name of an algorithm e.g. 'dl'", metavar="character"),
  make_option("--smooth_dir", type = "character", default = NULL,    help = "Name of a smooth directory e.g. 'smooth_3x3_n10'", metavar="character")
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)
if (length(opt) != 4 || sum(sapply(opt, is.null)) != 0){
  print_help(opt_parser)
  stop("Wrong arguments!")
}
experiment <- opt$experiment # "rep_prodes_41"
algorithm <-  opt$algorithm  # "svm"
smooth_dir <- opt$smooth_dir # "smooth_7x7_n10"

stopifnot(experiment %in% paste0("rep_prodes_", 40:99))
stopifnot(algorithm %in% c("dl", "svm", "rf"))
stopifnot(smooth_dir %in% paste0("smooth_", c("3x3", "5x5", "7x7"), "_n10"))

img_path <- c(
    rep_prodes_40 = file.path(base_path, "03_classify", experiment, "results"),
    rep_prodes_41 = file.path(base_path, "03_classify", experiment, "results"),
    rep_prodes_42 = file.path(base_path, "03_classify", experiment, "results"),
    rep_prodes_50 = file.path(base_path, "03_classify", experiment, "results"),
    rep_prodes_51 = file.path(base_path, "03_classify", experiment, "results"),
    rep_prodes_52 = file.path(base_path, "03_classify", experiment, "results")
)

img_pattern <- "^l8_(simple|maskcloud)_[0-9]{6}_[0-9]{4}_(dl|rf|svm)_[0-9]{4}_[0-9]_[0-9]{4}_[0-9].tif" 

# key for encoding PRODES's SHP into a TIF
prodes_labels <- list(
    DESMATAMENTO  = "deforestation",
    RESIDUO       = "deforestation",
    FLORESTA      = "forest",
    NAO_FLORESTA  = "no forest",
    NAO_FLORESTA2 = "no forest",
    HIDROGRAFIA   = "water"
)
water_masks <- c(
    "225063" = file.path(base_path, "data/raster/mascaras/surface_water-Pekel_et_al_2016/tiled/extent_225063.tif"),
    "226064" = file.path(base_path, "data/raster/mascaras/surface_water-Pekel_et_al_2016/tiled/extent_226064.tif"),
    "232066" = file.path(base_path, "data/raster/mascaras/surface_water-Pekel_et_al_2016/tiled/extent_232066.tif"),
    "233067" = file.path(base_path, "data/raster/mascaras/surface_water-Pekel_et_al_2016/tiled/extent_233067.tif")
)
prodes_maps <- c(
    "225063" = file.path(base_path, "data/vector/prodes_tiled/prodes_225_063.shp"),
    "226064" = file.path(base_path, "data/vector/prodes_tiled/prodes_226_064.shp"),
    "232066" = file.path(base_path, "data/vector/prodes_tiled/prodes_232_066.shp"),
    "233067" = file.path(base_path, "data/vector/prodes_tiled/prodes_233_067.shp")
)

# get classification labels
labels_csv <- file.path(paste0(img_path[experiment], '_dl'), "int_labels.csv") %>%
    ensurer::ensure_that(file.exists, err_desc = "Missing label file!") %>%
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

path_res_vec <- img_path[experiment] %>% paste0('_', algorithm) %>% 
    file.path(smooth_dir) %>%
    list.files(pattern = img_pattern, full.names = TRUE, 
               include.dirs = FALSE) %>%
    ensurer::ensure_that(length(.) > 0, err_desc = sprintf("No classified images found for %s %s", experiment, smooth_dir))

# loop each file resulting from classification or smoothing
out_dir <- file.path(dirname(path_res_vec[1]), "validation")
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
    scene <- res_file %>% basename() %>%
        stringr::str_extract(pattern = "_[0-9]{6}_") %>%
        stringr::str_sub(2, -2) %>%
        ensurer::ensure_that(nchar(.) == 6, err_desc = "Invalid scene identifier")
    cov_res <- cov_read(res_file)

    # rasterize PRODES
    cov_ref <- prodes_rasterize(ref_path = prodes_maps[scene], 
                                pyear = pyear, cov_res = cov_res,
                                level_key_pt = prodes_labels,
                                level_key = key_labels_rev) %>%
        ensurer::ensure_that(!is.null(.), err_desc = "Rasterization failed!")

    # masking and storing results
    cov_res_masked <- raster::mask(cov_res, mask = cov_ref)
    water_mask <- water_masks[scene] %>% cov_read()  %>%
        cov_proj(proj4string = raster::crs(cov_res_masked, asText = TRUE)) %>%
        raster::resample(y = cov_res_masked, method = "ngb") %>%
        raster::crop(y = raster::extent(cov_res_masked))
    cov_res_masked <- raster::mask(cov_res_masked, mask = water_mask, maskvalue = 1)
    raster::writeRaster(cov_res_masked,
                        filename = file.path(out_dir, stringr::str_replace(basename(res_file), ".tif", "_masked.tif")),
                        overwrite=TRUE)

    # build reference-result data.frame
    ref_res_df <- raster::stack(cov_ref, cov_res_masked, quick = FALSE)[] %>%
        as.data.frame() %>% tidyr::drop_na() %>%
        dplyr::rename("lab_ref_num" = !!names(.[1]), "lab_res_num" = !!names(.[2])) %>%
        dplyr::mutate(lab_ref = dplyr::recode(lab_ref_num, !!!key_labels),
                      lab_res = dplyr::recode(lab_res_num, !!!key_labels)) %>%
        dplyr::filter(lab_res %in% unlist(prodes_labels))                           # filter out classes not available in PRODES

    # computing and storing reference-result differences
    cov_diff <- cov_ref - cov_res_masked
    raster::writeRaster(cov_diff,
                        filename = file.path(out_dir, stringr::str_replace(basename(res_file), ".tif", "_masked_refdiff.tif")),
                        overwrite = TRUE)

    # computing confusion matrix
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

    return(asses_accuracy(error_matrix = as.matrix(as.data.frame.matrix(con_mat$table)),
                          class_areas))
}, out_dir = out_dir)
names(res_acc) <- path_res_vec

# storing accuracy results
res_file <- file.path(out_dir, "accuracy.rds")
saveRDS(res_acc, file = res_file)
print(sprintf("Saving results to %s", res_file))
print("Finished!")
