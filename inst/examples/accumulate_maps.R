#!/usr/bin/Rscript
# Merge yearly forest-deforestation maps in a prodes-like way
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ensurer))
suppressPackageStartupMessages(library(optparse))
library(sits.starfm)
library(sits.prodes)

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"
stopifnot(dir.exists(base_path))

# get arguments ----
option_list = list(
  make_option("--in_dir",     type = "character", default = NULL, help = "Path to a directory with classification results.", metavar="character"),
  make_option("--label_file", type = "character", default = NULL, help = "Path to a csv file detailing the labels in the images.", metavar="character"),
  make_option("--out_dir",    type = "character", default = NULL, help = "Path to a directory where to store the results.", metavar="character")
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)
if (length(opt) != 4 || sum(sapply(opt, is.null)) != 0){
  print_help(opt_parser)
  stop("Wrong arguments!")
}
in_dir     <- opt$in_dir     # "/net/150.163.2.206/disks/d6/shared/alber/prodes_reproduction/03_classify/rep_prodes_40/results_vote/smooth_3x3_n10"
label_file <- opt$label_file # "/net/150.163.2.206/disks/d6/shared/alber/prodes_reproduction/03_classify/rep_prodes_40/results_dl/int_labels.csv"
out_dir    <- opt$out_dir    # "/net/150.163.2.206/disks/d6/shared/alber/prodes_reproduction/03_classify/rep_prodes_40/results_vote/smooth_3x3_n10/prodes_map"
if (!file.exists(label_file)) {
    print_help(opt_parse)
    stop("File not found!")
}
if (!all(vapply(c(in_dir), dir.exists, logical(1)))) {
    print_help(opt_parse)
    stop("Directory not found!")
}
img_pattern <- "^l8_(simple|maskcloud)_[0-9]{6}_[0-9]{4}_(dl|rf|svm|dl-rf-svm)_[0-9]{4}_[0-9]_[0-9]{4}_[0-9](_vote.tif|.tif)"
corner_mask_path <- "/home/alber/shared/mask_l8_corner"

# setup
param <- list(dstnodata = -9999, 
    out_format = "GTiff", 
    creation_option = NULL,
    fileext = ".tif",
    data_type = "Int16")

# get classification labels
labels_csv <- label_file %>%
    read.csv(stringsAsFactors = FALSE)
int_labels        <- labels_csv$Code
names(int_labels) <- labels_csv$Label
rm(labels_csv)
stopifnot("deforestation" %in% names(int_labels))
# when the classes are clustered
if(sum(stringr::str_detect(names(int_labels), "_[0-9]+$")) > 0){
    names(int_labels) <- names(int_labels) %>%
        stringr::str_replace_all(pattern = "_[0-9]+$", replacement = "")
}

# vector of paths to images
in_path <- in_dir %>% 
    list.files(pattern = img_pattern, full.names = TRUE,
               include.dirs = FALSE) %>%
    ensurer::ensure_that(length(.) > 1, err_desc = sprintf("Not enough classified images found at %s", in_dir)) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(filepath = value)
img_tb <- in_path %>% dplyr::pull(filepath) %>% basename() %>%
    stringr::str_split(pattern = '_', simplify = TRUE) %>% as.data.frame(stringsAsFactors = FALSE) %>%
    dplyr::as_tibble() %>% dplyr::select(satellite = V1, brick = V2, scene = V3, 
        algorithm = V5, pyear = V8) %>%
    dplyr::bind_cols(in_path)
rm(in_path)

# accumulate deforestation in a PRODES-like fashion (e.g once a pixel is deforested, it never comes back to forest)
get_prodes_map <- function(img_path, out_file){
    scene <- img_path %>% get_scene() %>%
        ensurer::ensure_that(length(.) == 1, err_des = "All images should belong to the same scene!")
    # build a VRT
    calc_exp <- sprintf("numpy.where(numpy.logical_or(A == %s, B == %s), %s, %s)",
        int_labels["deforestation"], int_labels["deforestation"],
        int_labels["deforestation"], int_labels["forest"])
    # accumulate deforestation
    for(i in seq_along(img_path)){
        if (i == 1) {
            last_img <- img_path[i]
            next()
        }
        vrt_file <- c(last_img, img_path[i]) %>%
            sits.starfm::gdalbuildvrt(separate = TRUE)
        last_img <- vrt_file %>% rep(times = 2) %>%
            sits.starfm::gdal_calc(expression = calc_exp, band_number = 1:2, 
                                   dstnodata = param$dstnodata,
                                   data_type = param$data_type)
    }

    # mask the results
    res <- corner_mask_path %>% file.path(paste0("LC08_CORNERMASK_", scene,"_2016.tif")) %>%
        c(last_img) %>%
        ensurer::ensure_that(all(file.exists(.)), err_des = "File not found!'") %>%
        sits.starfm::gdalbuildvrt(separate = TRUE) %>%
        rep(times = 2) %>%
        sits.starfm::gdal_calc(out_filename = out_file,
                               expression = paste0("(numpy.where(A == 0, ", param$dstnodata, ", B)).astype(int16)"),
                               band_number = 1:2, dstnodata = param$dstnodata,
                               data_type = param$data_type)
    return(res)
}

for(sc in unique(img_tb$scene)) {
    prodes_year <- img_tb %>% dplyr::filter(scene == sc) %>%
        dplyr::pull(filepath) %>%
        basename() %>%
        stringr::str_extract_all(pattern = "_[0-9]{4}_") %>%
        unlist() %>% 
        stringr::str_sub(2, 5) %>%
        as.integer() %>%
        max()
    if (is.null(out_dir))
        out_dir <- img_tb %>% dplyr::filter(scene == sc) %>%
            dplyr::pull(filepath) %>%
            dplyr::first() %>%
            dirname() %>%
            file.path("prodes_reproduction")
    if(!dir.exists(out_dir))
        out_dir %>% dir.create()
    out_file <- file.path(out_dir, sprintf("prodes_reproduction_%s_%s.tif", sc, prodes_year))
    print(sprintf("Computing file %s ...", out_file))
    img_tb %>% dplyr::filter(scene == sc) %>%
        dplyr::pull(filepath) %>%
        get_prodes_map(out_file =out_file)
 
}

# TODO
print("Finished!")

