#!/usr/bin/Rscript

# Merge yearly forest-deforestation maps in a prodes-like way
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ensurer))
suppressPackageStartupMessages(library(optparse))
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
img_pattern <- "^l8_(simple|maskcloud)_[0-9]{6}_[0-9]{4}_dl-rf-svm_[0-9]{4}_[0-9]_[0-9]{4}_[0-9]_vote.tif"

# setup
no_data <- -9999
param <- list(dstnodata = no_data, 
    out_format = "GTiff", 
    creation_option = NULL,
    fileext = ".tif")

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



# TODO
img_path <- img_tb %>% dplyr::filter(scene == "233067") %>% dplyr::pull(filepath)


get_prodes_map <- function(img_path){
    #param <- param %>% append(sits.starfm:::get_landsat_metadata(img_path[1]))
    A_img <- B_img <- NULL
    exp <- sprintf("numpy.where(numpy.logical_or(A == %s, B == %s), %s, %s)", int_labels["deforestation"], int_labels["deforestation"], int_labels["deforestation"], int_labels["forest"])
    for(i in seq_along(img_path)){
print(i)
        if (i == 1) {
            A_img <- img_path[i]
            next()
        }
        B_img <- img_path[i]
        A_img <- c(A_img, B_img) %>%
           sits.starfm::gdal_calc(
dry_run = TRUE,
               out_filename = file.path(tempdir(), stringr::str_replace(basename(A_img), ".tif", paste0('_', i, '.tif'))),
               expression = exp,
               dstnodata = param[["dstnodata"]],
               out_format = param[["out_format"]],
               creation_option = param[["creation_option"]])
    }
    return(A_img)
}

#====================================================
stop("fix")
    # build the cloud mask
    img_mask <- pixel_qa %>%
        gdal_calc(
            out_filename =
                file.path(tmp_dir, paste0(paste("cloud_mask", img$sat_image, 
                                       sep = "_"), param[["fileext"]])),
            expression = "((numpy.bitwise_and(A, 40) != 0) * 1).astype(int16)",
            dstnodata = param[["dstnodata"]],
            out_format = param[["out_format"]],
            creation_option = param[["creation_option"]])
    img %>% dplyr::pull(files) %>% dplyr::bind_rows() %>%









