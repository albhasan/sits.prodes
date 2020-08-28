#!/usr/bin/env Rscript

# VALIDATE THE RESULT OF A CLASSIFICATION USING prodes
# Version 4

#--- Get arguments ----

suppressMessages(library(optparse))
option_list <- list(
  make_option("--in_dir", type = "character", default = NULL,
              help = "Path to a directory with classification results.",
              metavar = "character"),
  make_option("--label_file", type = "character", default = NULL,
              help = "Path to a csv file detailing the labels in the images.",
              metavar = "character"),
  make_option("--out_dir", type = "character", default = NULL,
              help = "Path to a directory where to store the validation results.",
              metavar = "character")
)

arguments <- parse_args(OptionParser(option_list = option_list))
if (!dir.exists(arguments$in_dir))
  stop(sprintf("Directory not found: %s", arguments$in_dir))
if (!file.exists(arguments$label_file))
  stop(sprintf("File not found: %s", arguments$label_file))
if (!dir.exists(arguments$out_dir)) {
  warning(sprintf("Output directory not found. Attempting to create it: %s", arguments$out_dir))
  dir.create(arguments$out_dir)
}

in_dir     <- arguments$in_dir
label_file <- arguments$label_file
out_dir    <- arguments$out_dir
rm(option_list, arguments)
detach(package:optparse)

#------------------------------------------------------------------------------
# TODO: remove
library(devtools)
devtools::load_all()
in_dir     <- "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_40/results_dl"
label_file <- "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_40/results_dl/int_labels.csv"
out_dir    <- "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_40/results_dl/validation_v4"
#------------------------------------------------------------------------------

# script ----
suppressMessages(library(dplyr))
suppressMessages(library(ensurer))
suppressMessages(library(gdalcmdline))
suppressMessages(library(sits.prodes))

print(sprintf("Processing %s using the labels at %s ", in_dir, label_file))

#---- functions ----

# Apply masks to the classification results.
mask_helper <- function(file_path_res, prodes_mask, water_mask, deter_mask,
                        out_file){

    stopifnot(all(vapply(c(file_path_res, prodes_mask, water_mask),
                         file.exists, logical(1))))

    file_path_res %>%
        mask_with(mask_file = water_mask) %>%
        (function(x){
             if (is.na(deter_mask))
                 return(x)
             x %>%
                 mask_with(mask_file = deter_mask) %>%
                 return()
        })%>%
        mask_with(mask_file = prodes_mask, out_file = out_file) %>%
        return()
}

#--- Script -----

# PRODES tibble of labels and the key-value for reference and for the classified map.
data(prodes_labels, package = "sits.prodes")
prodes_label_key = prodes_labels %>%
    dplyr::left_join(sits.prodes::get_prodes_map_key(label_file), by = c("id_pd" = "key_ref")) %>%
    dplyr::select(label_pd_pt, label_pd, id_pd)

# Get data.
prodes_maps <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/vector/prodes/tiled" %>%
    get_prodes_maps() %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "PRODES shapefiles not found!")
deter_maps <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/vector/deter/tiled" %>%
    get_deter_maps() %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "PRODES shapefiles not found!")
water_masks <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/mascaras/surface_water-Pekel_et_al_2016/tiled" %>%
    get_water_masks() %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "Water masks not found!")
#corner_masks <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/mask_l8_corner" %>%
#    get_corner_masks() %>%
#    ensurer::ensure_that(nrow(.) > 0, err_desc = "Corner masks not found!")

# Pattern of the classified images.
validation_tb <- in_dir %>%
    get_classified_maps() %>%
    dplyr::left_join(prodes_maps, by = "scene", suffix = c("", "_prodes")) %>%
    dplyr::left_join(water_masks, by = "scene", suffix = c("", "_water")) %>%
    #dplyr::left_join(corner_masks, by = "scene", suffix = c("", "_corner")) %>%
    #dplyr::left_join(quantile_maps, by = c("scene", "pyear"),
    #                 suffix = c("", "_quantile")) %>%
    dplyr::left_join(deter_maps, by = "scene", suffix = c("", "_deter"))

  # Rasterize PRODES to match the result map.
#-----------------------------------------
res <- validation_tb %>%
    dplyr::filter(pyear %in% c("2015", "2016", "2017")) %>%
    dplyr::filter(scene == "225063") %>%
    dplyr::slice(1:3) %>%
    dplyr::slice(3) %>%
#-----------------------------------------
    # Make names for the results.
    dplyr::mutate(res_masked = file.path(out_dir,
                                         stringr::str_replace(basename(file_path_res),
                                                              ".tif",
                                                              "_masked.tif"))) %>%
    # Make PRODES match the classification results
    dplyr::mutate(prodes_rasterized = purrr::pmap_chr(
                      dplyr::select(., file_path_prodes, file_path_res, pyear),
                      prodes2raster_helper,
                      prodes_label_key = prodes_label_key)) %>%
    # Convert PRODES map to a mask
    dplyr::mutate(prodes_mask = purrr::map_chr(prodes_rasterized,
                                                raster2mask)) %>%
    # Convert DETER to a mask.
    dplyr::mutate(deter_mask = purrr::pmap_chr(
                      dplyr::select(., file_path_deter, file_path_res, pyear),
                      deter2mask)) %>%
    # Invert the WATER mask.
    dplyr::mutate(water_mask = purrr::map_chr(file_path_water, invert_mask)) %>%
    # Apply the masks to the results.
    dplyr::mutate(res_masked = purrr::pmap_chr(
                      dplyr::select(., file_path_res, prodes_mask, water_mask, deter_mask),
                      mask_helper, out_file = res_masked))


