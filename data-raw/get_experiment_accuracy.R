#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))

setwd("/home/alber/Documents/data/experiments/prodes_reproduction/Rpackage/sits.prodes")
base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"
stopifnot(dir.exists(base_path))

# collect accuracy data
validation_tb <- base_path %>% 
    list.files(pattern = "validation_tb.rds", recursive = TRUE, full.names = TRUE) %>%
    purrr::map_df(readRDS) %>%
    dplyr::mutate(experiment = stringr::str_extract(file_path_res, "rep_prodes_[0-9]+"), 
                  algorithm = purrr::map_chr(.$file_path_res, function(x){
                      unlist(stringr::str_split(stringr::str_extract(x, "results_[a-z]+"), '_'))[2]
                  }), 
                  smooth = stringr::str_extract(file_path_res, "smooth_[0-9]+x[0-9]+_n[0-9]{2}"), 
                  scene = stringr::str_extract(basename(file_path_res), "[0-9]{6}"),
                  #pyear = purrr::map_chr(file_path_res, function(x){
                  #    as.integer(dplyr::last(unlist(stringr::str_extract_all(basename(x), "[0-9]{4}"))))
                  #})
                  ) %>%
    dplyr::select(experiment, algorithm, smooth, scene, pyear, file_path_res, 
                  file_path_prodes, file_path_water, file_path_corner, 
                  file_path_quantile, prodes_rasterized, res_masked_prodes, 
                  res_masked_water, out_res, res_confusion, r_quantile, 
                  validation_data) 

fname <- file.path(getwd(), "inst", "extdata", "validation_tb.Rdata")
if (file.exists(fname)) {
    file.remove(fname)
}
print(sprintf("Saving new accuracy file to: %s", fname))
save(validation_tb, file = fname)
print("Finished!")

