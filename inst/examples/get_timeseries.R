#!/usr/bin/Rscript
# GET TIME SERIES FOR SAMPLE POINTS

library(tidyverse)
library(sits)
library(parallel)
library(devtools)
library(config)
setwd("/home/alber/Documents/data/experiments/prodes_reproduction/Rpackage/sits.prodes")
devtools::load_all()

cores <- floor(parallel::detectCores() * 3/4)

# get sample files
csv_files <- list.files('/home/alber/Documents/data/experiments/prodes_reproduction/data/samples',
                        pattern = 'validated_prodes_[0-9]{3}_[0-9]{3}_[0-9]{4}-[0-9]{2}-[0-9]{2}.csv',
                        full.names = TRUE)
err_msg <- validate_csv(csv_files)
if (length(err_msg) > 0) {
    print(err_msg)
    stop("ERROR in CSV files!")
}

# configuration ----
# brick_type = "oldinterpolation"
# brick_type = "interpolated"
# brick_type = "starfm"
# brick_type = "interpolated_few_clouds"
# brick_type = "interpolated_simple"
# brick_type = "starfm_few_clouds"
brick_type = "simple"
# class_bands <- c("blue", "ndvi", "nir", "red", "savi", "swir2")
class_bands <- c("ndvi", "nir", "red", "swir2", "dark", "substrate", "vegetation")

if (brick_type == "starfm") {
    path_bricks  <- "/home/alber/shared/brick"
    brick_prefix <- "LC8SR-MOD13Q1-STARFM_"
    fpaths <- parallel::mclapply(csv_files, get_timeseries,
                             path_bricks = path_bricks,
                             brick_prefix = brick_prefix,
                             class_bands = class_bands,
                             suffix = paste0("_", brick_type),
                             mc.cores = cores)
}else if (brick_type == "starfm_few_clouds") {
    path_bricks  <- "/home/alber/shared/brick_few_clouds"
    brick_prefix <- "LC8SR-MOD13Q1-STARFM_"
    fpaths <- parallel::mclapply(csv_files, get_timeseries,
                             path_bricks = path_bricks,
                             brick_prefix = brick_prefix,
                             class_bands = class_bands,
                             cov_name = brick_type,
                             time_len = 4,
                             time_by = 92, # TODO: this value is wrong
                             suffix = paste0("_", brick_type),
                             mc.cores = cores)
}else if (brick_type == "interpolated") {
    # Bricks created by interpolating bands, then computing vegetation indexes
    path_bricks <- "/home/alber/shared/brick_interp"
    brick_prefix <- "LC8SR-MOD13Q1-MYD13Q1_"
    fpaths <- parallel::mclapply(csv_files, get_timeseries,
                             path_bricks = path_bricks,
                             brick_prefix = brick_prefix,
                             class_bands = class_bands,
                             suffix = paste0("_", brick_type),
                             mc.cores = cores)
}else if (brick_type == "interpolated_few_clouds") {
    path_bricks <- "/home/alber/shared/brick_interp_few_clouds"
    brick_prefix <- "LC8SR-MOD13Q1-MYD13Q1_"
    fpaths <- parallel::mclapply(csv_files, get_timeseries,
                             path_bricks = path_bricks,
                             brick_prefix = brick_prefix,
                             class_bands = class_bands,
                             cov_name = brick_type,
                             time_len = 4,
                             time_by = 92, # TODO: this value is wrong
                             suffix = paste0("_", brick_type),
                             mc.cores = cores)
}else if (brick_type == "oldinterpolation") {
    # Bricks created by interpolating bands AND interpolating vegetation indexes
    path_bricks <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/bricks"
    brick_prefix <- "LC8SR-MOD13Q1-MYD13Q1_"
    fpaths <- parallel::mclapply(csv_files, get_timeseries,
                             path_bricks = path_bricks,
                             brick_prefix = brick_prefix,
                             class_bands = class_bands,
                             suffix = paste0("_", brick_type),
                             mc.cores = cores)
}else if (brick_type == "simple") {
    # Bricks created by piling up  bands, olating vegetation indexes, and spectral mistures
    path_bricks <- "/home/alber/shared/brick_simple"
    brick_prefix <- "LC8SR-SIMPLE_"

    band_names <- c("ndvi", "evi", "red", "nir", "mir", "swir1", "swir2", "class", "dark", "substrate", "vegetation")
    scale_factor   <- as.list(rep(1/10000, length(band_names)))
    maximum_values <- as.list(rep(10000,   length(band_names)))
    minimum_values <- as.list(c(-3000, -3000, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    missing_values <- rep(-3000, length(band_names)) # TODO: validate each time !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    names(maximum_values) <- names(minimum_values) <- names(missing_values) <- names(scale_factor) <- band_names

    fpaths <- parallel::mclapply(csv_files, get_timeseries,
                             path_bricks = path_bricks,
                             brick_prefix = brick_prefix,
                             class_bands = class_bands,
                             scale_factor = scale_factor,
                             missing_values  = missing_values,
                             minimum_values  = minimum_values,
                             maximum_values  = maximum_values,
                             suffix = paste0("_", brick_type),
                             cov_name = "Brick simple",
                             time_len = 4,
                             time_by = ceiling(365/4),
                             mc.cores = cores)
}else{
    stop("Unknown kind ofd brick")
}

print("Files created (samples & time series): ")
print(fpaths)
print("Done!")
