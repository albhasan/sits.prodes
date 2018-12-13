#!/usr/bin/Rscript

# GET TIME SERIES FOR THE SAMPLE POINTS

setwd("/home/alber/Documents/data/experiments/prodes_reproduction/Rpackage/sits.prodes")
library(devtools)
devtools::load_all()

#library(sits)
#library(parallel)

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
brick_type = "oldinterpolation"
# brick_type = "intepolated"
# brick_type = "starfm"
# class_bands <- c("blue", "ndvi", "nir", "red", "savi", "swir2")
class_bands <- c("ndvi", "nir", "red", "swir2")


if (brick_type == "starfm") {
    path_bricks  <- "/home/alber/shared/brick"
    brick_prefix <- "LC8SR-MOD13Q1-STARFM_"
}else if (brick_type == "intepolated") {
    # Bricks created by interpolating bands, then computing vegetation indexes
    path_bricks <- "/home/alber/shared/brick_interp"
    brick_prefix <- "LC8SR-MOD13Q1-MYD13Q1_"
}else if (brick_type == "oldinterpolation") {
    # Bricks created by interpolating bands AND interpolating vegetation indexes
    path_bricks <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/bricks"
    brick_prefix <- "LC8SR-MOD13Q1-MYD13Q1_"
}

fpaths <- parallel::mclapply(csv_files, get_timeseries,
                             path_bricks = path_bricks,
                             brick_prefix = brick_prefix,
                             class_bands = class_bands,
                             suffix = paste0("_", brick_type),
                             mc.cores = cores)

print("Files created (samples & time series): ")
print(fpaths)
print("Done!")
