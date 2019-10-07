#!/usr/bin/Rscript

# GET TIME SERIES FOR SAMPLE POINTS

library(config)
library(dplyr)
library(sits)
library(sits.prodes)
library(parallel)

#------
# NOTE: Remove
library(devtools)
devtools::load_all()
#------

base_path = "/home/alber/Documents/data/experiments/prodes_reproduction"
cores <- floor(parallel::detectCores() * 3/4)
no_data <- -9999 # Landsat no data value

# get sample files
csv_files <- base_path %>%
    file.path("data", "samples") %>%
    list.files(pattern = 'validated_prodes_[0-9]{3}_[0-9]{3}_[0-9]{4}-[0-9]{2}-[0-9]{2}.csv',
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
# brick_type = "simple"
# brick_type = "mask_cloud"
# brick_type = "raw"
brick_type = "hls_raw"

class_bands <- c("ndvi", "nir", "red", "blue", "green", "swir1", "swir2",
                  "dark", "substrate", "vegetation")

print(sprintf("Brick type: %s", brick_type))

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
    # Bricks created by selecting the images with fewer clouds and piling up
    # bands , vegetation indexes, and spectral mixstures
    path_bricks <- "/home/alber/shared/brick_simple"
    brick_prefix <- "LC8SR-SIMPLE_"
    band_names <- c("ndvi", "evi", "red", "nir", "mir", "swir1", "swir2",
                    "class", "dark", "substrate", "vegetation")
    scale_factor   <- as.list(rep(1/10000, length(band_names)))
    maximum_values <- as.list(rep(10000,   length(band_names)))
    minimum_values <- as.list(rep(0,       length(band_names)))
    missing_values <- rep(no_data, length(band_names))
    names(maximum_values) <- names(minimum_values) <- names(missing_values) <- names(scale_factor) <- band_names
    #maximum_values$substrate <- maximum_values$vegetation <- 50000

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
}else if (brick_type == "mask_cloud") {
    # Bricks created by masking clouds and piling up bands, vegetation indexes, and spectral mixtures
    path_bricks <- "/home/alber/shared/brick_maskcloud"
    brick_prefix <- "LC8SR-MASKCLOUD_"

    band_names <- c("ndvi", "evi", "blue", "green", "red", "nir", "mir", "swir1", "swir2", "class", "dark", "substrate", "vegetation")
    scale_factor   <- as.list(rep(1/10000, length(band_names)))
    maximum_values <- as.list(rep(10000,   length(band_names)))
    minimum_values <- as.list(rep(0,       length(band_names)))
    missing_values <- rep(no_data, length(band_names))
    names(maximum_values) <- names(minimum_values) <- names(missing_values) <- names(scale_factor) <- band_names
    #maximum_values$substrate <- maximum_values$vegetation <- 50000

    fpaths <- parallel::mclapply(csv_files, get_timeseries,
                             path_bricks = path_bricks,
                             brick_prefix = brick_prefix,
                             class_bands = class_bands,
                             scale_factor = scale_factor,
                             missing_values  = missing_values,
                             minimum_values  = minimum_values,
                             maximum_values  = maximum_values,
                             suffix = paste0("_", brick_type),
                             cov_name = "Brick masked clouds",
                             time_len = 4,
                             time_by = ceiling(365/4),
                             mc.cores = cores)
}else if (brick_type == "raw") {
    # Bricks created by piling up raw images
    path_bricks <- "/home/alber/shared/brick_raw"
    brick_prefix <- "LC8SR-RAW_"

    old_band_names <- c("ndvi", "evi", "blue", "green", "red", "nir", "mir", "swir1", "swir2", "dark", "substrate", "vegetation", "cloud")
    band_names <- c("blue", "cloud", "dark", "ndvi", "nir", "red", "substrate", "swir2", "vegetation")
    scale_factor   <- as.list(rep(1/10000, length(band_names)))
    maximum_values <- as.list(rep(10000,   length(band_names)))
    minimum_values <- as.list(rep(0,       length(band_names)))
    missing_values <- rep(no_data, length(band_names))
    names(maximum_values) <- names(minimum_values) <- names(missing_values) <- names(scale_factor) <- band_names

    scale_factor$cloud   <- 1
    maximum_values$cloud <- 1
    fpaths <- parallel::mclapply(csv_files, sits.prodes:::get_timeseries,
                             path_bricks = path_bricks,
                             brick_prefix = brick_prefix,
                             class_bands = band_names,
                             scale_factor = scale_factor,
                             missing_values  = missing_values,
                             minimum_values  = minimum_values,
                             maximum_values  = maximum_values,
                             suffix = paste0("_", brick_type),
                             cov_name = "Brick raw",
                             time_len = 23,
                             time_by = ceiling(365/23),
                             mc.cores = cores)
    print(fpaths)
}else if(brick_type == "hls_raw") {
    # Bricks created by piling up raw HLS images[V
    band_names <- c("band01", "band02", "band03", "band04", "band05", 
                     "band06", "band07", "band09", "band10", "band11", 
                     "cloud", "evi2", "msavi", "nbr2", "nbr", "ndmi", 
                     "ndvi","savi" )
    path_bricks <- "/home/alber/shared/brick_hls_raw"
    brick_prefix <- "HLSL30-RAW_"
    scale_factor   <- as.list(rep(1/10000, length(band_names)))
    maximum_values <- as.list(rep(10000,   length(band_names)))
    minimum_values <- as.list(rep(0,       length(band_names)))
    missing_values <- rep(no_data, length(band_names))
    names(maximum_values) <- names(minimum_values) <- names(missing_values) <- names(scale_factor) <- band_names
    scale_factor$cloud   <- 1
    maximum_values$cloud <- 1
    # NOTE: 
    #    - NDVI's minimum value is 0. Shouldn't it be -10000?
    #    - missing_values == no_data?
    samples_csv <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/samples/hls" %>%
        list.files(pattern = "shp$", full.names = TRUE) %>%
        purrr::map(process_valid_shp, 
		   out_dir = "/home/alber/Documents/data/experiments/prodes_reproduction/data/samples/hls/csv") %>%
        unlist()
    fpaths <- parallel::mclapply(samples_csv, sits.prodes:::get_timeseries,
                                 path_bricks = path_bricks,
                                 brick_prefix = brick_prefix,
                                 class_bands = band_names,
                                 scale_factor = scale_factor,
                                 missing_values  = missing_values,
                                 minimum_values  = minimum_values,
                                 maximum_values  = maximum_values,
                                 suffix = paste0("_", brick_type),
                                 cov_name = "Brick HLS raw",
                                 time_len = 23,
                                 time_by = ceiling(365/23),
                                 mc.cores = cores)

#----
# test
#Error in sits.prodes:::get_timeseries(samples_csv[[1]], path_bricks = path_bricks,  :
#  Invalid start_date in file name /tmp/Rtmpt9umzk/prodes_T19LGK_p2017_2016-08-01.csv
#res <- sits.prodes:::get_timeseries(samples_csv[[1]], 
#                                    path_bricks = path_bricks,
#                                    brick_prefix = brick_prefix,
#                                    class_bands = band_names,
#                                    scale_factor = scale_factor,
#                                    missing_values  = missing_values,
#                                    minimum_values  = minimum_values,
#                                    maximum_values  = maximum_values,
#                                    suffix = paste0("_", brick_type),
#                                    cov_name = "Brick HLS raw",
#                                    time_len = 23,
#                                    time_by = ceiling(365/23))
#
#----


    print(fpaths)

}else{
    stop("Unknown kind of brick")
}

print("Files created (samples & time series): ")
print(fpaths)
print("Done!")

