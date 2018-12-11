# export sample-point shps to CSV for later use in sits::sits_get_data(./get_timeseries.R)

library(devtools)
devtools::load_all()

library(sf)
library(tidyverse)

val_shps <- list.files("/home/alber/Documents/data/experiments/prodes_reproduction/data/samples/shp",
                       pattern = 'validated_prodes_[0-9][0-9][0-9]_[0-9][0-9][0-9].shp',
                       full.names = TRUE)

csv_paths <- lapply(val_shps, process_valid_shp,
                    out_dir = "/home/alber/Documents/data/experiments/prodes_reproduction/data/samples")

csv_paths
