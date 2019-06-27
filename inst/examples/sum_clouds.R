#!/usr/bin/Rscript

suppressMessages(library(dplyr))
suppressMessages(library(sits.prodes))
suppressMessages(library(raster))

in_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_raw"
out_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/cloud_count"

cloud_brick <- in_dir %>% list.files(pattern = "LC8SR-RAW_[0-9]{6}_[0-9]{4}-[0-9]{2}-[0-9]{2}_cloud_STACK_BRICK.tif",
                                     full.names = TRUE) %>%
     sits.prodes::get_brick_md() %>%
     tibble::as_tibble()

for(rid in seq_along(cloud_brick$path)){
    print(sprintf("Processing CLOUD BRICK %s ...", cloud_brick$path[[rid]]))
    r <- raster::stack(cloud_brick$path[[rid]]) %>%
        sum(na.rm = TRUE)
    out_file <- file.path(out_dir,
                          paste0("l8_raw-cloud-sum", '_',
                                 cloud_brick$pathrow[[rid]], '_',
                                 cloud_brick$year[[rid]], ".tif"))
    print(sprintf("Saving to %s ...", out_file))
    raster::writeRaster(r, filename = out_file, 
                        datatype = "INT1S", 
                        options = c("COMPRESS=LZW", "TILED=YES"))
}
print("Done")

