#!/usr/bin/Rscript

# Merge the masks of the corners of the classifications.

library(dplyr)
library(raster)

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"
stopifnot(dir.exists(base_path))

corner_masks <- base_path %>% file.path("data", "raster", "mask_l8_corner") %>% 
    list.files(pattern = "LC08_CORNERMASK_[0-9]{6}_[0-9]{4}.tif", full.names = TRUE) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(file_path = "value") %>%
    dplyr::mutate(fname = basename(file_path), 
                  scene   = stringr::str_extract(fname, pattern = "_[0-9]{6}_")   %>% stringr::str_sub(2, -2),
                  year_pd = stringr::str_extract(fname, pattern = "_[0-9]{4}[.]") %>% stringr::str_sub(2, -2))

scenes <- corner_masks %>% dplyr::pull(scene) %>% unique() %>% sort()

for(sc in scenes){
    print(sprintf("Processing sc %s...", sc))
    file_paths <- corner_masks %>% dplyr::filter(scene == sc) %>%
       dplyr::pull(file_path)
    r <- file_paths[1] %>% raster::raster()
    for(fpath in file_paths[-1]){
        print(sprintf("Processing file %s...", basename(fpath)))
        s <- raster::raster(fpath) 
        # match rasters
        if (!raster::compareRaster(r, s, extent = TRUE, rowcol = TRUE, crs = TRUE,
                                  stopiffalse = FALSE, showwarning = TRUE)) {
            tmp_fn <- fpath %>% basename() %>% tools::file_path_sans_ext() %>%
                paste0('_') %>% tempfile(fileext = ".tif")
            s <- raster::projectRaster(from = s, to = r, method = "ngb",
                                        filename = tmp_fn)
        }
        r <- r %>% raster::mask(mask = s)
    }
    raster::writeRaster(r, filename = file.path(dirname(fpath),
                        paste0("LC08_CORNERMASK_", sc, ".tif")),
                        overwrite = TRUE)
}

print("Done!")

