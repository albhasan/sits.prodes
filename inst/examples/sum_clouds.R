#!/usr/bin/Rscript

suppressMessages(library(dplyr))
suppressMessages(library(sits.prodes))
suppressMessages(library(raster))
suppressMessages(library(sf))

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"

in_dir <- file.path(base_path, "data/raster/brick_raw")
out_dir <- file.path(base_path, "data/raster/cloud_count")
prodes_mask <-c(
        "225063" = file.path(base_path, "data/vector/prodes/prodes_tiled/hand_dissolved/PDigital2017_AMZ_pol_225_063.shp"),
        "226064" = file.path(base_path, "data/vector/prodes/prodes_tiled/hand_dissolved/PDigital2017_AMZ_pol_226_064.shp"),
        "233067" = file.path(base_path, "data/vector/prodes/prodes_tiled/hand_dissolved/PDigital2017_AMZ_pol_233_067.shp")
    )
stopifnot(vapply(c(in_dir, out_dir), dir.exists, logical(1)))
stopifnot(vapply(prodes_mask, file.exists, logical(1)))

mask_tb <- prodes_mask %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(file_path = "value") %>%
    dplyr::mutate(pathrow = names(prodes_mask),
                  prodes_mask = purrr::map(file_path, sf::st_read, quiet = TRUE))

cloud_brick <- in_dir %>% list.files(pattern = "LC8SR-RAW_[0-9]{6}_[0-9]{4}-[0-9]{2}-[0-9]{2}_cloud_STACK_BRICK.tif",
                                     full.names = TRUE) %>%
    sits.prodes::get_brick_md() %>%
    tibble::as_tibble() %>%
    ensurer::ensure_that(nrow(.) > 0) %>%
    dplyr::left_join(mask_tb, by = "pathrow")

for(rid in seq_along(cloud_brick$path)){
    print(sprintf("Processing CLOUD BRICK %s ...", cloud_brick$path[[rid]]))
    out_file <- file.path(out_dir,
                          paste0("l8_raw-cloud-sum", '_',
                                 cloud_brick$pathrow[[rid]], '_',
                                 cloud_brick$year[[rid]], ".tif"))
    print(sprintf("Saving to %s ...", out_file))
    r <- raster::stack(cloud_brick$path[[rid]]) %>%
        sum(na.rm = TRUE) %>%
        raster::mask(mask = sf::st_transform(cloud_brick$prodes_mask[[rid]], 
                                             crs = crs(.))) %>%
        raster::writeRaster(filename = out_file, 
                            datatype = "INT2S", 
                            options = c("COMPRESS=LZW", "TILED=YES"),
                            overwrite = TRUE)
    # Reclassify the matrix according to its quantiles.
    rclmat <- r[] %>%
        .[!is.na(.)] %>%
        .[which(. > 0)] %>%
        stats::quantile() %>%
        rep(each = 2) %>%
        .[-c(1, length(.))] %>%
        matrix(byrow = TRUE, ncol = 2) %>%
        cbind(1:length(.))
    r %>% raster::reclassify(rcl = rclmat) %>%
        raster::writeRaster(filename = stringr::str_replace(out_file, 
                                                            pattern = ".tif", 
                                                            replacement = "_quantiles.tif"),
                            datatype = "INT2S",
                            options = c("COMPRESS=LZW", "TILED=YES"),
                            overwrite = TRUE)
}
print("Done")

