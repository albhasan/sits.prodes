#  Test Interpolated bricks

library(tidyverse)

# Use gdallocationinfo to get a time series out of a brick
get_ts <- function(brick_path, pix_x, pix_y){
    cmd <- paste("gdallocationinfo", brick_path, pix_x, pix_y)
    system(cmd, intern = TRUE) %>%
        .[gtools::even(seq_along(.))] %>% .[2:length(.)] %>% strsplit(" ") %>%
        lapply(dplyr::last) %>% unlist() %>% as.numeric() %>% return()
}

brick_interp <- c(
    red = "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/bricks/LC8SR-MOD13Q1-MYD13Q1_225063_2013-08-29_red_STACK_BRICK.tif",
    nir = "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/bricks/LC8SR-MOD13Q1-MYD13Q1_225063_2013-08-29_nir_STACK_BRICK.tif",
    ndvi = "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/bricks/LC8SR-MOD13Q1-MYD13Q1_225063_2013-08-29_ndvi_STACK_BRICK.tif"
)

brick_ts <- lapply(brick_interp, get_ts, pix_x = 4000, pix_y = 4000) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
        ndvi_Rcomputed = (nir - red)/(nir + red) * 10000,
        ndvi_diff = ndvi - ndvi_Rcomputed
    ) %>%

    as.data.frame()
brick_ts
