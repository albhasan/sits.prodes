#  Test the bricks by computing the NDVI differences between the brick and computed in R

library(tidyverse)

# get a time series out of a brick
get_ts <- function(brick_path, pix_x, pix_y){
    cmd <- paste("gdallocationinfo", brick_path, pix_x, pix_y)
    system(cmd, intern = TRUE) %>%
        .[gtools::even(seq_along(.))] %>% .[2:length(.)] %>% strsplit(" ") %>%
        lapply(dplyr::last) %>% unlist() %>% as.numeric() %>% return()
}

# list some bricks
brick_ls <- list(
    brick_starfm = c(
        red  = "/home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_225063_2013-08-10_red_STACK_BRICK.tif",
        nir  = "/home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_225063_2013-08-10_nir_STACK_BRICK.tif",
        ndvi = "/home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_225063_2013-08-10_ndvi_STACK_BRICK.tif"
    ),
    brick_starfm_summer = c(
        red  = "/home/alber/shared/brick_few_clouds/LC8SR-MOD13Q1-STARFM_225063_2013-08-10_red_STACK_BRICK_fewclouds.tif",
        nir  = "/home/alber/shared/brick_few_clouds/LC8SR-MOD13Q1-STARFM_225063_2013-08-10_nir_STACK_BRICK_fewclouds.tif",
        ndvi = "/home/alber/shared/brick_few_clouds/LC8SR-MOD13Q1-STARFM_225063_2013-08-10_ndvi_STACK_BRICK_fewclouds.tif"
    ),
    brick_interp = c(
        red  = "/home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_225063_2013-08-29_red_STACK_BRICK.tif",
        nir  = "/home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_225063_2013-08-29_nir_STACK_BRICK.tif",
        ndvi = "/home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_225063_2013-08-29_ndvi_STACK_BRICK.tif"
    ),
    brick_interp_summer = c(
        red  = "/home/alber/shared/brick_interp_few_clouds/LC8SR-MOD13Q1-MYD13Q1_225063_2013-08-29_red_STACK_BRICK_fewclouds.tif",
        nir  = "/home/alber/shared/brick_interp_few_clouds/LC8SR-MOD13Q1-MYD13Q1_225063_2013-08-29_nir_STACK_BRICK_fewclouds.tif",
        ndvi = "/home/alber/shared/brick_interp_few_clouds/LC8SR-MOD13Q1-MYD13Q1_225063_2013-08-29_ndvi_STACK_BRICK_fewclouds.tif"
    ),
    brick_original = c(
        red = "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/bricks/LC8SR-MOD13Q1-MYD13Q1_225063_2013-08-29_red_STACK_BRICK.tif",
        nir = "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/bricks/LC8SR-MOD13Q1-MYD13Q1_225063_2013-08-29_nir_STACK_BRICK.tif",
        ndvi = "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/bricks/LC8SR-MOD13Q1-MYD13Q1_225063_2013-08-29_ndvi_STACK_BRICK.tif"
    )
)

# build a tibble of the differences between brick and R-computed NDVI
brick_ts <- lapply(brick_ls, function(brick_paths){
    lapply(brick_paths, get_ts, pix_x = 4000, pix_y = 4000) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(
            ndvi_Rcomputed = (nir - red)/(nir + red) * 10000,
            ndvi_diff = ndvi - ndvi_Rcomputed
        ) %>%
        return()
})

# print the results
lapply(brick_ts,  as.data.frame)
