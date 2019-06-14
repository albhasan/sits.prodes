library(tibble)
library(devtools)

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"

brick_description <- tibble::tribble(
    ~Classification_type, ~Obs_year, ~Description,
    #--------------------|----------|--------------------|
    "interpolated",       23,        "The samples were taken from the bricks of Landsat 8 where the clouds were filled up by interpolation of MXD13 data. The vegetation indexes were recalculated from the bricks of the corresponding bands.",
    "starfm",             23,        "The samples were taken from the bricks of Landsat 8 where the clouds were filled up by the StarFM mixture model (Landsat 8 - MOD13Q1 mixture model).",
    "simple",             4,         "The samples were taken from the bricks of the best 4 Landsat 8 images of a year.",
    "mask_clouds",        4,         "The samples were taken from the bricks of the best 4 Landsat 8 images of a year and the clouds were masked as 'no_data'."
)

setwd(file.path(base_path, "Rpackage", "sits.prodes"))
usethis::use_data(brick_description, overwrite = TRUE)

