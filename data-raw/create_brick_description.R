library(tibble)
library(devtools)

print("Creating a table of descriptions of each brick...")

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"

brick_description <- tibble::tribble(
    ~Classification_type, ~Obs_year, ~Description,
    #--------------------|----------|--------------------|
    "interpolated",       0,         "TODO",
    "starfm",             0,         "TODO",
    "simple",             0,         "TODO",
    "mask_clouds",        0,         "TODO"
)

setwd(file.path(base_path, "Rpackage", "sits.prodes"))
usethis::use_data(brick_description, overwrite = TRUE)

